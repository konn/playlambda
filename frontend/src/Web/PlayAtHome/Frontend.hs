{-# LANGUAGE DataKinds, FlexibleContexts, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE RecursiveDo, TypeApplications                              #-}
{-# OPTIONS_GHC -Wall #-}
module Web.PlayAtHome.Frontend where
import           Control.Arrow          ((>>>))
import           Control.Lens
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import qualified Data.Aeson             as Aeson
import           Data.ByteString.Lazy   (fromStrict, toStrict)
import           Data.List.NonEmpty     (nonEmpty)
import           Data.Map.Strict        (Map)
import           Data.Maybe
import           Data.Monoid            ((<>))
import qualified Data.Set               as Set
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import           Data.Time
import           Data.UUID              as UUID
import           Reflex
import           Reflex.Dom
import           System.Random
import           Text.URI               hiding (Password)

import Obelisk.Configs
import Obelisk.Frontend
import Obelisk.Generated.Static
import Obelisk.Route

import Control.Monad        (void)
import Web.PlayAtHome.Route
import Web.PlayAtHome.Types

app ::
  ( DomBuilder t m, MonadFix m,
    PostBuild t m, PerformEvent t m,
    Prerender js t m, HasConfigs m,
    Reflex t,
    MonadIO (Performable m),
    MonadHold t m
  ) => m ()
app = do
  r <- getConfig "common/route"
  el "h1" $ text "Pλay At Home"
  rec
    msgEvDyn <-
      widgetHold (fmap Left <$> loginWidget initEvts)
      (leftmost
        [ fmap (fmap Right) joinRoomWidget <$ loggedInEv
        , fmap (fmap Right) . roomWidget playEvEvs <$> joinedRoomEv
        ]
      )
    let msgSendEv = switch $ current msgEvDyn
        initEvts = fmapMaybe (Aeson.decode @InitEvent . fromStrict)
            $ switchDyn wsRespEv
        playEvEvs =
          fmapMaybe (Aeson.decode @PlayEvent . fromStrict)
            $ switchDyn wsRespEv
    loggedInEv <- headE $
      ffilter
        (\case { LogInSuccess{} -> True; _ -> False})
        initEvts
    let joinedRoomEv =
          fmapMaybe
            (\case
              YouJoinedRoom _ _ rinfo -> Just rinfo
              RoomCreated _ _ rinfo -> Just rinfo
              _ -> Nothing
            )
          playEvEvs
    wsRespEv <- prerender (return never) $
      case checkEncoder fullRouteEncoder of
        Left err -> do
          el "div" $ text err
          return never
        Right encoder -> do
          let wsPath = fst $ encode encoder $
                FullRoute_Backend BackendRoute_Room :/ ()
              sendEv =
                pure . toStrict
                . either Aeson.encode Aeson.encode
                <$> msgSendEv
          let mUri = do
                uri' <- mkURI . T.decodeUtf8 =<< r
                pathPiece <- nonEmpty =<< mapM mkPathPiece wsPath
                wsScheme <- case uriScheme uri' of
                  rtextScheme | rtextScheme == mkScheme "https" -> mkScheme "wss"
                  rtextScheme | rtextScheme == mkScheme "http" -> mkScheme "ws"
                  _ -> Nothing
                return $ uri'
                  { uriPath = Just (False, pathPiece)
                  , uriScheme = Just wsScheme
                  }
          case mUri of
            Nothing -> return never
            Just uri -> do
              ws <- webSocket (render uri) $ def & webSocketConfig_send .~ sendEv
              return (_webSocket_recv ws)

  return ()

diceRoller
  :: (MonadIO (Performable m),
      MonadHold t m, PostBuild t m,
      PerformEvent t m,
      DomBuilder t m)
  => m (Event t Dice)
diceRoller = el "div" $ do
  btn <- constButtonM
          "Roll dice"
          (liftIO $ randomRIO (1 :: Int, 6))
  rand <- holdDyn Nothing (Just <$> btn)
  dynText $ maybe "" ((" " <>) . T.pack . show) <$> rand
  pure $ (`Dice` 6) <$> btn

constButtonM
  :: (DomBuilder t m, PerformEvent t m)
  => Text -> Performable m a -> m (Event t a)
constButtonM t act = do
  (e, _) <- elAttr' "button" ("type" =: "button") $ text t
  performEvent $ pushAlways (const $ pure act) (domEvent Click e)

joinRoomWidget ::
  ( DomBuilder t m, MonadFix m,
    PostBuild t m, PerformEvent t m,
    Prerender js t m
  )
  => m (Event t PlayCmd)
joinRoomWidget = el "div" $ do
  rec
    room <- inputElement $ def
      & inputElementConfig_setValue .~ ("" <$ eNewCmd)
      & inputElementConfig_elementConfig
      . elementConfig_initialAttributes
      .~ ("placeholder" =: "部屋名" <> "id" =: "room-id")
    pass <- inputElement $ def
      & inputElementConfig_setValue .~ ("" <$ eNewCmd)
      & inputElementConfig_elementConfig
      . elementConfig_initialAttributes
      .~ ("placeholder" =: "PASS" <>
          "type" =: "password"
          )
    -- doFocus user
    regBtn <- constButtonM "作成" (pure ())
    loginBtn <- constButtonM "入室" (pure ())
    let eNewCmd =
          attachWith
            (flip uncurry)
            (current
            $ (,) <$> value room
                  <*> (Password <$> value pass)
            )
          (leftmost
            [ CreateRoom <$ regBtn
            , JoinRoom . RoomId . fromJust . UUID.fromText <$ keypress Enter room
            , JoinRoom . RoomId . fromJust . UUID.fromText <$ keypress Enter pass
            , JoinRoom . RoomId . fromJust . UUID.fromText <$ loginBtn
            ]
          )
  return eNewCmd

roomWidget ::
  ( DomBuilder t m, MonadHold t m, MonadFix m,
    PostBuild t m, MonadIO (Performable m),
    PerformEvent t m
  )
  => Event t PlayEvent
  -> RoomInfo
  -> m (Event t PlayCmd)
roomWidget evt0 rinfo = el "div" $ do
  let evt = ffilter ((== Just (roomId rinfo)) . peRoomId) evt0
  mems <- foldDyn
    (\case
      JoinedRoom _ _ uid -> Set.insert uid
      MemberLeft _ uid _ -> Set.delete uid
      _ -> id
    )
    (roomMembers rinfo) evt
  logs <- foldDyn
    ((:) . renderLog)
    []
    evt
  el "h2" $ text $ roomName rinfo
  el "div" $ text $ "部屋ID: " <> toText (getRoomId $ roomId rinfo)
  void $ elAttr "div" ("id" =: "members") $ do
    el "h3" $ text "Memebrs"
    el "ul" $ simpleList (Set.toList <$> mems) $ \dynUid ->
      el "li" $ dynText (runUserId <$> dynUid)
  void $ elAttr "div" ("id" =: "log") $ do
    el "h3" $ text "ログ"
    elAttr "ul" ("id" =: "log-body") $
      simpleList logs $ \msg ->
        elDynAttr "li" (msgClass <$> msg) $
        dynText $ renderMsg <$> msg
  dice <- diceRoller
  pure $ leftmost
    [DiceRoll (roomId rinfo) <$> dice]

msgClass :: Msg -> Map Text Text
msgClass ErrorMsg{} = "class" =: "log-error"
msgClass Info{}     = "class" =: "log-info"
msgClass Warn{}     = "class" =: "log-warn"

renderMsg :: Msg -> Text
renderMsg msg =
  tshow (timeStamp msg) <> ": " <> message msg

data Msg
  = Info  { timeStamp :: !UTCTime
          , message   :: !Text
          }
  | Warn  { timeStamp :: !UTCTime
          , message   :: !Text
          }
  | ErrorMsg { timeStamp :: !UTCTime
          , message      :: !Text
          }
  deriving (Read, Show, Eq, Ord)

renderLog :: PlayEvent -> Msg
renderLog (JoinedRoom time _ uid) = Info time $
  runUserId uid <> " さんが入室しました"
renderLog (YouJoinedRoom time _ rinfo) =
  Info time $
  "あなたが部屋「" <> roomName rinfo
  <> "」に入室しました"
renderLog (DiceRolled time _ who (Dice me mx)) =
  Info time $
      runUserId who <> "さんが【"
  <> tshow mx <> "面】ダイスを振り【"
  <> tshow me <>"】を出しました"
renderLog (Bye now) =
  Info now "さようなら"
renderLog (InvalidCommand now str) = ErrorMsg now $
  "内部エラーです：" <> T.pack str
renderLog (MemberLeft now who _) =
  Info now $ runUserId who <> "さんが去りました"
renderLog (RoomCreated now _ rinfo) =
  Info now $
    "部屋を作成しました：" <> roomName rinfo
renderLog (RoomNotFound now rid) =
  ErrorMsg now $
    "そんな識別子の部屋はありません: " <> tshow (getRoomId rid)
renderLog (NotRoomMember now _ _) =
  ErrorMsg now "あなたは部屋のメンバーではありません"
renderLog (JoinFailed now _) =
  ErrorMsg now "その部屋には入れません"

tshow :: Show a => a -> T.Text
tshow = T.pack . show

loginWidget ::
  ( DomBuilder t m, MonadFix m,
    MonadHold t m,
    PostBuild t m, PerformEvent t m,
    Prerender js t m
  )
  => Event t InitEvent
  -> m (Event t InitCmd)
loginWidget evt = el "div" $ do
  rec
    curResp <- holdDyn (Right ()) colorOrChange
    let colorOrChange = leftmost
            [ Left <$> evt
            , Right () <$ domEvent Input user
            , Right () <$ domEvent Input pass
            ]
        colorEvt = colorOrChange <&> \case
          Left LogInSuccess{} -> mempty
          Right () -> "style" =: Nothing
          Left _ -> "style" =: Just "background-color: red"
    user <- inputElement
      $ def
      & inputElementConfig_setValue .~ (runUserId . initUid <$> eNewCmd)
      & inputElementConfig_elementConfig
      %~ (elementConfig_initialAttributes
              .~ ("placeholder" =: "User Name")
          >>> elementConfig_modifyAttributes .~ colorEvt
          )
    pass <- inputElement $ def
      & inputElementConfig_setValue .~ (getPassword . initPassword <$> eNewCmd)
      & inputElementConfig_elementConfig
      %~ (elementConfig_initialAttributes
            .~ ("placeholder" =: "PASS" <>
                "type" =: "password"
                )
          >>> elementConfig_modifyAttributes .~ colorEvt
          )

    -- doFocus user
    regBtn <- constButtonM "登録" $ pure ()
    loginBtn <- constButtonM "Login" $ pure ()
    let eNewCmd =
          attachWith
            (flip uncurry)
            (current
            $ (,) . UserId . T.strip <$> value user <*> (Password <$> value pass)
            )
          (leftmost
            [ CreateUser <$ regBtn
            , LogIn <$ keypress Enter user
            , LogIn <$ keypress Enter pass
            , LogIn <$ loginBtn
            ]
          )
    elDynAttr "div"
      (curResp <&> \case
        Right () -> "style" =: "visibility: hidden"
        Left LogInSuccess{} -> "style" =: "visibility: hidden"
        Left _ -> "style" =: "visibility: visible; font-size: small; color: red"
      )
      $ dynText $ curResp <&> \case
        Left (LogInFailed now uid) ->
          T.pack (show now) <> ": ログイン失敗（" <> runUserId uid <>  "）；パスワードか名前を確認してね"
        Left (UserAlreadyExists now uid) ->
          T.pack (show now) <> ": ログイン失敗（" <> runUserId uid <>  "）；パスワードか名前を確認してね"
        _ -> ""

  return eNewCmd

-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Pλay At Home"
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = app
  }

{- doFocus
  :: (DomBuilder t m,
      PostBuild t m,
      PerformEvent t m,
      Prerender js t m
    )
  => InputElement EventResult (DomBuilderSpace m) t
  -> m ()
doFocus ie = prerender_ (return ()) $ do
  pb <- getPostBuild
  let h = _inputElement_raw ie
  performEvent_ (fmap (liftJSM . const (focus h)) pb)
  return ()
 -}
