{-# LANGUAGE ApplicativeDo, DataKinds, DeriveAnyClass, DeriveGeneric    #-}
{-# LANGUAGE DerivingStrategies, ExtendedDefaultRules, FlexibleContexts #-}
{-# LANGUAGE GADTs, LambdaCase, OverloadedStrings, RecordWildCards      #-}
{-# LANGUAGE RecursiveDo, TypeApplications                              #-}
{-# OPTIONS_GHC -Wall -Wno-type-defaults #-}
module Web.PlayAtHome.Frontend where
import Web.PlayAtHome.Route
import Web.PlayAtHome.Types

import           Control.Arrow               ((>>>))
import           Control.Lens                hiding ((.=))
import           Control.Lens.Extras         (is)
import           Control.Monad               (void, when)
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Data.Aeson                  (object, (.=))
import qualified Data.Aeson                  as Aeson
import           Data.ByteString.Lazy        (fromStrict, toStrict)
import           Data.Dependent.Sum
import           Data.List.NonEmpty          (nonEmpty)
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as M
import           Data.Maybe                  (fromMaybe)
import           Data.Monoid                 ((<>))
import           Data.Promise
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import           Data.Time
import           Data.UUID                   as UUID
import           GHC.Generics                (Generic)
import           JSDOM.Types                 (JSVal)
import           Language.Javascript.JSaddle
import           Obelisk.Configs
import           Obelisk.Frontend
import           Obelisk.Generated.Static
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Reflex
import           Reflex.Dom
import           System.Random
import           Text.URI                    hiding (Password)

newtype Auth0 = Auth0 { runAuth0  :: JSVal }
  deriving (Generic)
  deriving anyclass (ToJSVal, FromJSVal)
default (Text)

getAuth0 :: MonadJSM m => Auth0Config -> m (Promise Auth0)
getAuth0 cfg = liftJSM $
  Promise . Auth0 <$> jsg1 "createAuth0Client" (Aeson.toJSON cfg)

instance MakeObject Auth0 where
  makeObject (Auth0 ob) = pure $ Object ob

isAuthenticated
  :: MonadJSM m => Auth0 -> m Bool
isAuthenticated auth0 = liftJSM $
  fromJSValUnchecked =<< (auth0 ^. js0 ("isAuthenticated" :: String))

app ::
  ( DomBuilder t m, MonadFix m,
    PostBuild t m, PerformEvent t m,
    Prerender js t m, HasConfigs m,
    Reflex t,
    MonadIO (Performable m),
    MonadHold t m,
    Routed t (DSum FrontendRoute Identity) m
  ) => m ()
app = do
  r <- sample . current =<< askRoute
  let params :: Map Text (Maybe Text)
      params = case r of
        FrontendRoute_Main :=> Identity dic -> dic
  route <- getConfig "common/route"
  authConf <- fromMaybe (error "Aeson decoding config failed") . Aeson.decode @Auth0Config . fromStrict . fromMaybe (error "Reading auth_config failed")
    <$> getConfig "common/auth_config.json"
  el "h1" $ text "Pλay At Home"
  void $ prerender (el "div" $ text "loading...") $ do
    rec
      pb <- getPostBuild
      auth0Ev <- performEvent $ pb <&> \_ -> do
        auth0 <- liftJSM . await =<< getAuth0 authConf
        console <- liftJSM $ jsg "console"
        liftJSM $ do
          console ^. js1 "log" (Aeson.toJSON authConf)
          console ^. js1 "log" auth0
          console ^. js1 "log" (M.keys params)

        let hasCode = M.member "code" params && M.member "state" params
        when hasCode $
          void $ liftJSM $ auth0 ^. js0 "handleRedirectCallback"
        pure $ Just auth0
      dynAuth0 <- holdDyn Nothing  auth0Ev
      msgEvDyn <-
        widgetHold (fmap Aeson.toJSON <$> loginWidget dynAuth0 initEvts)
        (leftmost
          [ fmap (fmap Aeson.toJSON) (joinRoomWidget playEvEvs) <$
              leftmost [roomFailureEv, loggedInEv]
          , fmap (fmap Aeson.toJSON) . roomWidget playEvEvs <$> joinedRoomEv
          ]
        )
      let msgSendEv = switch $ current msgEvDyn
          initEvts = fmapMaybe (Aeson.decode @InitEvent . fromStrict)
              $ switchDyn wsRespEv
          playEvEvs =
            fmapMaybe (Aeson.decode @PlayEvent . fromStrict)
              $ switchDyn wsRespEv
          roomFailureEv = () <$
            ffilter
              (\case
                RoomNotFound{} -> True
                JoinFailed {} -> True
                _ -> False
              )
            playEvEvs
          loggedInEv = () <$
            ffilter
              (\case
                  LogInSuccess{} -> True
                  _ -> False
              )
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
                  pure . toStrict . Aeson.encode <$> msgSendEv
            let mUri = do
                  uri' <- mkURI . T.decodeUtf8 =<< route
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
  dice <- diceRoller
  void $ elAttr "div" ("id" =: "log") $ do
    el "h3" $ text "ログ"
    elAttr "ul" ("id" =: "log-body") $
      simpleList logs $ \msg ->
        elDynAttr "li" (msgClass <$> msg) $
        dynText $ renderMsg <$> msg
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

data LoginLikeForm ident evt cmd  =
  LoginLikeForm
  { loginHeader     :: Text
  , loginLabel      :: Text
  , createLabel     :: Text
  , namePlaceholder :: Text
  , validateName    :: Text -> Either Text ident
  , nameId          :: Text
  , passId          :: Text
  , loginCmd        :: ident -> Password -> cmd
  , createCmd       :: Text -> Password -> cmd
  , validateEvent   :: evt -> Either Text ()
  }

mkLoginLikeForm ::
  ( DomBuilder t m,
    MonadFix m,
    MonadHold t m,
    PostBuild t m,
    PerformEvent t m,
    Prerender js t m
  )
  => LoginLikeForm ident evt cmd
  -> Event t evt
  -> m (Event t cmd)
mkLoginLikeForm LoginLikeForm{..} evt = el "div" $ do
  el "h2" $ text loginHeader
  rec
    let colorOrChange = leftmost
            [ validateEvent <$> evt
            , Right () <$ domEvent Input name
            , Right () <$ domEvent Input pass
            , (() <$) <$> loginEv'
            ]
        colorEvt = colorOrChange <&> \case
          Right _ -> "style" =: Nothing
          Left _  -> "style" =: Just "background-color: red"
    name <- inputElement
      $ def
      & inputElementConfig_elementConfig
      %~ (elementConfig_initialAttributes
          .~ ("placeholder" =: namePlaceholder <>
              "id" =: nameId
              )
          >>> elementConfig_modifyAttributes .~ colorEvt
          )
    pass <- inputElement $ def
      & inputElementConfig_elementConfig
      %~ (elementConfig_initialAttributes
            .~ ("placeholder" =: "PASS" <>
                "type" =: "password" <>
                "id" =: passId
                )
          >>> elementConfig_modifyAttributes .~ colorEvt
          )
    let isDisabled =
          (||)  <$> (T.null <$> value name)
                <*> (T.null <$> value pass)
        disabled b  | b = "disabled" =: "disabled" :: Map Text Text
                    | otherwise =  mempty
    loginDis <- holdDyn False (is _Left <$> loginEv')
    regBtn <- domEvent Click . fst
          <$> elDynAttr' "button" (mappend ("type" =: "button") . disabled <$> isDisabled)
              (text createLabel)
    loginBtn <- domEvent Click . fst
            <$> elDynAttr' "button" (mappend ("type" =: "button") . disabled <$>
                    ((||) <$> loginDis <*> isDisabled)
                  )
                (text loginLabel)
    let eNewCmd = leftmost [createEv, loginEv]
        createEv =
          current (createCmd <$> value name <*>  (Password <$> value pass))
            <@ regBtn
        loginEv = fmapMaybe (either (const Nothing) Just)  loginEv'
        loginEv' =
          current (loginCmdM <$> value name <*> (Password <$> value pass))
          <@
          leftmost [keypress Enter name, keypress Enter pass, loginBtn]
    curResp <- holdDyn (Right ()) colorOrChange
    elDynAttr "div"
      (curResp <&> \case
        Right _ -> "style" =: "visibility: hidden"
        Left _  -> "style" =: "visibility: visible; font-size: small; color: red"
      )
      $ dynText $ either id (const "") <$> curResp

  return eNewCmd
  where
    loginCmdM txt pw =
      flip loginCmd pw <$> validateName txt

loginWidget ::
  ( DomBuilder t m, MonadFix m,
    MonadHold t m,
    PostBuild t m,
    PerformEvent t m,
    Prerender js t m,
    MonadJSM (Performable m)
  )
  => Dynamic t (Maybe Auth0)
  -> Event t InitEvent
  -> m (Event t InitCmd)
loginWidget  auth0 _iev = el "div" $ do
  rec
    (login, _) <- elDynAttr' "button"
      (mappend ("id" =: "btn-login")
      . (\p -> if p then "disabled" =: "disabled" else mempty)
        <$> loggedIn
      ) $
        text "ログイン"
    (logout, _) <- elDynAttr' "button"
      (mappend ("id" =: "btn-login")
      . (\p -> if p then mempty else "disabled" =: "disabled")
      <$> loggedIn
      ) $
        text "ログアウト"
    let loginEv = domEvent Click login
        btnEvs = leftmost [loginEv, domEvent Click logout]
    v <- prerender (return False) $ do
        mauth0 <- sample $ current auth0
        maybe (pure False) isAuthenticated mauth0
    loggedIn <- holdDyn False (tagPromptlyDyn v btnEvs)

  performEvent $
    tagPromptlyDyn
    (auth0 <&> \case
      Nothing -> pure ()
      (Just auther) -> liftJSM $
        void $ auther ^. js1 "loginWithRedirect"
                  (object ["redirect_uri" .= "http://localhost:8000/" ])
    )
    loginEv
  pure never


joinRoomWidget ::
  ( DomBuilder t m, MonadFix m,
    PostBuild t m, PerformEvent t m,
    Prerender js t m, MonadHold t m
  )
  => Event t PlayEvent
  -> m (Event t PlayCmd)
joinRoomWidget = mkLoginLikeForm LoginLikeForm
    { loginHeader     = "部屋の作成／入室"
    , loginLabel      = "入室"
    , createLabel     = "作成"
    , namePlaceholder = "部屋ID／部屋名"
    , nameId          = "room-id"
    , passId          = "room-pass"
    , validateName =
        maybe (Left "部屋名が正しくありません") (Right . RoomId) . UUID.fromText
    , loginCmd        = JoinRoom
    , createCmd       = CreateRoom
    , validateEvent   = \case
        RoomNotFound now rid ->
          Left $ tshow now <> "そんな部屋ありません：" <> UUID.toText (getRoomId rid)
        JoinFailed now rid ->
          Left $ tshow now <> "そんな部屋ありません：" <> UUID.toText (getRoomId rid)
        _ -> Right ()
    }


-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Pλay At Home"
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do
      auth0Url <- T.decodeUtf8 . fromMaybe "frontend/auth0" <$> getConfig "frontend/auth0"
      app
      elAttr "script" ("src" =: auth0Url) blank
  }
