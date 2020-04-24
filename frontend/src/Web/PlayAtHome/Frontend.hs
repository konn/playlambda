{-# LANGUAGE DataKinds, DerivingStrategies, ExtendedDefaultRules    #-}
{-# LANGUAGE FlexibleContexts, GADTs, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures, RecordWildCards, RecursiveDo    #-}
{-# LANGUAGE TypeApplications                                       #-}
{-# OPTIONS_GHC -Wall -Wno-type-defaults #-}
module Web.PlayAtHome.Frontend where
import Web.PlayAtHome.Frontend.Types
import Web.PlayAtHome.Route
import Web.PlayAtHome.Types

import           Control.Arrow               ((>>>))
import           Control.Lens                hiding ((.=))
import           Control.Lens.Extras         (is)
import           Control.Monad               (guard, join, void, when)
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Data.Aeson                  (object, (.=))
import qualified Data.Aeson                  as Aeson
import           Data.ByteString.Lazy        (fromStrict)
import           Data.Dependent.Sum
import           Data.List.NonEmpty          (nonEmpty)
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as M
import           Data.Maybe                  (fromJust, fromMaybe)
import           Data.Monoid                 ((<>))
import           Data.Promise
import           Data.Semialign
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import           Data.These
import           Data.Time
import           Data.UUID                   as UUID
import           JSDOM                       (currentWindowUnchecked)
import           JSDOM.Custom.Window         (getHistory)
import           JSDOM.Generated.History     (replaceState)
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

app ::
  ( DomBuilder t m, MonadFix m,
    PostBuild t m, PerformEvent t m,
    Prerender js t m, HasConfigs m,
    Reflex t, MonadHold t m,
    MonadIO (Performable m),
    MonadHold t m,
    Routed t (DSum FrontendRoute Identity) m
  ) => m ()
app = do
  route <- T.decodeUtf8 . fromJust <$> getConfig "common/route"
  r <- sample . current =<< askRoute
  let params :: Map Text (Maybe Text)
      params = case r of
        FrontendRoute_Main :=> Identity dic -> dic
  authConf <- fromMaybe (error "Aeson decoding config failed")
    . Aeson.decode @Auth0Config
    . fromStrict . fromMaybe (error "Reading auth_config failed")
    <$> getConfig "common/auth_config.json"
  el "h1" $ text "Pλay At Home"
  dynAuth0 <- fmap join $ prerender
    (do el "div" (text "loading...")
        pure $ constDyn NoAuthInfo) $ do
    pb <- getPostBuild
    ev <- performEvent $ pb <&> \_ -> do
      auth0 <- liftJSM . await =<< getAuth0 authConf

      let hasCode = M.member "code" params && M.member "state" params
      when hasCode $ liftJSM $ do
        void $
          await @JSVal =<< fromJSValUnchecked
            =<< (auth0 ^. js0 "handleRedirectCallback")
        window <- currentWindowUnchecked
        hist <- getHistory window
        replaceState hist obj "Pλay At Home" (Just route)

      authed <- isAuthenticated auth0
      if authed
        then maybe (Unauthenticated auth0) (Authenticated auth0)
              <$> getTokenSilently auth0
        else pure $ Unauthenticated auth0
    holdDyn NoAuthInfo ev
  loginWidget route dynAuth0

  rec
    msgSendEv <-
      switchHold never
      =<< dyn (buildPageBody route resps roomEv <$> dynAuth0)
    let roomEv = fmapMaybe (Aeson.decode @RoomEvent . fromStrict) $
          switchDyn wsRespEv
        resps = fmapMaybe (Aeson.decode @ClientResp . fromStrict)
            $ switchDyn wsRespEv

    wsRespEv <- prerender (return never) $
      case checkEncoder fullRouteEncoder of
        Left err -> do
          el "div" $ text err
          return never
        Right encoder -> do
          let wsPath = fst $ encode encoder $
                FullRoute_Backend BackendRoute_Room :/ ()
              sendEv = (:[]) . Aeson.encode <$> msgSendEv
          let mUri = do
                uri' <- mkURI route
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

buildPageBody
  :: (MonadFix m,
      DomBuilder t m,
      Prerender js t m, MonadHold t m,
      PerformEvent t m, PostBuild t m,
      MonadIO (Performable m)
      )
  => Text
  -> Event t ClientResp
  -> Event t RoomEvent
  -> AuthState
  -> m (Event t ClientCmd)
buildPageBody _ evResp roomEvt (Authenticated _ tok) = do
  let isWelcome = L (LogIn tok) <$ ffilter (== L Welcome) evResp
      playResps = filterRight (getOneOf <$> evResp)
      rinfoEv = fmapMaybe
          (\case
            YouJoinedRoom _ _ rinfo -> Just rinfo
            RoomCreated _ _ rinfo -> Just rinfo
            _ -> Nothing
          )
        roomEvt
  cmds <- fmap switchDyn
    $ widgetHold (do el "div" $ text "Loading..."; pure never)
    $ alignWith
        (\case
          This (L (LogInSuccess _ uid)) -> do
            el "div" $ text $ "Logged in as: " <> tshow uid
            joinRoomWidget playResps
          This (L (LogInFailed _ err)) -> do
            el "div" $ text $ "LogIn Failed!: " <> err
            pure never
          This _ -> pure never
          These _ rinfo -> roomWidget roomEvt rinfo
          That rinfo -> roomWidget roomEvt rinfo
        )
        evResp
        rinfoEv
  pure $ leftmost [isWelcome, R <$> cmds]
buildPageBody _ _ _ NoAuthInfo = pure never
buildPageBody _ _ _ Unauthenticated{} = pure never


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
  => Event t RoomEvent
  -> RoomInfo
  -> m (Event t PlayCmd)
roomWidget evt0 rinfo = el "div" $ do
  let evt = ffilter ((== Just (roomId rinfo)) . reRoomId) evt0
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

renderLog :: RoomEvent -> Msg
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
renderLog (MemberLeft now who _) =
  Info now $ runUserId who <> "さんが去りました"
renderLog (RoomCreated now _ rinfo) =
  Info now $
    "部屋を作成しました：" <> roomName rinfo

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

data LogInOrOut = IsLogin | IsLogout
  deriving (Read, Show, Eq, Ord)

loginWidget ::
  ( DomBuilder t m, MonadFix m,
    MonadHold t m,
    PostBuild t m,
    PerformEvent t m,
    Prerender js t m,
    MonadJSM (Client m)
  )
  => Text
  -> Dynamic t AuthState
  -> m ()
loginWidget route authSt = el "div" $ do
  let mkDisable _ NoAuthInfo               = "disabled" =: "disabled"
      mkDisable IsLogin Authenticated{}    = "disabled" =: "disabled"
      mkDisable IsLogout Unauthenticated{} = "disabled" =: "disabled"
      mkDisable _ _                        = mempty
      mkBtnAttr ident logTy =
        mappend ("id" =: ident)
          <$> (mkDisable logTy <$> authSt)
  rec
    (login, _) <- elDynAttr' "button"
      (mkBtnAttr "btn-login" IsLogin) $
        text "ログイン"
    (logout, _) <- elDynAttr' "button"
      (mkBtnAttr "btn-logout" IsLogout) $
      text "ログアウト"
    let loginEv = domEvent Click login
        logOutEv = domEvent Click logout
    void $prerender (pure ()) $
      void $ performEvent $ tagPromptlyDyn authSt logOutEv <&> \case
      NoAuthInfo -> pure ()
      st -> void $
        liftJSM $ void $ frontendAuth0 st ^. js1 "logout"
        (object ["retunTo" .= route])

  void $ prerender (pure ()) $ void $ performEvent_ $
    tagPromptlyDyn
    (authSt <&> \case
      Unauthenticated auther -> void $
        liftJSM $
          void $ auther ^. js1 "loginWithRedirect"
                    (object ["redirect_uri" .= route ])
      _ -> pure ()
    )
    loginEv

joinRoomWidget ::
  ( DomBuilder t m, MonadFix m,
    PerformEvent t m, PostBuild t m,
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
