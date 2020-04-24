{-# LANGUAGE FlexibleContexts, GADTs, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE RecordWildCards, ScopedTypeVariables, TypeApplications #-}
{-# OPTIONS_GHC -Wall #-}
module Web.PlayAtHome.Backend where
import           Control.Lens
import           Control.Monad
import           Crypto.JOSE.JWK
import           Crypto.JWT                   as JWT
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Dependent.Sum           (DSum (..))
import qualified Data.HashMap.Strict          as HM
import qualified Data.Map.Strict              as M
import           Data.Maybe                   (fromJust)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import qualified Data.UUID.V4                 as UUID
import qualified Network.WebSockets           as WS
import           Network.WebSockets.Snap
import           Network.Wreq
import           Obelisk.Backend
import           RIO                          hiding (view, (^.))
import qualified RIO.ByteString.Lazy          as LBS
import           RIO.Time
import           Web.PlayAtHome.Backend.Types
import           Web.PlayAtHome.Route
import           Web.PlayAtHome.Types

application
  :: TVar ServerState -> WS.PendingConnection -> RIO ServerEnv ()
application tState pending = do
  conn <- liftIO $ WS.acceptRequest pending
  liftIO $ WS.forkPingThread conn 30
  send conn Welcome
  src <- receiveRaw conn
  case eitherDecode @InitCmd src of
    Left err -> do
      now <- getCurrentTime
      send conn $ InvalidCommand now $ "Invalid command: " ++ show err
    Right cmd -> procInitialCmd tState conn cmd

send :: (MonadIO m, ToJSON d) => WS.Connection -> d -> m ()
send conn = liftIO . WS.sendBinaryData conn . encode

validateToken
  :: (MonadThrow m, MonadIO m, MonadReader ServerEnv m) => Token -> m UserInfo
validateToken tok = do
  Auth0Config{..} <- view auth0ConfigL
  let eResp = JWT.decodeCompact @_ @JWTError
          $ LBS.fromStrict $ T.encodeUtf8 tok
  euid <- forM eResp $ \ jwt -> do
    url <- view jwkUrlL
    jwks <- liftIO $ fmap (view responseBody) . asJSON @_ @JWKSet
        =<< get url
    now <- getCurrentTime
    let chk = maybe (const False) ((==) . fromString . T.unpack) audience
        validIssuer = (== fromString (T.unpack ("https://" <> domain <> "/")))
        ecSet :: Either JWTError ClaimsSet
        ecSet = flip runReaderT now $
          verifyClaims (defaultJWTValidationSettings chk) jwks jwt
    case ecSet of
      Right cset
        | Just uid <- cset ^. claimSub
        , Just iss <- cset ^. claimIss
        , validIssuer iss  ->
            pure $ UserId $ uid ^.string
      _ -> throwM $ InvalidAccessToken tok
  let wopts = defaults & auth ?~ oauth2Bearer (T.encodeUtf8 tok)
  bdy <- liftIO $ fmap (view responseBody) . asJSON @_ @Value
    =<< getWith wopts (T.unpack $ "http://" <> domain <> "/userinfo")
  let pair =
        (,) <$> (either (const Nothing) Just euid <|> bdy ^? key "sub" . _JSON)
            <*> bdy ^? key "name". _JSON
  case pair of
    Just (userId, userName) ->
      let userNickname = fromMaybe userName
            $ bdy ^? key "nickname" . _JSON
            <|> bdy ^? key "preferred_username" ._JSON
          userPicture = bdy ^? key "picture" ._JSON
      in pure UserInfo{..}
    Nothing          -> throwM $ InvalidAccessToken tok

procInitialCmd
  :: (MonadUnliftIO m, MonadThrow m, MonadReader ServerEnv m)
  => TVar ServerState
  -> WS.Connection -> InitCmd -> m ()
procInitialCmd tState conn (LogIn tok) = do
  now <- getCurrentTime
  ServerState{..} <- readTVarIO tState
  eresl <- try @_ @AuthError $ validateToken tok
  case eresl of
    Left err -> do
      send conn $ LogInFailed now $ tshow err
      procInitialCmd tState conn
        =<< receive conn
    Right uinfo -> do
      send conn $ LogInSuccess now uinfo
      startSession tState conn uinfo

startSession
  :: forall m. MonadUnliftIO m
  => TVar ServerState
  -> WS.Connection
  -> UserInfo
  -> m ()
startSession tsess conn uinfo = do
  atomically $
    modifyTVar' tsess $
      serverUserConnsL %~ HM.insert (uinfo ^. userIdL) conn
  mainLoop `finally` unregisterUser tsess uinfo conn
  where
    uid = uinfo ^. userIdL
    mainLoop :: MonadIO m => m ()
    mainLoop = forever $ do
      src <- receiveRaw conn
      case eitherDecode src of
        Left err -> do
          now <- getCurrentTime
          send conn $ InvalidCommand now $ show err
        Right (JoinRoom rid pwd) -> do
          now <- getCurrentTime
          eMsg <- atomically $ do
            st@ServerState{..} <- readTVar tsess
            case HM.lookup rid serverRoomPasses of
              Just hsh
                | isValidPassword hsh pwd
                -> do
                  let rinfo = (serverRooms HM.! rid)
                            & roomMembersL
                            %~ M.insert uid uinfo
                  writeTVar tsess $
                    st & serverRoomsL.at rid ?~ rinfo
                  pure $ Right rinfo
              _ ->  pure $ Left $ JoinFailed now rid
          case eMsg of
            Left msg -> send conn msg
            Right rinfo -> do
              send conn $ YouJoinedRoom now rid rinfo
              forM_ (roomMembers rinfo) $ \uinfo' ->
                let uid' = uinfo' ^. userIdL
                in when (uid /= uid') $ do
                conns <- serverUserConns
                  <$> readTVarIO tsess
                forM_ (HM.lookup uid' conns) $ \conn' ->
                  send conn' $ JoinedRoom now rid uinfo

        Right (CreateRoom title pwd) -> do
          rid <- RoomId <$> liftIO UUID.nextRandom
          hsh <- liftIO $ hashPassword pwd 12
          let rinfo = RoomInfo
                { roomName = title
                , roomMembers = M.singleton uid uinfo
                , roomId = rid
                }
          atomically $ modifyTVar' tsess $ \st ->
            st  & serverRoomsL      %~ HM.insert rid rinfo
                & serverRoomPassesL %~ HM.insert rid hsh
          now <- getCurrentTime
          send conn $
            RoomCreated now rid rinfo
          send conn $
            JoinedRoom now rid uinfo

        Right (DiceRoll rid dice) -> do
          now <- getCurrentTime
          mans <- HM.lookup rid . serverRooms
            <$> readTVarIO tsess
          case mans of
            Nothing -> send conn $ RoomNotFound now rid
            Just RoomInfo{..}
              | uid `M.member` roomMembers
              -> forM_ roomMembers $ \uinfo' -> do
                let uid' = uinfo' ^. userIdL
                conns <- serverUserConns
                  <$> readTVarIO tsess
                forM_ (HM.lookup uid' conns) $ \c ->
                  send c $ DiceRolled now rid uinfo dice
              | otherwise -> send conn
                  $ NotRoomMember now rid uid

unregisterUser
  :: MonadUnliftIO m
  => TVar ServerState -> UserInfo -> WS.Connection -> m ()
unregisterUser tses uinfo conn = do
  let uid = uinfo ^. userIdL
  _rooms <- atomically $ do
    st@ServerState{..} <- readTVar tses
    writeTVar tses $
      st  & serverUserConnsL %~ HM.delete uid
          & serverRoomsL %~ HM.map (roomMembersL %~ M.delete uid)
    pure $ HM.filter (M.member uid . roomMembers)
        $ st ^. serverRoomsL
  -- TODO: Send @MemberLeft@ commands to remaining members
  now <- getCurrentTime
  liftIO $ WS.sendClose conn $ encode $ Bye now

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      conf@Auth0Config{..} <- either fail pure =<<
        eitherDecodeFileStrict @Auth0Config "config/common/auth_config.json"
      let jwksPath = T.unpack $ "https://" <> domain <> "/.well-known/jwks.json"
      opts <- logOptionsHandle stdout True
      withLogFunc opts $ \logFun -> do
        state <- newTVarIO initServerState
        let env = ServerEnv conf jwksPath logFun state
        serve $ \case
          BackendRoute_Missing :=> Identity () -> return ()
          BackendRoute_Room :=> Identity () ->
            runWebSocketsSnap (runRIO env . application state)
  , _backend_routeEncoder = fullRouteEncoder
  }

receive :: (FromJSON a, MonadIO m) => WS.Connection -> m a
receive = liftIO . fmap (fromJust . decode) . WS.receiveData

receiveRaw :: (MonadIO m) => WS.Connection -> m LBS.ByteString
receiveRaw = liftIO . WS.receiveData
