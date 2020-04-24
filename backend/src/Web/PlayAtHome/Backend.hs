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
import           Data.Maybe                   (fromJust)
import qualified Data.Set                     as S
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
  :: (MonadThrow m, MonadIO m, MonadReader ServerEnv m) => Token -> m UserId
validateToken tok = do
  Auth0Config{..} <- view auth0ConfigL
  case JWT.decodeCompact @_ @JWTError $ LBS.fromStrict $ T.encodeUtf8 tok of
    Right jwt | Just aud <- audience -> do
      url <- view jwkUrlL
      jwks <- liftIO $ fmap (view responseBody) . asJSON @_ @JWKSet
          =<< get url
      now <- getCurrentTime
      let chk = (== fromString (T.unpack aud))
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
    _ -> do
      let wopts = defaults & auth ?~ oauth2Bearer (T.encodeUtf8 tok)
      bdy <- liftIO $ fmap (view responseBody) . asJSON @_ @Value
        =<< getWith wopts (T.unpack $ "http://" <> domain <> "/userinfo")
      case bdy ^? key "sub" . _JSON of
        Just uid -> pure uid
        Nothing  -> throwM $ InvalidAccessToken tok

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
    Right uid -> do
      send conn $ LogInSuccess now uid
      startSession tState conn uid

startSession
  :: forall m. MonadUnliftIO m
  => TVar ServerState
  -> WS.Connection
  -> UserId
  -> m ()
startSession tsess conn uid = do
  atomically $
    modifyTVar' tsess $
      serverUserConnsL %~ HM.insert uid conn
  mainLoop `finally` unregisterUser tsess uid conn
  where
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
                            %~ S.insert uid
                  writeTVar tsess $
                    st & serverRoomsL.at rid ?~ rinfo
                  pure $ Right rinfo
              _ ->  pure $ Left $ JoinFailed now rid
          case eMsg of
            Left msg -> send conn msg
            Right rinfo -> do
              send conn $ YouJoinedRoom now rid rinfo
              forM_ (roomMembers rinfo) $ \uid' ->
                when (uid /= uid') $ do
                conns <- serverUserConns
                  <$> readTVarIO tsess
                forM_ (HM.lookup uid' conns) $ \conn' ->
                  send conn' $ JoinedRoom now rid uid

        Right (CreateRoom title pwd) -> do
          rid <- RoomId <$> liftIO UUID.nextRandom
          hsh <- liftIO $ hashPassword pwd 12
          let rinfo = RoomInfo
                { roomName = title
                , roomMembers = S.singleton uid
                , roomId = rid
                }
          atomically $ modifyTVar' tsess $ \st ->
            st  & serverRoomsL      %~ HM.insert rid rinfo
                & serverRoomPassesL %~ HM.insert rid hsh
          now <- getCurrentTime
          send conn $
            RoomCreated now rid rinfo
          send conn $
            JoinedRoom now rid uid

        Right (DiceRoll rid dice) -> do
          now <- getCurrentTime
          mans <- HM.lookup rid . serverRooms
            <$> readTVarIO tsess
          case mans of
            Nothing -> send conn $ RoomNotFound now rid
            Just RoomInfo{..}
              | uid `S.member` roomMembers
              -> forM_ roomMembers $ \uid' -> do
                conns <- serverUserConns
                  <$> readTVarIO tsess
                forM_ (HM.lookup uid' conns) $ \c ->
                  send c $ DiceRolled now rid uid dice
              | otherwise -> send conn
                  $ NotRoomMember now rid uid

unregisterUser
  :: MonadUnliftIO m
  => TVar ServerState -> UserId -> WS.Connection -> m ()
unregisterUser tses uid conn = do
  _rooms <- atomically $ do
    st@ServerState{..} <- readTVar tses
    writeTVar tses $
      st  & serverUserConnsL %~ HM.delete uid
          & serverRoomsL %~ HM.map (roomMembersL %~ S.delete uid)
    pure $ HM.filter (S.member uid . roomMembers)
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
        let env = ServerEnv conf jwksPath logFun
        state <- newTVarIO initServerState
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
