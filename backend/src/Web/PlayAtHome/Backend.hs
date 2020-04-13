{-# LANGUAGE GADTs, LambdaCase, OverloadedStrings, RecordWildCards #-}
{-# LANGUAGE TypeApplications                                      #-}
module Web.PlayAtHome.Backend where
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Dependent.Sum           (DSum (..))
import qualified Data.HashMap.Strict          as HM
import qualified Data.HashSet                 as HS
import           Data.Time
import qualified Data.UUID.V4                 as UUID
import qualified Network.WebSockets           as WS
import           Network.WebSockets.Snap
import           Obelisk.Backend
import           Web.PlayAtHome.Backend.Types
import           Web.PlayAtHome.Route
import           Web.PlayAtHome.Types

application :: TVar ServerState -> WS.ServerApp
application tState pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  src <- WS.receiveData conn
  case eitherDecode @InitCmd src of
    Left err ->
      WS.sendClose conn $ encode $
        InvalidCommand $ "Invalid command: " ++ show err
    Right cmd -> procInitialCmd tState conn cmd

send :: ToJSON d => WS.Connection -> d -> IO ()
send conn = WS.sendBinaryData conn . encode

procInitialCmd
  :: TVar ServerState
  -> WS.Connection -> InitCmd -> IO ()
procInitialCmd tState conn (LogIn uid passwd) = do
  now <- getCurrentTime
  ServerState{..} <- readTVarIO tState
  case HM.lookup uid serverUserPasses of
    Just hsh
      | isValidPassword hsh passwd -> do
        send conn $ LogInSuccess now uid
        startSession tState conn uid
    _ -> WS.sendClose conn $ encode $
          LogInFailed now uid
procInitialCmd tState conn (CreateUser uid passwd) = do
  now <- getCurrentTime
  phash <- hashPassword passwd 12
  est <- atomically $ do
    ServerState{..} <- readTVar tState
    if uid `HM.member` serverUserPasses
      then pure $ Left $ UserAlreadyExists now uid
      else do
        modifyTVar' tState $ \st ->
          st & serverUserPassesL %~ HM.insert uid phash
        pure $ Right $ LogInSuccess now uid
  case est of
    Right msg -> do
      send conn msg
      startSession tState conn uid
    Left err -> WS.sendClose conn $ encode err

startSession
  :: TVar ServerState
  -> WS.Connection
  -> UserId
  -> IO ()
startSession tsess conn uid = do
  atomically $
    modifyTVar' tsess $
      serverUserConnsL %~ HM.insert uid conn
  mainLoop `finally` unregisterUser tsess uid conn
  where
    mainLoop = forever $ do
      src <- WS.receiveData conn
      case eitherDecode src of
        Left err -> do
          send conn $ InvalidCommand $ show err
          throwIO $ userError "Invalid command"
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
                            %~ HS.insert uid
                  writeTVar tsess $
                    st & serverRoomsL.at rid ?~ rinfo
                  pure $ Right rinfo
              _ -> pure $ Left $ JoinFailed now rid
          case eMsg of
            Left msg -> send conn msg
            Right rinfo -> do
              send conn $ YouJoinedRoom now rid rinfo
              forM_ (roomMembers rinfo) $ \uid' ->
                when (uid /= uid) $ do
                conns <- serverUserConns
                  <$> readTVarIO tsess
                forM_ (HM.lookup uid' conns) $ \conn' ->
                  send conn' $ JoinedRoom now rid uid

        Right (CreateRoom title pwd) -> do
          rid <- RoomId <$> UUID.nextRandom
          hsh <- hashPassword pwd 12
          atomically $ modifyTVar' tsess $ \st ->
            st & serverRoomsL %~
              HM.insert rid
              RoomInfo
                { roomName = title
                , roomMembers = HS.singleton uid
                }
              & serverRoomPassesL %~
                  HM.insert rid hsh
          now <- getCurrentTime
          WS.sendBinaryData conn $ encode $
            RoomCreated now rid
          WS.sendBinaryData conn $ encode $
            JoinedRoom now rid uid

        Right (DiceRoll rid dice) -> do
          now <- getCurrentTime
          mans <- HM.lookup rid . serverRooms
            <$> readTVarIO tsess
          case mans of
            Nothing -> send conn $ RoomNotFound now rid
            Just RoomInfo{..}
              | uid `HS.member` roomMembers
              -> forM_ roomMembers $ \uid' -> do
                conns <- serverUserConns
                  <$> readTVarIO tsess
                forM_ (HM.lookup uid' conns) $ \c ->
                  send c $ DiceRolled now rid uid dice
              | otherwise -> send conn
                $ NotRoomMember now rid uid



unregisterUser
  :: TVar ServerState -> UserId -> WS.Connection -> IO ()
unregisterUser tses uid conn = do
  _rooms <- atomically $ do
    st@ServerState{..} <- readTVar tses
    writeTVar tses $
      st  & serverUserConnsL %~ HM.delete uid
          & serverRoomsL %~ HM.map (roomMembersL %~ HS.delete uid)
    pure $ HM.filter (HS.member uid . roomMembers)
        $ st ^. serverRoomsL
  -- TODO: Send @MemberLeft@ commands to remaining members
  now <- getCurrentTime
  WS.sendClose conn $ encode $ Bye now

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      state <- newTVarIO initServerState
      serve $ \case
        BackendRoute_Missing :=> Identity () -> return ()
        BackendRoute_Room :=> Identity () ->
          runWebSocketsSnap (application state)
  , _backend_routeEncoder = fullRouteEncoder
  }
