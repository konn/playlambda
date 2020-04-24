{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell       #-}
module Web.PlayAtHome.Backend.Types where
import           Control.Lens
import           Data.HashMap.Strict (HashMap)
import           GHC.Generics
import qualified Network.WebSockets  as WS
import           RIO                 hiding (lens)

import Web.PlayAtHome.Types

data User =
  User
    { userPassword :: Password
    , userConn     :: WS.Connection
    }
  deriving (Generic)

initServerState :: ServerState
initServerState =
  ServerState mempty mempty mempty mempty

data ServerEnv =
  ServerEnv
    { auth0Config :: Auth0Config
    , jwkUrl      :: String
    , logFunction :: LogFunc
    }

instance HasLogFunc ServerEnv where
  logFuncL = lens logFunction $ \a b -> a{logFunction = b}

-- | To be honest, i really want to use stm-containers,
--   but nix won't allow me to do so...
data ServerState =
  ServerState
    { serverUserPasses
      :: HashMap UserId PassHash
    , serverUserConns
      :: HashMap UserId WS.Connection
    , serverRooms
      :: HashMap RoomId RoomInfo
    , serverRoomPasses
      :: HashMap RoomId PassHash
    }
    deriving (Generic)

makeLensesWith
  (lensRules & lensField .~ mappingNamer (pure . (++ "L")) )
  ''ServerState

makeLensesWith
  (lensRules & lensField .~ mappingNamer (pure . (++ "L")) )
  ''ServerEnv


newtype AuthError = InvalidAccessToken Token
  deriving (Typeable, Show)
  deriving anyclass (Exception)

