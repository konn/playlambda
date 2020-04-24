{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell       #-}
{-# OPTIONS_GHC -funbox-strict-fields -Wall #-}
module Web.PlayAtHome.Types
  ( PlayCmd(..), PlayEvent(..), peRoomId,
    _JoinedRoom, _YouJoinedRoom, _DiceRolled,
    _MemberLeft, _Bye, _InvalidCommand, _RoomCreated,
    _RoomNotFound, _NotRoomMember, _JoinFailed
  , InitCmd(..), InitEvent(..)
  , isValidPassword, hashPassword
  , Password(..), UserId(..), RoomId(..)
  , Dice(..), Card(..), Deck(..)
  , PassHash(..), Auth0Config(..), Token
  , domainL, clientIdL, audienceL
  , RoomInfo(..), roomMembersL, roomNameL, roomIdL
  ) where
import           Control.Lens
import           Crypto.PasswordStore
import           Data.Aeson
import           Data.ByteString       (ByteString)
import           Data.Hashable
import qualified Data.Set              as Set
import           Data.String
import           Data.Text             (Text)
import           Data.Text.Encoding    (encodeUtf8)
import           Data.Time
import           Data.UUID
import           Data.Vector           (Vector)
import           Data.Vector.Instances ()
import           GHC.Generics

newtype Password = Password { getPassword :: Text }
  deriving (Read, Show, Eq, Ord)
  deriving newtype (Hashable, IsString, FromJSON, ToJSON)

isValidPassword :: PassHash -> Password -> Bool
isValidPassword (PassHash hsh) (Password pw) =
  verifyPassword (encodeUtf8 pw) hsh

hashPassword :: Password -> Int -> IO PassHash
hashPassword (Password pw) =
  fmap PassHash . makePassword (encodeUtf8 pw)

newtype PassHash = PassHash { runPassHash :: ByteString }
  deriving (Read, Show, Eq, Ord)
  deriving newtype (Hashable, IsString)

newtype UserId = UserId { runUserId :: Text }
  deriving (Read, Show, Eq, Ord)
  deriving newtype (Hashable, IsString, FromJSON, ToJSON)

newtype RoomId = RoomId { getRoomId :: UUID }
  deriving (Read, Show, Eq, Ord)
  deriving newtype (Hashable)

instance ToJSON RoomId where
  toJSON = toJSON . toText . getRoomId

instance FromJSON RoomId where
  parseJSON = withText "uuid" $
    maybe (fail "invalid UUID") (pure . RoomId) . fromText

data Dice =
  Dice
    { diceRolled :: !Int
    , diceMax    :: !Int
    }
    deriving (Read, Show, Eq, Ord, Generic)
    deriving anyclass (Hashable, ToJSON, FromJSON)

data RoomInfo = RoomInfo
  { roomName    :: !Text
  , roomMembers :: Set.Set UserId
  , roomId      :: !RoomId
  }
  deriving (Read, Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

makeLensesWith
  (lensRules &  lensField .~ mappingNamer (pure . (++ "L")))
  ''RoomInfo

type Token = Text

newtype InitCmd
  = LogIn
    { jwtToken :: Token
    }
    deriving (Read, Show, Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON)

data PlayCmd
  = CreateRoom !Text !Password
  | JoinRoom !RoomId !Password
  | DiceRoll !RoomId !Dice
    deriving (Read, Show, Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON)

data InitEvent
  = Welcome
  | LogInSuccess !UTCTime !UserId
  | LogInFailed !UTCTime !Text
    deriving (Read, Show, Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON)

data PlayEvent
  = JoinedRoom !UTCTime !RoomId !UserId
  | YouJoinedRoom !UTCTime !RoomId !RoomInfo
  | DiceRolled !UTCTime !RoomId !UserId !Dice
  | MemberLeft !UTCTime !UserId !RoomId
  | Bye !UTCTime
  | InvalidCommand !UTCTime String
  | RoomCreated !UTCTime !RoomId !RoomInfo
  | RoomNotFound !UTCTime !RoomId
  | NotRoomMember !UTCTime !RoomId !UserId
  | JoinFailed !UTCTime !RoomId
    deriving (Read, Show, Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON)

peRoomId :: PlayEvent -> Maybe RoomId
peRoomId (JoinedRoom _ rid _)    = Just rid
peRoomId (YouJoinedRoom _ rid _) = Just rid
peRoomId (DiceRolled _ rid _ _)  = Just rid
peRoomId (MemberLeft _ _ rid)    = Just rid
peRoomId (RoomCreated _ rid _)   = Just rid
peRoomId (RoomNotFound _ rid)    = Just rid
peRoomId (JoinFailed _ rid)      = Just rid
peRoomId (NotRoomMember _ rid _) = Just rid
peRoomId _                       = Nothing

makePrisms ''PlayEvent

newtype Card  = Card { getCard :: Text }
  deriving (Read, Show, Eq, Ord, Generic)
  deriving newtype (FromJSON, ToJSON)

newtype Deck  = Deck { getDeck :: Vector Card }
  deriving (Read, Show, Eq, Ord, Generic)

data Auth0Config =
  Auth0Config
    { domain   :: Text
    , clientId :: Text
    , audience :: Maybe Text
    }
    deriving (Read, Show, Eq, Ord, Generic)

makeLensesWith
  (lensRules & lensField .~ mappingNamer (pure . (++ "L")) )
  ''Auth0Config

auth0Opts :: Options
auth0Opts = defaultOptions
  { fieldLabelModifier = camelTo2 '_'
  , omitNothingFields = True
  }

instance ToJSON Auth0Config where
  toJSON = genericToJSON auth0Opts

instance FromJSON Auth0Config where
  parseJSON = genericParseJSON auth0Opts
