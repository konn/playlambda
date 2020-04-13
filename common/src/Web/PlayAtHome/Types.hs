{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell       #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Web.PlayAtHome.Types
  ( PlayCmd(..), PlayEvent(..)
  , InitCmd(..), InitEvent(..)
  , isValidPassword, hashPassword
  , Password(..), UserId(..), RoomId(..)
  , Dice(..), Card(..), Deck(..)
  , PassHash(..)
  , RoomInfo(..), roomMembersL, roomNameL
  ) where
import           Control.Lens
import           Crypto.PasswordStore
import           Data.Aeson
import           Data.ByteString       (ByteString)
import           Data.Hashable
import qualified Data.HashSet          as HS
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
  , roomMembers :: HS.HashSet UserId
  }
  deriving (Read, Show, Eq, Ord, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON)

makeLensesWith
  (lensRules &  lensField .~ mappingNamer (pure . (++ "L")))
  ''RoomInfo

data InitCmd
  = LogIn !UserId !Password
  | CreateUser !UserId !Password
    deriving (Read, Show, Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON)

data PlayCmd
  = CreateRoom !Text !Password
  | JoinRoom !RoomId !Password
  | DiceRoll !RoomId !Dice
    deriving (Read, Show, Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON)

data InitEvent
  = LogInSuccess !UTCTime !UserId
  | UserAlreadyExists !UTCTime !UserId
  | LogInFailed !UTCTime !UserId
    deriving (Read, Show, Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON)

data PlayEvent
  = JoinedRoom !UTCTime !RoomId !UserId
  | YouJoinedRoom !UTCTime !RoomId !RoomInfo
  | DiceRolled !UTCTime !RoomId !UserId !Dice
  | MemberLeft !UTCTime !UserId !RoomId
  | Bye !UTCTime
  | InvalidCommand String
  | RoomCreated !UTCTime !RoomId
  | RoomNotFound !UTCTime !RoomId
  | NotRoomMember !UTCTime !RoomId !UserId
  | JoinFailed !UTCTime !RoomId
    deriving (Read, Show, Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON)

newtype Card  = Card { getCard :: Text }
  deriving (Read, Show, Eq, Ord, Generic)
  deriving newtype (FromJSON, ToJSON)

newtype Deck  = Deck { getDeck :: Vector Card }
  deriving (Read, Show, Eq, Ord, Generic)
