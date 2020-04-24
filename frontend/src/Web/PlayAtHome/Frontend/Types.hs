{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DerivingStrategies #-}
{-# LANGUAGE ExtendedDefaultRules, GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE PatternSynonyms                                   #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Web.PlayAtHome.Frontend.Types where
import Web.PlayAtHome.Types

import           Control.Applicative
import           Control.Lens
import           Data.Aeson                  ()
import qualified Data.Aeson                  as A
import           Data.Promise
import           Data.Text                   (Text)
import           GHC.Generics                (Generic)
import           Language.Javascript.JSaddle
import           Type.Reflection             (Typeable)

newtype Auth0 = Auth0 { runAuth0  :: JSVal }
  deriving (Generic)
  deriving anyclass (ToJSVal, FromJSVal)

default (Text)

getAuth0 :: MonadJSM m => Auth0Config -> m (Promise Auth0)
getAuth0 cfg = liftJSM $
  Promise <$> jsg1 "createAuth0Client" (A.toJSON cfg)

instance MakeObject Auth0 where
  makeObject (Auth0 ob) = pure $ Object ob

isAuthenticated
  :: MonadJSM m => Auth0 -> m Bool
isAuthenticated auth0 = liftJSM $
  await =<< fromJSValUnchecked =<< (auth0 ^. js0 ("isAuthenticated" :: String))

getTokenSilently :: MonadJSM m => Auth0 -> m (Maybe Text)
getTokenSilently auth0 = liftJSM $
  awaitMaybe =<< fromJSValUnchecked
    =<< (auth0 ^. js0 ("getTokenSilently" :: String))

type ClientCmd = OneOf InitCmd PlayCmd
type ClientResp = OneOf InitEvent PlayEvent

newtype OneOf a b = OneOf { getOneOf :: Either a b }
  deriving (Read, Show, Eq, Ord, Generic)
  deriving newtype (Functor, Bifunctor, Foldable)

pattern L :: a -> OneOf a b
pattern L a = OneOf (Left a)

pattern R :: b -> OneOf a b
pattern R b = OneOf (Right b)

instance (A.FromJSON a, A.FromJSON b) => A.FromJSON (OneOf a b) where
  parseJSON ob = L <$> A.parseJSON ob <|> R <$> A.parseJSON ob

instance (A.ToJSON a, A.ToJSON b) => A.ToJSON (OneOf a b) where
  toJSON = either A.toJSON A.toJSON . getOneOf

data AuthState
  = NoAuthInfo
  | Unauthenticated { frontendAuth0 :: Auth0 }
  | Authenticated
      { frontendAuth0 :: Auth0
      , frontendJWT   :: Token
      }
    deriving (Show, Typeable)

instance Show Auth0 where
  showsPrec _ Auth0{} = showString "<Auth0>"
