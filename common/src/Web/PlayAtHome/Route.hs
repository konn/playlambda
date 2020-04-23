{-# LANGUAGE EmptyCase, FlexibleContexts, FlexibleInstances, GADTs        #-}
{-# LANGUAGE KindSignatures, LambdaCase, MultiParamTypeClasses            #-}
{-# LANGUAGE OverloadedStrings, RankNTypes, TemplateHaskell, TypeFamilies #-}
module Web.PlayAtHome.Route where

{- -- You will probably want these imports for composing Encoders.
import Prelude hiding (id, (.))
import Control.Category
-}

import Data.Functor.Identity
import Data.Text             (Text)

import qualified Control.Category as Cat
import           Data.Map         (Map)
import           Obelisk.Route
import           Obelisk.Route.TH

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing :: BackendRoute ()
  BackendRoute_Room    :: BackendRoute ()

data FrontendRoute :: * -> * where
  FrontendRoute_Main :: FrontendRoute (Map Text (Maybe Text))

fullRouteEncoder
  :: Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
fullRouteEncoder = mkFullRouteEncoder
  (FullRoute_Backend BackendRoute_Missing :/ ())
  (\case
      BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
      BackendRoute_Room -> PathSegment "room" $ unitEncoder mempty
  )
  (\case
      FrontendRoute_Main -> PathEnd Cat.id)

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  ]
