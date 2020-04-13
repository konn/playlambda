{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, OverloadedStrings #-}
module Main where
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Text              (Text, pack)
import Reflex
import Reflex.Dom
import Reflex.Network
import System.Random

import Common.Route
import Frontend
import Obelisk.Frontend
import Obelisk.Route.Frontend
import Reflex.Dom

main :: IO ()
main = do
  let Right validFullEncoder = checkEncoder fullRouteEncoder
  run $ runFrontend validFullEncoder frontend
