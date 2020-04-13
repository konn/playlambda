{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, OverloadedStrings #-}
module Main where
import Obelisk.Frontend
import Obelisk.Route.Frontend
import Reflex.Dom
import Web.PlayAtHome.Frontend
import Web.PlayAtHome.Route

main :: IO ()
main = do
  let Right validFullEncoder = checkEncoder fullRouteEncoder
  run $ runFrontend validFullEncoder frontend
