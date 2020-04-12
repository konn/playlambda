{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Lens
import Reflex
import Reflex.Dom

main :: IO ()
main = mainWidget $ el "div" $ do
  el "h1" $ text "playlambda"
