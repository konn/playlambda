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

main :: IO ()
main = mainWidget $ el "div" $ do
  el "h1" $ text "Pλay At Home"
  diceRoller

diceRoller
  :: (MonadIO (PushM t), MonadHold t m, PostBuild t m, DomBuilder t m)
  => m ()
diceRoller = el "div" $ do
  btn <- constButtonM
          "Roll dice"
          (liftIO $ randomRIO (1 :: Int, 6))
  rand <- holdDyn Nothing (Just <$> btn)
  dynText $ maybe "" ((" " <>) . pack . show) <$> rand
  pure ()

constButtonM
  :: DomBuilder t m
  => Text -> PushM t a -> m (Event t a)
constButtonM tag act = do
  (e, _) <- elAttr' "button" ("type" =: "button") $ text tag
  return $ pushAlways (const act) (domEvent Click e)

