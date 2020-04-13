{-# LANGUAGE DataKinds, FlexibleContexts, OverloadedStrings #-}
{-# LANGUAGE TypeApplications                               #-}
module Web.PlayAtHome.Frontend where

import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Text                   (Text, pack)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import           Language.Javascript.JSaddle (eval, liftJSM)
import           Reflex
import           Reflex.Dom.Core
import           System.Random

import Obelisk.Configs
import Obelisk.Frontend
import Obelisk.Generated.Static
import Obelisk.Route

import Web.PlayAtHome.Route
import Web.PlayAtHome.Types

diceRoller
  :: (MonadIO (Performable m),
      MonadHold t m, PostBuild t m,
      PerformEvent t m,
      DomBuilder t m)
  => m ()
diceRoller = el "div" $ do
  btn <- constButtonM
          "Roll dice"
          (liftIO $ randomRIO (1 :: Int, 6))
  rand <- holdDyn Nothing (Just <$> btn)
  dynText $ maybe "" ((" " <>) . pack . show) <$> rand
  pure ()

constButtonM
  :: (DomBuilder t m, PerformEvent t m)
  => Text -> Performable m a -> m (Event t a)
constButtonM tag act = do
  (e, _) <- elAttr' "button" ("type" =: "button") $ text tag
  performEvent $ pushAlways (const $ pure act) (domEvent Click e)



-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Pλay At Home"
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do
      el "h1" $ text "Pλay At Home"

      -- `prerender` and `prerender_` let you choose a widget to run on the server
      -- during prerendering and a different widget to run on the client with
      -- JavaScript. The following will generate a `blank` widget on the server and
      -- print "Hello, World!" on the client.
      prerender_ blank $ liftJSM $ void $ eval ("console.log('Hello, World!')" :: T.Text)

      elAttr "img" ("src" =: static @"obelisk.jpg") blank
      el "div" $ do
        exampleConfig <- getConfig "common/example"
        case exampleConfig of
          Nothing -> text "No config file found in config/common/example"
          Just s  -> text $ T.decodeUtf8 s
      diceRoller
      return ()
  }
