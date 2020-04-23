{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
module Data.Promise where
import Control.Concurrent.MVar
import Control.Lens
import Control.Monad               (void)
import Control.Monad.IO.Class
import Data.Maybe                  (fromJust)
import Language.Javascript.JSaddle

newtype Promise a = Promise { runPromise :: a }
  deriving (Read, Show, Eq, Ord)

instance MakeObject a => MakeObject (Promise a) where
  makeObject = makeObject . runPromise

instance ToJSVal a => ToJSVal (Promise a) where
  toJSVal = toJSVal . runPromise

instance FromJSVal a => FromJSVal (Promise a) where
  fromJSVal = fmap (fmap Promise) . fromJSVal @a

await :: (MakeObject a, FromJSVal a) => Promise a -> JSM a
await (Promise jsv) = do
  slot <- liftIO newEmptyMVar
  void $ jsv ^. js1 "then" (fun $ \_ _ [e] ->
    liftIO $ putMVar slot e
    )
  fmap fromJust . fromJSVal =<< liftIO (takeMVar slot)
