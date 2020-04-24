module Data.Promise where
import Control.Concurrent.MVar
import Control.Lens
import Control.Monad               (void)
import Control.Monad.IO.Class
import Data.Maybe                  (fromJust)
import Language.Javascript.JSaddle

newtype Promise a = Promise { runPromise :: JSVal }

instance MakeObject (Promise a) where
  makeObject = makeObject . runPromise

instance ToJSVal a => ToJSVal (Promise a) where
  toJSVal = toJSVal . runPromise

instance FromJSVal (Promise a) where
  fromJSVal = fmap (fmap Promise) . fromJSVal

await :: (FromJSVal a) => Promise a -> JSM a
await jsv = do
  slot <- liftIO newEmptyMVar
  void $ jsv ^. js1 "then" (fun $ \_ _ [e] ->
    liftIO $ putMVar slot e
    )
  fmap fromJust . fromJSVal =<< liftIO (takeMVar slot)

awaitMaybe :: (FromJSVal a) => Promise a -> JSM (Maybe a)
awaitMaybe jsv = do
  slot <- liftIO newEmptyMVar
  void $ jsv ^. js2 "then"
    (fun $ \_ _ [e] ->
      liftIO $ putMVar slot (Just e)
    )
    (fun $ \_ _ _ ->
      liftIO $ putMVar slot Nothing
    )
  mapM fromJSValUnchecked =<< liftIO (takeMVar slot)
