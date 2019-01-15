{-# LANGUAGE GADTs #-}
module Graphics.Cairo.Render where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Graphics.Cairo.HasStatus
import Graphics.Cairo.Types

data Render m a where
  Render :: MonadIO m => ReaderT Context m a -> Render m a

instance (MonadIO m) => Functor (Render m) where
  fmap f (Render render) = Render $ fmap f render

instance (MonadIO m) => Applicative (Render m) where
  pure = Render . pure
  (<*>) (Render f) (Render a) = Render $ f <*> a

instance (MonadIO m) => Monad (Render m) where
  (>>=) (Render x) f = Render $ ReaderT $ \ r -> do
    a <- runReaderT x r
    let Render b' = f a
    runReaderT b' r

instance (MonadIO m) => MonadIO (Render m) where
  liftIO ioa = Render $ liftIO ioa

runRender :: (MonadIO m) => Context -> Render m a -> m a
runRender context (Render render) = runReaderT render context

with :: (MonadIO m, HasStatus t) =>
        t -> (t -> Render IO a) -> Render m a
with sth f = Render $ ReaderT $ \r -> liftIO $
  use sth (\s ->
    let (Render x) = f s
    in runReaderT x r)
