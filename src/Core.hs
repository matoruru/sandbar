{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Core where

import Control.Monad (Functor, Monad)
import Control.Applicative (Applicative)
import Config (Config)
import Control.Monad.Reader (MonadReader, ReaderT)
import Control.Monad.IO.Class (MonadIO(liftIO))
import RIO (IO)

newtype SandbarIO a = SandbarIO (ReaderT Config IO a)
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config)

io :: MonadIO m => IO a -> m a
io = liftIO
