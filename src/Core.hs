{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Core where

import Control.Monad (Functor, Monad)
import Control.Applicative (Applicative)
import Control.Monad.Reader (MonadReader, ReaderT(runReaderT))
import Control.Monad.IO.Class (MonadIO(liftIO))
import RIO (IO)
import Context (Context)

newtype SandbarIO a = SandbarIO (ReaderT Context IO a)
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader Context)

runSandbarIO :: SandbarIO a -> Context -> IO a
runSandbarIO (SandbarIO sio) = runReaderT sio

io :: MonadIO m => IO a -> m a
io = liftIO
