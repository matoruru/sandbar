{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Sandbar.Core where

import Control.Monad (Functor, Monad)
import Control.Applicative (Applicative)
import Control.Monad.IO.Class (MonadIO(liftIO))
import RIO ((.), IO)
import Control.Monad.State (MonadState, StateT(runStateT))
import Control.Monad.Reader (MonadReader, ReaderT(runReaderT))
import Sandbar.Context (ContextR, ContextRW)

newtype SandbarIO a = SandbarIO (ReaderT ContextR (StateT ContextRW IO) a)
  deriving (Applicative, Functor, Monad, MonadIO, MonadState ContextRW, MonadReader ContextR)

runSandbarIO :: SandbarIO a -> ContextR -> ContextRW -> IO (a, ContextRW)
runSandbarIO (SandbarIO sio) = runStateT . runReaderT sio

io :: MonadIO m => IO a -> m a
io = liftIO
