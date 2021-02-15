module Sandbar.Context where

import RIO (Show)
import Sandbar.Config (Config)
import Sandbar.X11InfoRW (X11InfoRW)
import Sandbar.X11InfoR (X11InfoR)

data ContextRW = ContextRW
  { config :: Config
  , x11InfoRW :: X11InfoRW
  } deriving (Show)

newtype ContextR = ContextR
  { x11InfoR :: X11InfoR
  } deriving (Show)
