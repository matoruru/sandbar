module Context where

import RIO (Show)
import Config (Config)
import X11InfoRW (X11InfoRW)
import X11InfoR (X11InfoR)

data ContextRW = ContextRW
  { config :: Config
  , x11InfoRW :: X11InfoRW
  } deriving (Show)

newtype ContextR = ContextR
  { x11InfoR :: X11InfoR
  } deriving (Show)
