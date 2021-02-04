module Context where

import RIO (Show)
import Graphics.X11 (GC, Pixmap, Visual, Colormap, Screen, ScreenNumber, Display, Window)
import Config (Config)

data Context = Context
  { config :: Config
  , x11Info :: X11Info
  } deriving (Show)

data X11Info = X11Info
  { display :: Display
  , screenNumber :: ScreenNumber
  , rootWindow :: Window
  , colormap :: Colormap
  , visual :: Visual
  , screen :: Screen
  , window :: Window
  , pixmap :: Pixmap
  , gc :: GC
  , gc_clr :: GC
  } deriving (Show)
