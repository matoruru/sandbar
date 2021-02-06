module X11InfoR where

import RIO (Show)
import Graphics.X11 (ScreenNumber, Screen, Display, Window)

data X11InfoR = X11InfoR
  { display :: Display
  , rootWindow :: Window
  , screen :: Screen
  , screenNumber :: ScreenNumber
  } deriving (Show)
