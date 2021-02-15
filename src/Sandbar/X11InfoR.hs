module Sandbar.X11InfoR where

import RIO (Show)
import Graphics.X11 (Pixel, Colormap, ScreenNumber, Screen, Display, Window)
import Sandbar.Types (ColorName)

data X11InfoR = X11InfoR
  { display :: Display
  , rootWindow :: Window
  , screen :: Screen
  , screenNumber :: ScreenNumber
  , colormap :: Colormap
  , defaultColorName :: ColorName
  , defaultColor :: Pixel
  } deriving (Show)
