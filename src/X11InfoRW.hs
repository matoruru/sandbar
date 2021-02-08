module X11InfoRW where

import RIO (Show)
import Graphics.X11 (GC, Visual, Colormap, Window)

data X11InfoRW = X11InfoRW
  { colormap :: Colormap
  , visual :: Visual
  , window :: Window
  , gc :: GC
  , gc_clr :: GC
  } deriving (Show)
