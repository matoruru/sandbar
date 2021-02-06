module X11InfoRW where

import RIO (Show)
import Graphics.X11 (GC, Pixmap, Visual, Colormap, Window)

data X11InfoRW = X11InfoRW
  { colormap :: Colormap
  , visual :: Visual
  , window :: Window
  , pixmap :: Pixmap
  , gc :: GC
  , gc_clr :: GC
  } deriving (Show)
