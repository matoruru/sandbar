{-# LANGUAGE DeriveGeneric #-}

module Config where

import RIO (Show, Generic)
import Data.Yaml (FromJSON)
import Graphics.X11 (Position)
import GHC.Word (Word32)

newtype Config = Config
  { bar :: Bar
  } deriving (Show, Generic)

instance FromJSON Config


{-
  (x,y)----------------------+  -+-
     |                       |   |
     |          Bar          |   | height
     |                       |   |
     +-----------------------+  -+-

     |                       |
     +-----------------------+
     |         Width         |

-}
data Bar = Bar
  { x_pos :: Position
  , y_pos :: Position
  , width :: Word32
  , height :: Word32
  } deriving (Show, Generic)

instance FromJSON Bar
