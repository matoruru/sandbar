{-# LANGUAGE DeriveGeneric #-}

module Config where

import RIO (String, Show, Generic)
import Data.Yaml (FromJSON)
import Graphics.X11 (Position)
import GHC.Word (Word32)

newtype Config = Config
  { bar :: Bar
  } deriving (Show, Generic)

instance FromJSON Config

{-
 (x_pos,y_pos)----------------+  -+-
      |                       |   |
      |          bar          |   | height
      |                       |   |
      +-----------------------+  -+-
 
      |                       |
      +-----------------------+
      |         width         |

-}
data Bar = Bar
  { x_pos :: Position
  , y_pos :: Position
  , width :: Word32
  , height :: Word32
  , background_color :: String
  , font :: String
  , font_color :: String
  } deriving (Show, Generic)

instance FromJSON Bar
