{-# LANGUAGE DeriveGeneric #-}

module Config where

import RIO (Integer, String, Show, Generic)
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
  , text :: String
  , font :: String
  , font_color :: String
  , font_x_pos :: Integer
  , font_y_pos :: Integer
  , rectangle :: Rectangle
  } deriving (Show, Generic)

instance FromJSON Bar

--data Font = Font
--  { name :: String
--  , color :: String
--  , x_pos :: Integer
--  , y_pos :: Integer
--  }

data Rectangle = Rectangle
  { rectangle_x_pos :: Position
  , rectangle_y_pos :: Position
  , rectangle_width :: Word32
  , rectangle_height :: Word32
  , rectangle_color :: String
  } deriving (Show, Generic)

instance FromJSON Rectangle
