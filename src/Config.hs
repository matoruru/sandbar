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
  { bar_name :: String
  , bar_x_pos :: Position
  , bar_y_pos :: Position
  , bar_width :: Word32
  , bar_height :: Word32
  , bar_background_color :: String
  , text :: [Text]
  , rectangle :: [Rectangle]
  } deriving (Show, Generic)

instance FromJSON Bar

data Text = Text
  { text_name :: String
  , text_font :: String
  , text_background_color :: String
  , text_value :: String
  , text_color :: String
  , text_x_pos :: Integer
  , text_y_pos :: Integer
  } deriving (Show, Generic)

instance FromJSON Text

data Rectangle = Rectangle
  { rectangle_name :: String
  , rectangle_x_pos :: Position
  , rectangle_y_pos :: Position
  , rectangle_width :: Word32
  , rectangle_height :: Word32
  , rectangle_color :: String
  } deriving (Show, Generic)

instance FromJSON Rectangle
