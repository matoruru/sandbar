{-# LANGUAGE TemplateHaskell #-}

module Config where

import Data.Aeson.TH (Options(rejectUnknownFields), defaultOptions, deriveJSON)
import RIO (Eq, Bool(True), Integer, String, Show)
import Graphics.X11 (Position)
import GHC.Word (Word32)

data Text = Text
  { text_name :: String
  , text_font :: String
  , text_background_color :: String
  , text_value :: String
  , text_color :: String
  , text_x_pos :: Integer
  , text_y_pos :: Integer
  } deriving (Show, Eq)

$(deriveJSON defaultOptions{ rejectUnknownFields = True } ''Text)

data Bar = Bar
  { bar_name :: String
  , bar_x_pos :: Position
  , bar_y_pos :: Position
  , bar_width :: Word32
  , bar_height :: Word32
  , bar_background_color :: String
  , text :: [Text]
  } deriving (Show, Eq)

$(deriveJSON defaultOptions{ rejectUnknownFields = True } ''Bar)

newtype Config = Config
  { bar :: Bar
  } deriving (Show, Eq)

-- Root should not have `rejectUnknownFields` in order to allow anchors.
$(deriveJSON defaultOptions ''Config)
