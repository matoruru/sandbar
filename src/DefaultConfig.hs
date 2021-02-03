module DefaultConfig where

import Config ( Bar(..), Config(..) )

defaultConfig :: Config
defaultConfig = Config
  { bar = Bar
    { x_pos = 0
    , y_pos = 0
    , width = 1920
    , height = 20
    }
  }
