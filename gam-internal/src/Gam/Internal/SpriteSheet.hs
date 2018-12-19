module Gam.Internal.SpriteSheet where

import Gam.Internal.Prelude
import Gam.Internal.RGBA    (RGBA)
import Gam.Internal.V       (V)

data SpriteSheet
  = SpriteSheet
  { file        :: Text
  , spriteSize  :: (Int, Int)
  , transparent :: Maybe RGBA
  } deriving stock (Generic)
