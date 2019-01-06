module Internal.Sprite where

import Gam.Internal.Prelude
import Gam.Internal.SpriteSheet (SpriteSheet)

data Sprite
  = Sprite
  { indices :: [(Int, Int)]
  , sheet :: SpriteSheet
  } deriving stock (Generic)
