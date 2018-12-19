module Gam.Internal.SpriteSheet where

import Gam.Internal.Prelude


data SpriteSheet
  = SpriteSheet
  { file        :: Text
  , spriteSize  :: (Int, Int)
  } deriving stock (Generic)
