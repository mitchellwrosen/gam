module Gam.Internal.SpriteSheet where

import Gam.Internal.Prelude


data SpriteSheet
  = SpriteSheet
  { file        :: Hashed FilePath
  , spriteSize  :: (Int, Int)
  } deriving stock (Generic)
