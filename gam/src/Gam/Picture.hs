module Gam.Picture
  ( Picture
  , sprite
  , translate
  ) where

import Gam.Internal.Picture     (Picture(..))
import Gam.Internal.Prelude
import Gam.Internal.SpriteSheet (SpriteSheet)
import Gam.Internal.V           (V)


sprite :: SpriteSheet -> Int -> Picture
sprite =
  Sprite

translate :: V -> Picture -> Picture
translate =
  Translate
