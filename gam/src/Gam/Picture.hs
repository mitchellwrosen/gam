module Gam.Picture
  ( Picture
  , sprite
  , alpha
  , scale
  , translate
  ) where

import Gam.Internal.Picture (Picture)
import Gam.Internal.Prelude
import Gam.Internal.Sprite  (Sprite)
import Gam.Internal.V       (V)

import qualified Gam.Internal.Picture as Picture


sprite :: Sprite -> Picture
sprite =
  Picture.Sprite

alpha :: Float -> Picture -> Picture
alpha =
  Picture.Alpha

scale :: (Float, Float) -> Picture -> Picture
scale =
  Picture.Scale

translate :: V -> Picture -> Picture
translate =
  Picture.Translate
