module Gam.Internal.Sprite where

import Gam.Internal.FrameCount  (FrameCount(..))
import Gam.Internal.Prelude
import Gam.Internal.SpriteSheet (SpriteSheet)

import qualified Gam.Internal.SpriteSheet as SpriteSheet

import qualified SDL


data Sprite
  = Sprite
  { sheet :: SpriteSheet
  , indices :: [(Int, Int)]
  }

rect :: FrameCount -> Sprite -> SDL.Rectangle CInt
rect (FrameCount n) (Sprite { sheet, indices }) =
  SDL.Rectangle
    (SDL.P (SDL.V2 (fromIntegral (nx * sx)) (fromIntegral (ny * sy))))
    (SDL.V2 (fromIntegral sx) (fromIntegral sy))
  where
    (nx, ny) = indices !! (fromIntegral n `rem` length indices)
    (sx, sy) = SpriteSheet.spriteSize sheet
