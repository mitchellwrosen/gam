module Gam.Internal.P where

import Gam.Internal.Prelude
import Gam.Internal.V (V, pattern V)

import qualified Linear
import qualified Linear.Affine as Linear


newtype P
  = P_ (Linear.Point Linear.V2 Float)
  deriving newtype (Num)

pattern P :: Float -> Float -> P
pattern P x y =
  P_ (Linear.P (Linear.V2 x y))


add :: V -> P -> P
add (V vx vy) (P px py) =
  P (px + vx) (py + vy)

unwrap :: P -> Linear.Point Linear.V2 Float
unwrap (P_ p) =
  p
