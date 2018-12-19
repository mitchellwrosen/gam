module Gam.Internal.P where

import Gam.Internal.Prelude hiding (subtract)
import Gam.Internal.V

import Linear.Affine ((.-.))

import qualified Linear
import qualified Linear.Affine as Linear


newtype P
  = P_ (Linear.Point Linear.V2 Float)
  deriving stock (Show)
  deriving newtype (Num)

pattern P :: Float -> Float -> P
pattern P x y =
  P_ (Linear.P (Linear.V2 x y))
{-# COMPLETE P #-}


add :: V -> P -> P
add (V vx vy) (P px py) =
  P (px + vx) (py + vy)

asV :: P -> V
asV (P_ (Linear.P p)) =
  V_ p

distance :: P -> P -> Float
distance p q =
  Linear.norm (toV2 (subtract p q))

subtract :: P -> P -> V
subtract (P_ p) (P_ q) =
  V_ (q .-. p)

unwrap :: P -> Linear.Point Linear.V2 Float
unwrap (P_ p) =
  p
