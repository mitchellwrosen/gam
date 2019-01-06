module Gam.Internal.V
  ( module Gam.Internal.V
  , module Internal.V
  ) where

import Gam.Internal.M              (M(..))
import Gam.Internal.Prelude
import Gam.Internal.Transformation (Transformation(..))
import Internal.V

import qualified Gam.Internal.M as M

import Data.Vector.Unboxed ((!))


data Semiplane
  = SameSemiplane
  | Perpendicular
  | OppositeSemiplane

-- | Do these vectors lie on the same line?
collinear :: V -> V -> Bool
collinear v1 v2 =
  d * d == quadrance v1 * quadrance v2
  where
    d = dot v1 v2

dot :: V -> V -> Float
dot (V x1 y1) (V x2 y2) =
  x1 * x2 + y1 * y2

flipY :: V -> V
flipY (V x y) =
  V x (-y)

fromRadians :: Float -> V
fromRadians r =
  V (cos r) (sin r)

lerp :: Float -> V -> V -> V
lerp p v1 v2 =
  scale p v1 + scale (1 - p) v2

magnitude :: V -> Float
magnitude =
  sqrt . quadrance

maxQuadrance :: Float -> V -> V
maxQuadrance q v =
  if qq > q
    then scale (q / qq) v
    else v
  where
    qq = quadrance v

normalize :: V -> V
normalize v =
  scale (1 / magnitude v) v

-- | Are these two vectors perpendicular?
perpendicular :: V -> V -> Bool
perpendicular v1 v2 =
  dot v1 v2 == 0

quadrance :: V -> Float
quadrance v =
  dot v v

rotate :: Float -> V -> V
rotate r (V x y) =
  V (c*x - s*y) (s*x + c*y)
  where
    c = cos r
    s = sin r

rotate90 :: V -> V
rotate90 (V x y) =
  V (-y) x

scale :: Float -> V -> V
scale n (V x y) =
  V (n * x) (n * y)

-- | Are two vectors pointing in the "same" direction (angle between < 90), are
-- they perpendicular, or are they pointing in the "opposite" direction (angle
-- between > 90)?
semiplane :: V -> V -> Semiplane
semiplane v1 v2 =
  case compare (dot v1 v2) 0 of
    LT -> OppositeSemiplane
    EQ -> Perpendicular
    GT -> SameSemiplane

toRadians :: V -> Float
toRadians (V x y) =
  atan2 y x

transform :: Transformation -> V -> V
transform (Transformation r s _)  (V x y) =
  V (x*(m!0) + y*(m!2)) (x*(m!1) + y*(m!3))
  where
    M m = M.rotate r <> M.scale s
