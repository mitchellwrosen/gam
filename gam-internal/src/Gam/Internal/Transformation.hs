module Gam.Internal.Transformation where

import Gam.Internal.M
import Gam.Internal.Prelude
import Internal.V           (V(..))

import qualified Gam.Internal.M as M

data Transformation
  = Transformation Float Float V

instance Monoid Transformation where
  mempty = Transformation 0 1 0
  mappend = (<>)

instance Semigroup Transformation where
  Transformation r1 s1 t1 <> Transformation r2 s2 t2 =
    Transformation (r1 + r2) (s1 * s2) (t1 + t2)

invert :: Transformation -> Transformation
invert (Transformation r s t) =
  Transformation (-r) (1/s) (-t)

rotate :: Float -> Transformation
rotate r =
  Transformation r 1 0

scale :: Float -> Transformation
scale n =
  Transformation 0 n 0

toM :: Transformation -> M
toM (Transformation r s t) =
  M.rotate r <> M.scale s <> M.translate t

translate :: V -> Transformation
translate v =
  Transformation 0 1 v
