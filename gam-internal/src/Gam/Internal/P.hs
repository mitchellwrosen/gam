module Gam.Internal.P where

import Gam.Internal.M              (M(..))
import Gam.Internal.Prelude        hiding (subtract)
import Gam.Internal.Transformation (Transformation)
import Gam.Internal.V              (V(..))

import qualified Gam.Internal.Transformation as Transformation
import qualified Gam.Internal.V              as V

import Data.Vector.Unboxed ((!))


data P
  = P
  { x :: Float
  , y :: Float
  } deriving stock (Show)

add :: V -> P -> P
add (V vx vy) (P px py) =
  P (px + vx) (py + vy)

asV :: P -> V
asV (P x y) =
  V x y

distance :: P -> P -> Float
distance p q =
  V.magnitude (subtract p q)

distanceSquared :: P -> P -> Float
distanceSquared p q =
  V.quadrance (subtract p q)

max :: P -> P -> P
max (P x1 y1) (P x2 y2) =
  P (Gam.Internal.Prelude.max x1 x2) (Gam.Internal.Prelude.max y1 y2)

min :: P -> P -> P
min (P x1 y1) (P x2 y2) =
  P (Gam.Internal.Prelude.min x1 x2) (Gam.Internal.Prelude.min y1 y2)

minus :: P -> P -> V
minus =
  flip subtract

subtract :: P -> P -> V
subtract (P x1 y1) (P x2 y2) =
  V (x2 - x1) (y2 - y1)

transform :: Transformation -> P -> P
transform t (P x y) =
  P (x*(m!0) + y*(m!2) + (m!4)) (x*(m!1) + y*(m!3) + (m!5))
  where
    M m = Transformation.toM t
