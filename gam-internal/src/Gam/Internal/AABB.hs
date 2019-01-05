module Gam.Internal.AABB where

import Gam.Internal.Prelude
import Gam.Internal.P (P(..))


data AABB
  = AABB
  { xmin :: Float
  , xmax :: Float
  , ymin :: Float
  , ymax :: Float
  }

contains :: P -> AABB -> Bool
contains (P x y) (AABB xmin xmax ymin ymax) =
  x >= xmin &&
  x <= xmax &&
  y >= ymin &&
  y <= ymax

intersects :: AABB -> AABB -> Bool
intersects (AABB xmin1 xmax1 ymin1 ymax1) (AABB xmin2 xmax2 ymin2 ymax2) =
  xmin1 <= xmax2 &&
  xmin2 <= xmax1 &&
  ymin1 <= ymax2 &&
  ymin2 <= ymax1
