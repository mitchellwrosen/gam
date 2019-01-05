module Gam.Internal.LineSegment where

import Gam.Internal.P (P)
import Gam.Internal.V (V)

-- | A line segment.
data LineSegment
  = LineSegment P V -- P + tV, where 0 <= t <= 1
