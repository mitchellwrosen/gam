module Gam.Internal.Line where

import Gam.Internal.P (P)
import Gam.Internal.V (V)

-- | An infinite line.
data Line
  = Line P V -- P + tV, where V is a unit vector, -∞ <= t <= ∞
