module Gam.Internal.Circle where

import Gam.Internal.P       (P)
import Gam.Internal.Prelude


data Circle
  = Circle
  { center :: P
  , radius :: Float
  }
