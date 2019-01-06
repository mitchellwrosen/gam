module Internal.Form where

import Gam.Internal.P       (P)
import Gam.Internal.Prelude


data Opts
  = Opts
  { alpha :: Float
  , position :: P
  , rotation :: Float
  , scale :: (Float, Float)
  } deriving stock (Generic)
