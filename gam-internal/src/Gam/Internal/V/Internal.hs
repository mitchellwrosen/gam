module Gam.Internal.V.Internal where

import Gam.Internal.Prelude


data V
  = V
  { x :: Float
  , y :: Float
  } deriving stock (Show)

instance Num V where
  V x1 y1 + V x2 y2 = V (x1 + x2) (y1 + y2)
  V x1 y1 - V x2 y2 = V (x1 - x2) (y1 - y2)
  V x1 y1 * V x2 y2 = V (x1 * x2) (y1 * y2)
  negate (V x y) = V (negate x) (negate y)
  abs (V x y) = V (abs x) (abs y)
  signum (V x y) = V (signum x) (signum y)
  fromInteger n = V (fromInteger n) (fromInteger n)
