module Gam.Internal.V where


import qualified Linear

data V a
  = V a a

fromV2 :: Linear.V2 a -> V a
fromV2 (Linear.V2 x y) =
  V x y

toV2 :: V a -> Linear.V2 a
toV2 (V x y) =
  Linear.V2 x y
