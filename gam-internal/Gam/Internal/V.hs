module Gam.Internal.V where

import Gam.Internal.Prelude

import Linear (V2(..))


newtype V
  = V_ (V2 Float)

pattern V :: Float -> Float -> V
pattern V x y =
  V_ (V2 x y)

toV2 :: V -> V2 Float
toV2 (V_ v) =
  v
