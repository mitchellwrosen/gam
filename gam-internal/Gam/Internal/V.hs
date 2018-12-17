module Gam.Internal.V where

import Gam.Internal.Prelude

import Linear (V2(..))


newtype V
  = V_ (V2 Float)

pattern V :: Float -> Float -> V
pattern V x y =
  V_ (V2 x y)

add :: V -> V -> V
add (V_ v) (V_ w) =
  V_ (v + w)

fromV2 :: V2 Float -> V
fromV2 =
  V_

toV2 :: V -> V2 Float
toV2 (V_ v) =
  v
