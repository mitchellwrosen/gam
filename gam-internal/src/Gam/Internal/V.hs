module Gam.Internal.V where

import Gam.Internal.Prelude

import Linear ((*^))

import qualified Linear


newtype V
  = V_ (Linear.V2 Float)
  deriving stock (Show)
  deriving newtype (Num)

pattern V :: Float -> Float -> V
pattern V x y =
  V_ (Linear.V2 x y)
{-# COMPLETE V #-}

add :: V -> V -> V
add (V_ v) (V_ w) =
  V_ (v + w)

fromV2 :: Linear.V2 Float -> V
fromV2 =
  V_

maxQuadrance :: Float -> V -> V
maxQuadrance q v =
  if qq > q
    then scale (q / qq) v
    else v
  where
    qq = quadrance v

quadrance :: V -> Float
quadrance (V_ v) =
  Linear.quadrance v

scale :: Float -> V -> V
scale n (V_ v) =
  V_ (n *^ v)

toV2 :: V -> Linear.V2 Float
toV2 (V_ v) =
  v
