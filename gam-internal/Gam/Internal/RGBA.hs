module Gam.Internal.RGBA where

import Gam.Internal.Prelude

import Linear


data RGBA
  = RGBA Word8 Word8 Word8 Word8
  deriving stock (Eq, Generic)
  deriving anyclass (Hashable)

toV4 :: RGBA -> V4 Word8
toV4 (RGBA a b c d) =
  V4 a b c d
