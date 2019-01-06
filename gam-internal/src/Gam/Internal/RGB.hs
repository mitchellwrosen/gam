module Gam.Internal.RGB where

import Gam.Internal.Prelude

data RGB
  = RGB Word8 Word8 Word8
  deriving stock (Eq, Generic)
  deriving anyclass (Hashable)
