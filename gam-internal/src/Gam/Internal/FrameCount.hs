module Gam.Internal.FrameCount where

import Gam.Internal.Prelude


newtype FrameCount
  = FrameCount Int64
  deriving newtype (Num)
