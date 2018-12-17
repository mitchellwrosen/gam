module Gam.Main.Tea.Sub
  ( Sub
  , micros
  ) where

import Gam.Internal.Prelude
import Internal.Sub

import qualified SDL


micros :: (Int -> msg) -> Sub msg
micros f =
  Sub
    (Just f)
    Nothing
    []
