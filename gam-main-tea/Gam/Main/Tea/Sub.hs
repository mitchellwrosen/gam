module Gam.Main.Tea.Sub
  ( Sub
  , micros
  ) where

import Gam.Internal.Prelude
import Internal.Sub


micros :: (Int -> msg) -> Sub msg
micros f =
  Sub
    (Just f)
    Nothing
    Nothing
