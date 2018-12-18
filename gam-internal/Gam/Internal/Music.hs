module Gam.Internal.Music where

import Gam.Internal.Prelude


data Music
  = Music
  { file :: FilePath
  , volume :: Int
  }
