module Gam.Internal.Music where

import Gam.Internal.Prelude


data Music
  = Music
  { file :: Hashed FilePath
  , volume :: Int
  , fadeIn :: Int -- ^ Milliseconds.
  , fadeOut :: Int -- ^ Milliseconds.
  }
