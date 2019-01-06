module Gam.Internal.Config where

import Gam.Internal.Prelude


data Config
  = Config
  { size :: (Int, Int)
  , title :: Text
  }
