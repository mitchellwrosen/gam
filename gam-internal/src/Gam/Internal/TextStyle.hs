module Gam.Internal.TextStyle where

import Gam.Internal.Prelude
import Gam.Internal.RGBA     (RGBA)
import Gam.Internal.Typeface (Typeface)

data TextStyle
  = TextStyle
  { aliased :: Bool
  , color :: RGBA
  , font :: FilePath
  , kerning :: Bool
  , outline :: Int
  , size :: Int
  , typeface :: Typeface
  } deriving stock (Generic)
