module Internal.Collage where

import Gam.Internal.P         (P)
import Gam.Internal.Prelude
import Gam.Internal.TextStyle (TextStyle)
import Internal.Form          (Opts)

import qualified Internal.Sprite as Sprite

data Collage
  = Collage
  { position :: P
  , forms :: [(Opts, Form)]
  }

data Form
  = Sprite Sprite.Sprite
  | Textual TextStyle Text
