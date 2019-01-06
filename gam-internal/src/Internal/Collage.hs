module Internal.Collage where

import Gam.Internal.P         (P)
import Gam.Internal.Prelude
import Gam.Internal.RGB       (RGB)
import Gam.Internal.TextStyle (TextStyle)
import Gam.Internal.V         (V)
import Internal.Form          (Opts)

import qualified Internal.Sprite as Sprite

data Collage
  = Collage
  { position :: P
  , forms :: [(Opts, Form)]
  }

data Form
  = Rectangle RGB V
  | Sprite Sprite.Sprite
  | Textual TextStyle Text
