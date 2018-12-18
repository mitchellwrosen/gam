module Gam.Internal.Typeface where

import Gam.Internal.Prelude

import qualified SDL.Font


data Typeface
  = Typeface Bool Bool Bool Bool

instance Monoid Typeface where
  mempty = Typeface False False False False
  mappend = (<>)

instance Semigroup Typeface where
  Typeface a1 b1 c1 d1 <> Typeface a2 b2 c2 d2 =
    Typeface (a1 || a2) (b1 || b2) (c1 || c2) (d1 || d2)

bold :: Typeface
bold =
  Typeface True False False False

italic :: Typeface
italic =
  Typeface False True False False

strikethrough :: Typeface
strikethrough =
  Typeface False False True False

underline :: Typeface
underline =
  Typeface False False False True

fromStyles :: [SDL.Font.Style] -> Typeface
fromStyles =
  foldMap $ \case
    SDL.Font.Bold          -> bold
    SDL.Font.Italic        -> italic
    SDL.Font.Strikethrough -> strikethrough
    SDL.Font.Underline     -> underline

toStyles :: Typeface -> [SDL.Font.Style]
toStyles (Typeface bold italic strikethrough underline) =
  catMaybes
    [ SDL.Font.Bold          <$ guard bold
    , SDL.Font.Italic        <$ guard italic
    , SDL.Font.Strikethrough <$ guard strikethrough
    , SDL.Font.Underline     <$ guard underline
    ]
