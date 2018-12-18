module Gam.Internal.Picture where

import Gam.Internal.FontCache        (FontCache)
import Gam.Internal.P                (P)
import Gam.Internal.Prelude
import Gam.Internal.SpriteSheet      (SpriteSheet(..))
import Gam.Internal.SpriteSheetCache (SpriteSheetCache)
import Gam.Internal.TextStyle        (TextStyle(..))
import Gam.Internal.V                (V)

import qualified Gam.Internal.FontCache        as FontCache
import qualified Gam.Internal.P                as P
import qualified Gam.Internal.RGBA             as RGBA
import qualified Gam.Internal.SpriteSheetCache as SpriteSheetCache
import qualified Gam.Internal.Texture          as Texture
import qualified Gam.Internal.Typeface         as Typeface

import qualified Linear
import qualified Linear.Affine as Linear
import qualified SDL
import qualified SDL.Font


data Picture
  = Alpha Float Picture
  | Append Picture Picture
  | Empty
  | FlipX Picture
  | FlipY Picture
  | Rotate Float Picture
  | Sprite SpriteSheet Int
  | Textual TextStyle Text
  | Translate V Picture

instance Monoid Picture where
  mempty = Empty
  mappend = (<>)

instance Semigroup Picture where
  Empty <> pic2 = pic2
  pic1 <> Empty = pic1
  pic1 <> pic2 = Append pic1 pic2

render ::
     forall m r.
     ( HasType FontCache r
     , HasType SDL.Renderer r
     , HasType SpriteSheetCache r
     , MonadReader r m
     , MonadUnliftIO m
     )
  => Picture
  -> m ()
render =
  go 0 0 False False 1
  where
    go :: P -> Float -> Bool -> Bool -> Float -> Picture -> m ()
    go !point !rotate !flipX !flipY !alpha = \case
      Alpha f pic ->
        go point rotate flipX flipY (alpha * f) pic

      Append pic1 pic2 -> do
        go point rotate flipX flipY alpha pic1
        go point rotate flipX flipY alpha pic2

      Empty ->
        pure ()

      FlipX pic ->
        go point rotate (not flipX) flipY alpha pic

      FlipY pic ->
        go point rotate flipX (not flipY) alpha pic

      Rotate n pic ->
        go point (n + rotate) flipX flipY alpha pic

      Sprite sheet which ->
        renderSprite (Texture.Opts alpha flipX flipY rotate) point sheet which

      Textual style text ->
        renderText (Texture.Opts alpha flipX flipY rotate) point style text

      Translate v pic ->
        go (P.add v point) rotate flipX flipY alpha pic

renderSprite ::
     ( HasType SDL.Renderer r
     , HasType SpriteSheetCache r
     , MonadReader r m
     , MonadUnliftIO m
     )
  => Texture.Opts
  -> P
  -> SpriteSheet
  -> Int
  -> m ()
renderSprite opts point sheet which = do
  texture <-
    SpriteSheetCache.load (sheet ^. the @"file") (sheet ^. the @"transparent")

  let
    srcRect :: SDL.Rectangle CInt
    srcRect =
      SDL.Rectangle
        (Linear.P (Linear.V2 (nx * sx) (ny * sy)))
        spriteV2

      where
        (ny, nx) =
          fromIntegral which `quotRem` (Texture.width texture `div` sx)

  Texture.render
    opts
    (Just srcRect)
    (Just dstRect)
    texture

  where
    (fromIntegral -> sx, fromIntegral -> sy) =
      sheet ^. the @"spriteSize"

    spriteV2 :: Linear.V2 CInt
    spriteV2 =
      Linear.V2 sx sy

    dstRect :: SDL.Rectangle CInt
    dstRect =
      SDL.Rectangle
        (round <$> P.unwrap point)
        spriteV2

renderText ::
     ( HasType FontCache r
     , HasType SDL.Renderer r
     , MonadReader r m
     , MonadUnliftIO m
     )
  => Texture.Opts
  -> P
  -> TextStyle
  -> Text
  -> m ()
renderText opts point (TextStyle { aliased, color, font, kerning, outline, size, typeface }) text = do
  font <-
    FontCache.load font size

  do
    oldKerning <- SDL.Font.getKerning font
    when (oldKerning /= kerning) (SDL.Font.setKerning font kerning)

  do
    oldOutline <- SDL.Font.getOutline font
    when (oldOutline /= outline) (SDL.Font.setOutline font outline)

  do
    let newStyles = Typeface.toStyles typeface
    oldStyles <- SDL.Font.getStyle font
    when (oldStyles /= newStyles) (SDL.Font.setStyle font newStyles)

  let
    createSurface =
      if aliased
        then SDL.Font.solid
        else SDL.Font.blended

  texture <-
    bracket
      (createSurface font (RGBA.toV4 color) text)
      SDL.freeSurface
      Texture.fromSurface

  let
    dstRect :: SDL.Rectangle CInt
    dstRect =
      SDL.Rectangle
        (round <$> P.unwrap point)
        (Linear.V2 (Texture.width texture) (Texture.height texture))

  Texture.render
    opts
    Nothing
    (Just dstRect)
    texture
