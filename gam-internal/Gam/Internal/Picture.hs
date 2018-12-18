module Gam.Internal.Picture where

import Gam.Internal.FontCache        (FontCache)
import Gam.Internal.Prelude
import Gam.Internal.SpriteSheet      (SpriteSheet(..))
import Gam.Internal.SpriteSheetCache (SpriteSheetCache)
import Gam.Internal.TextStyle        (TextStyle(..))
import Gam.Internal.V                (V)

import qualified Gam.Internal.FontCache        as FontCache
import qualified Gam.Internal.RGBA             as RGBA
import qualified Gam.Internal.SpriteSheetCache as SpriteSheetCache
import qualified Gam.Internal.Texture          as Texture
import qualified Gam.Internal.Typeface         as Typeface
import qualified Gam.Internal.V                as V

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
  | Scale (Float, Float) Picture
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
     , MonadIO m
     , MonadReader r m
     )
  => Picture
  -> m ()
render =
  go 1 False False 0 1 1 0
  where
    go ::
         Float
      -> Bool
      -> Bool
      -> Float
      -> Float
      -> Float
      -> V
      -> Picture
      -> m ()
    go !alpha !flipX !flipY !rotate !scaleX !scaleY !translate = \case
      Alpha f pic ->
        go (alpha * f) flipX flipY rotate scaleX scaleY translate pic

      Append pic1 pic2 -> do
        go alpha flipX flipY rotate scaleX scaleY translate pic1
        go alpha flipX flipY rotate scaleX scaleY translate pic2

      Empty ->
        pure ()

      FlipX pic ->
        go alpha (not flipX) flipY rotate scaleX scaleY translate pic

      FlipY pic ->
        go alpha flipX (not flipY) rotate scaleX scaleY translate pic

      Rotate n pic ->
        go alpha flipX flipY (n + rotate) scaleX scaleY translate pic

      Scale (x, y) pic ->
        go alpha flipX flipY rotate (x * scaleX) (y * scaleY) translate pic

      Sprite sheet which ->
        renderSprite (Texture.Opts alpha flipX flipY rotate) (scaleX, scaleY) translate sheet which

      Textual style text ->
        renderText (Texture.Opts alpha flipX flipY rotate) (scaleX, scaleY) translate style text

      Translate v pic ->
        go alpha flipX flipY rotate scaleX scaleY (v + translate) pic

renderSprite ::
     ( HasType SDL.Renderer r
     , HasType SpriteSheetCache r
     , MonadIO m
     , MonadReader r m
     )
  => Texture.Opts
  -> (Float, Float)
  -> V
  -> SpriteSheet
  -> Int
  -> m ()
renderSprite opts (scaleX, scaleY) translate sheet which = do
  texture <-
    SpriteSheetCache.load (sheet ^. the @"file") (sheet ^. the @"transparent")

  let
    srcRect :: SDL.Rectangle CInt
    srcRect =
      SDL.Rectangle
        (Linear.P (Linear.V2 (nx * sx) (ny * sy)))
        (Linear.V2 sx sy)

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

    dstRect :: SDL.Rectangle CInt
    dstRect =
      SDL.Rectangle
        (round <$> Linear.P (V.toV2 translate))
        (Linear.V2
          (round (scaleX * fromIntegral sx))
          (round (scaleY * fromIntegral sy)))

renderText ::
     ( HasType FontCache r
     , HasType SDL.Renderer r
     , MonadIO m
     , MonadReader r m
     )
  => Texture.Opts
  -> (Float, Float)
  -> V
  -> TextStyle
  -> Text
  -> m ()
renderText opts (scaleX, scaleY) translate
    (TextStyle { aliased, color, font, kerning, outline, size, typeface })
    text = do

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


  surface <-
    if aliased
      then SDL.Font.solid   font (RGBA.toV4 color) text
      else SDL.Font.blended font (RGBA.toV4 color) text

  texture <-
    Texture.fromSurface surface

  SDL.freeSurface surface

  let
    dstRect :: SDL.Rectangle CInt
    dstRect =
      SDL.Rectangle
        (round <$> Linear.P (V.toV2 translate))
        (Linear.V2
          (round (scaleX * fromIntegral (Texture.width texture)))
          (round (scaleY * fromIntegral (Texture.height texture))))

  Texture.render
    opts
    Nothing
    (Just dstRect)
    texture
