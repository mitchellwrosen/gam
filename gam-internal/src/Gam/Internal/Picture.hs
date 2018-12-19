module Gam.Internal.Picture where

import Gam.Internal.FontCache         (FontCache)
import Gam.Internal.Prelude
import Gam.Internal.RenderedTextCache (RenderedTextCache)
import Gam.Internal.SpriteSheet       (SpriteSheet(..))
import Gam.Internal.SpriteSheetCache  (SpriteSheetCache)
import Gam.Internal.TextStyle         (TextStyle(..))
import Gam.Internal.V                 (V)

import qualified Gam.Internal.FontCache         as FontCache
import qualified Gam.Internal.RenderedTextCache as RenderedTextCache
import qualified Gam.Internal.RGBA              as RGBA
import qualified Gam.Internal.SpriteSheetCache  as SpriteSheetCache
import qualified Gam.Internal.Texture           as Texture
import qualified Gam.Internal.Typeface          as Typeface
import qualified Gam.Internal.V                 as V

import qualified Linear
import qualified Linear.Affine as Linear
import qualified SDL
import qualified SDL.Font


data Picture
  = Alpha Float Picture
  | FlipX Picture
  | FlipY Picture
  | Rotate Float Picture
  | Scale (Float, Float) Picture
  | Sprite SpriteSheet Int
  | Textual TextStyle Text
  | Translate V Picture

render ::
     forall m r.
     ( HasType FontCache r
     , HasType RenderedTextCache r
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

  SDL.TextureInfo _ _ width _ <-
    SDL.queryTexture texture

  let
    srcRect :: SDL.Rectangle CInt
    srcRect =
      SDL.Rectangle
        (Linear.P (Linear.V2 (nx * sx) (ny * sy)))
        (Linear.V2 sx sy)

      where
        (ny, nx) =
          fromIntegral which `quotRem` (width `div` sx)

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
     , HasType RenderedTextCache r
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
renderText opts (scaleX, scaleY) translate style text = do

  texture <-
    RenderedTextCache.load style text

  SDL.TextureInfo _ _ width height <-
    SDL.queryTexture texture

  let
    dstRect :: SDL.Rectangle CInt
    dstRect =
      SDL.Rectangle
        (round <$> Linear.P (V.toV2 translate))
        (Linear.V2
          (round (scaleX * fromIntegral width))
          (round (scaleY * fromIntegral height)))

  Texture.render
    opts
    Nothing
    (Just dstRect)
    texture
