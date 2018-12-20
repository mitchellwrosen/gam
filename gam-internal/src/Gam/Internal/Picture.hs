module Gam.Internal.Picture where

import Gam.Internal.FontCache         (FontCache)
import Gam.Internal.Prelude
import Gam.Internal.RenderedTextCache (RenderedTextCache)
import Gam.Internal.SpriteSheet       (SpriteSheet(..))
import Gam.Internal.SpriteSheetCache  (SpriteSheetCache)
import Gam.Internal.TextStyle         (TextStyle(..))
import Gam.Internal.V                 (V)

import qualified Gam.Internal.RenderedTextCache as RenderedTextCache
import qualified Gam.Internal.SpriteSheetCache  as SpriteSheetCache
import qualified Gam.Internal.Texture           as Texture
import qualified Gam.Internal.V                 as V

import qualified Linear
import qualified Linear.Affine as Linear
import qualified SDL


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
        renderSprite alpha flipX flipY rotate (scaleX, scaleY) translate sheet
          which

      Textual style text ->
        renderText alpha flipX flipY rotate (scaleX, scaleY) translate style
          text

      Translate v pic ->
        go alpha flipX flipY rotate scaleX scaleY (v + translate) pic

renderSprite ::
     ( HasType SDL.Renderer r
     , HasType SpriteSheetCache r
     , MonadIO m
     , MonadReader r m
     )
  => Float
  -> Bool
  -> Bool
  -> Float
  -> (Float, Float)
  -> V
  -> SpriteSheet
  -> Int
  -> m ()
renderSprite alpha flipX flipY rotate scale translate sheet which = do
  texture <-
    SpriteSheetCache.load (sheet ^. the @"file")

  let
    clip :: SDL.Rectangle CInt
    clip =
      SDL.Rectangle
        (Linear.P (Linear.V2 (nx * sx) (ny * sy)))
        (Linear.V2 sx sy)

      where
        (ny, nx) =
          fromIntegral which `quotRem` (Texture.width texture `div` sx)

  Texture.render
    (Texture.Opts
      { Texture.alpha = alpha
      , Texture.clip = Just clip
      , Texture.flipX = flipX
      , Texture.flipY = flipY
      , Texture.rotate = rotate
      , Texture.scale = scale
      })
    (round <$> SDL.P (V.toV2 translate))
    texture

  where
    (fromIntegral -> sx, fromIntegral -> sy) =
      sheet ^. the @"spriteSize"

renderText ::
     ( HasType FontCache r
     , HasType RenderedTextCache r
     , HasType SDL.Renderer r
     , MonadIO m
     , MonadReader r m
     )
  => Float
  -> Bool
  -> Bool
  -> Float
  -> (Float, Float)
  -> V
  -> TextStyle
  -> Text
  -> m ()
renderText alpha flipX flipY rotate scale translate style text = do
  texture <-
    RenderedTextCache.load style text

  Texture.render
    (Texture.Opts
      { Texture.alpha = alpha
      , Texture.clip = Nothing
      , Texture.flipX = flipX
      , Texture.flipY = flipY
      , Texture.rotate = rotate
      , Texture.scale = scale
      })
    (round <$> Linear.P (V.toV2 translate))
    texture
