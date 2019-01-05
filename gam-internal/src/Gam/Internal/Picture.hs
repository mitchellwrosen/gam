module Gam.Internal.Picture where

import Gam.Internal.FontCache         (FontCache)
import Gam.Internal.FrameCount        (FrameCount)
import Gam.Internal.Prelude
import Gam.Internal.RenderedTextCache (RenderedTextCache)
import Gam.Internal.SpriteSheetCache  (SpriteSheetCache)
import Gam.Internal.TextStyle         (TextStyle(..))
import Gam.Internal.V                 (V)

import qualified Gam.Internal.RenderedTextCache as RenderedTextCache
import qualified Gam.Internal.Sprite            as Sprite
import qualified Gam.Internal.SpriteSheet       as SpriteSheet
import qualified Gam.Internal.SpriteSheetCache  as SpriteSheetCache
import qualified Gam.Internal.Texture           as Texture

import qualified SDL


data Picture
  = Alpha Float Picture
  | FlipX Picture
  | FlipY Picture
  | Rotate Float Picture
  | Scale (Float, Float) Picture
  | Sprite Sprite.Sprite
  | Textual TextStyle Text
  | Translate V Picture

data RenderOpts
  = RenderOpts
  { alpha :: Float
  , flipX :: Bool
  , flipY :: Bool
  , rotate :: Float
  , scaleX :: Float
  , scaleY :: Float
  , translate :: V
  } deriving stock (Generic)

render ::
     forall m r.
     ( HasType FontCache r
     , HasType (IORef FrameCount) r
     , HasType RenderedTextCache r
     , HasType SDL.Renderer r
     , HasType SpriteSheetCache r
     , MonadIO m
     , MonadReader r m
     )
  => Picture
  -> m ()
render =
  go RenderOpts
    { alpha = 1
    , flipX = False
    , flipY = False
    , rotate = 0
    , scaleX = 1
    , scaleY = 1
    , translate = 0
    }
  where
    go :: RenderOpts -> Picture -> m ()
    go opts = \case
      Alpha f pic ->
        go (opts & the @"alpha" %~ (* f)) pic

      FlipX pic ->
        go (opts & the @"flipX" %~ not) pic

      FlipY pic ->
        go (opts & the @"flipY" %~ not) pic

      Rotate n pic ->
        go (opts & the @"rotate" %~ (+ n)) pic

      Scale (x, y) pic ->
        go (opts & the @"scaleX" %~ (* x) & the @"scaleY" %~ (* y)) pic

      Sprite sprite ->
        renderSprite opts sprite

      Textual style text ->
        renderText opts style text

      Translate v pic ->
        go (opts & the @"translate" %~ (+ v)) pic

renderSprite ::
     ( HasType SDL.Renderer r
     , HasType SpriteSheetCache r
     , HasType (IORef FrameCount) r
     , MonadIO m
     , MonadReader r m
     )
  => RenderOpts
  -> Sprite.Sprite
  -> m ()
renderSprite
    RenderOpts { alpha, flipX, flipY, rotate, scaleX, scaleY, translate }
    sprite = do

  texture <-
    SpriteSheetCache.load (SpriteSheet.file (Sprite.sheet sprite))

  frameCount <-
    liftIO . readIORef =<< view (the @(IORef FrameCount))

  Texture.render
    (Texture.Opts
      { Texture.alpha = alpha
      , Texture.clip = Just (Sprite.rect frameCount sprite)
      , Texture.flipX = flipX
      , Texture.flipY = flipY
      , Texture.rotate = rotate
      , Texture.scale = (scaleX, scaleY)
      })
    translate
    texture

renderText ::
     ( HasType FontCache r
     , HasType RenderedTextCache r
     , HasType SDL.Renderer r
     , MonadIO m
     , MonadReader r m
     )
  => RenderOpts
  -> TextStyle
  -> Text
  -> m ()
renderText
    RenderOpts { alpha, flipX, flipY, rotate, scaleX, scaleY, translate } style
    text = do

  texture <-
    RenderedTextCache.load style text

  Texture.render
    (Texture.Opts
      { Texture.alpha = alpha
      , Texture.clip = Nothing
      , Texture.flipX = flipX
      , Texture.flipY = flipY
      , Texture.rotate = rotate
      , Texture.scale = (scaleX, scaleY)
      })
    translate
    texture
