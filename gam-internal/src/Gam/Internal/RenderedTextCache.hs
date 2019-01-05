module Gam.Internal.RenderedTextCache where

import Gam.Internal.FontCache (FontCache)
import Gam.Internal.Prelude
import Gam.Internal.TextStyle (TextStyle(..))
import Gam.Internal.Texture   (Texture)

import qualified Gam.Internal.FontCache as FontCache
import qualified Gam.Internal.RGBA      as RGBA
import qualified Gam.Internal.Texture   as Texture
import qualified Gam.Internal.Typeface  as Typeface

import Data.Tuple.Strict

import qualified Data.HashMap.Strict as HashMap
import qualified SDL
import qualified SDL.Font


-- TODO Rendered text cache expiry
newtype RenderedTextCache
  = RenderedTextCache (IORef (HashMap (T2 TextStyle Text) Texture))

new :: IO RenderedTextCache
new =
  RenderedTextCache <$> newIORef HashMap.empty

load ::
     ( HasType FontCache r
     , HasType RenderedTextCache r
     , HasType SDL.Renderer r
     , MonadIO m
     , MonadReader r m
     )
  => TextStyle
  -> Text
  -> m Texture
load style@(TextStyle { aliased, color, font, kerning, outline, size, typeface })
    text =
  lookup style text >>= \case
    Nothing -> do
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

      put style text texture

      pure texture

    Just texture ->
      pure texture

lookup ::
     ( HasType RenderedTextCache r
     , MonadIO m
     , MonadReader r m
     )
  => TextStyle
  -> Text
  -> m (Maybe Texture)
lookup style text = do
  RenderedTextCache cacheRef <- view (the @RenderedTextCache)
  liftIO (HashMap.lookup (T2 style text) <$> readIORef cacheRef)

put ::
     ( HasType RenderedTextCache r
     , MonadIO m
     , MonadReader r m
     )
  => TextStyle
  -> Text
  -> Texture
  -> m ()
put style text texture = do
  RenderedTextCache cacheRef <- view (the @RenderedTextCache)
  liftIO (modifyIORef' cacheRef (HashMap.insert (T2 style text) texture))
