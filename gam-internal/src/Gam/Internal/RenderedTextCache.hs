module Gam.Internal.RenderedTextCache where

import Gam.Internal.FontCache (FontCache)
import Gam.Internal.Prelude
import Gam.Internal.TextStyle (TextStyle)
import Gam.Internal.Texture   (Texture)

import qualified Gam.Internal.FontCache as FontCache
import qualified Gam.Internal.RGBA      as RGBA
import qualified Gam.Internal.TextStyle as TextStyle
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
load style text =
  lookup style text >>=
    maybe (loadNew style text) pure

loadNew ::
     ( HasType FontCache r
     , HasType RenderedTextCache r
     , HasType SDL.Renderer r
     , MonadIO m
     , MonadReader r m
     )
  => TextStyle
  -> Text
  -> m Texture
loadNew style text = do
  font <-
    FontCache.load (TextStyle.font style) (TextStyle.size style)

  setKerning font
  setOutline font
  setStyle font

  texture <-
    Texture.fromSurface =<<
      render font

  put style text texture

  pure texture

  where
    setKerning font = do
      oldKerning <- SDL.Font.getKerning font
      let newKerning = TextStyle.kerning style
      when (oldKerning /= newKerning) (SDL.Font.setKerning font newKerning)

    setOutline font = do
      oldOutline <- SDL.Font.getOutline font
      let newOutline = TextStyle.outline style
      when (oldOutline /= newOutline) (SDL.Font.setOutline font newOutline)

    setStyle font = do
      oldStyles <- SDL.Font.getStyle font
      let newStyles = Typeface.toStyles (TextStyle.typeface style)
      when (oldStyles /= newStyles) (SDL.Font.setStyle font newStyles)

    render font =
      doRender font (RGBA.toV4 (TextStyle.color style)) text
      where
        doRender =
          if TextStyle.aliased style
            then SDL.Font.solid
            else SDL.Font.blended

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
