module Gam.Internal.SpriteSheetCache where

import Gam.Internal.Prelude
import Gam.Internal.RGBA    (RGBA)

import qualified Gam.Internal.Texture as Texture

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text           as Text
import qualified SDL


-- T2 needs Hashable...
newtype SpriteSheetCache
  = SpriteSheetCache (IORef (HashMap (Text, Maybe RGBA) SDL.Texture))

new :: IO SpriteSheetCache
new =
  SpriteSheetCache <$> newIORef HashMap.empty

-- lookup + put
load ::
     ( HasType SDL.Renderer r
     , HasType SpriteSheetCache r
     , MonadIO m
     , MonadReader r m
     )
  => Text
  -> Maybe RGBA
  -> m SDL.Texture
load path transparent =
  lookup path transparent >>= \case
    Nothing -> do
      texture <- Texture.load (Text.unpack path) transparent
      put path transparent texture
      pure texture

    Just texture ->
      pure texture

lookup ::
     (HasType SpriteSheetCache r, MonadReader r m, MonadIO m)
  => Text
  -> Maybe RGBA
  -> m (Maybe SDL.Texture)
lookup path transparent = do
  SpriteSheetCache cacheRef <- view (the @SpriteSheetCache)
  liftIO (HashMap.lookup (path, transparent) <$> readIORef cacheRef)

put ::
     (HasType SpriteSheetCache r, MonadReader r m, MonadIO m)
  => Text
  -> Maybe RGBA
  -> SDL.Texture
  -> m ()
put path transparent texture = do
  SpriteSheetCache cacheRef <- view (the @SpriteSheetCache)
  liftIO (modifyIORef' cacheRef (HashMap.insert (path, transparent) texture))
