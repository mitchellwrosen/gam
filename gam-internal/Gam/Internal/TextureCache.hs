module Gam.Internal.TextureCache where

import Gam.Internal.Prelude
import Gam.Internal.RGBA    (RGBA)
import Gam.Internal.Texture (Texture)

import qualified Gam.Internal.Texture as Texture

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text           as Text
import qualified SDL


-- T2 needs Hashable...
newtype TextureCache
  = TextureCache (IORef (HashMap (Text, Maybe RGBA) Texture))

new :: IO TextureCache
new =
  TextureCache <$> newIORef HashMap.empty

-- lookup + put
load :: TextureCache -> SDL.Renderer -> Text -> Maybe RGBA -> IO Texture
load cache renderer path transparent = do
  lookup cache path transparent >>= \case
    Nothing -> do
      texture <- Texture.load renderer (Text.unpack path) transparent
      put cache path transparent texture
      pure texture

    Just texture ->
      pure texture

lookup :: TextureCache -> Text -> Maybe RGBA -> IO (Maybe Texture)
lookup (TextureCache cacheRef) path transparent =
  HashMap.lookup (path, transparent) <$> readIORef cacheRef

put :: TextureCache -> Text -> Maybe RGBA -> Texture -> IO ()
put (TextureCache cacheRef) path transparent texture =
  modifyIORef' cacheRef (HashMap.insert (path, transparent) texture)
