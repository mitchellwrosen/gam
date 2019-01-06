module Gam.Internal.SpriteSheetCache where

import Gam.Internal.Prelude
import Gam.Internal.Texture (Texture)

import qualified Gam.Internal.Texture as Texture

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text           as Text
import qualified SDL


newtype SpriteSheetCache
  = SpriteSheetCache (IORef (HashMap (Hashed FilePath) Texture))

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
  => Hashed FilePath
  -> m Texture
load path =
  lookup path >>= \case
    Nothing -> do
      texture <- Texture.fromImageFile (unhashed path)
      put path texture
      pure texture

    Just texture ->
      pure texture

lookup ::
     ( HasType SpriteSheetCache r
     , MonadIO m
     , MonadReader r m
     )
  => Hashed FilePath
  -> m (Maybe Texture)
lookup path = do
  SpriteSheetCache cacheRef <- view (the @SpriteSheetCache)
  liftIO (HashMap.lookup path <$> readIORef cacheRef)

put ::
     ( HasType SpriteSheetCache r
     , MonadIO m
     , MonadReader r m
     )
  => Hashed FilePath
  -> Texture
  -> m ()
put path texture = do
  SpriteSheetCache cacheRef <- view (the @SpriteSheetCache)
  liftIO (modifyIORef' cacheRef (HashMap.insert path texture))
