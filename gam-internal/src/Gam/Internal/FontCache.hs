module Gam.Internal.FontCache where

import Gam.Internal.Prelude

import qualified Data.HashMap.Strict as HashMap
import qualified SDL.Font            as SDL (Font)
import qualified SDL.Font            as SDL.Font


newtype FontCache
  = FontCache (IORef (HashMap (FilePath, Int) SDL.Font))

new :: IO FontCache
new =
  FontCache <$> newIORef HashMap.empty

load ::
    ( HasType FontCache r
    , MonadReader r m
    , MonadIO m
    )
 => FilePath
 -> Int
 -> m SDL.Font
load path size = do
  lookup path size >>= \case
    Nothing -> do
      font <- SDL.Font.load path size
      put path size font
      pure font

    Just font ->
      pure font

lookup ::
     (HasType FontCache r, MonadReader r m, MonadIO m)
  => FilePath
  -> Int
  -> m (Maybe SDL.Font)
lookup path size = do
  FontCache cacheRef <- view (the @FontCache)
  liftIO (HashMap.lookup (path, size) <$> readIORef cacheRef)

put ::
     (HasType FontCache r, MonadReader r m, MonadIO m)
  => FilePath
  -> Int
  -> SDL.Font
  -> m ()
put path size font = do
  FontCache cacheRef <- view (the @FontCache)
  liftIO (modifyIORef' cacheRef (HashMap.insert (path, size) font))
