module Gam.Internal.FontCache where

import Gam.Internal.Prelude

import qualified Data.HashMap.Strict as HashMap
import qualified SDL.Font            as SDL (Font)
import qualified SDL.Font            as SDL.Font

import Data.Hashable     (Hashed, hashed)
import Data.Tuple.Strict


newtype FontCache
  = FontCache (IORef (HashMap (Hashed (T2 FilePath Int)) SDL.Font))

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
  lookup key >>= \case
    Nothing -> do
      font <- SDL.Font.load path size
      put key font
      pure font

    Just font ->
      pure font
  where
    key :: Hashed (T2 FilePath Int)
    key =
      hashed (T2 path size)

lookup ::
     (HasType FontCache r, MonadReader r m, MonadIO m)
  => Hashed (T2 FilePath Int)
  -> m (Maybe SDL.Font)
lookup key = do
  FontCache cacheRef <- view (the @FontCache)
  liftIO (HashMap.lookup key <$> readIORef cacheRef)

put ::
     (HasType FontCache r, MonadReader r m, MonadIO m)
  => Hashed (T2 FilePath Int)
  -> SDL.Font
  -> m ()
put key font = do
  FontCache cacheRef <- view (the @FontCache)
  liftIO (modifyIORef' cacheRef (HashMap.insert key font))
