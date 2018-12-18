module Gam.Internal.Render where

import Gam.Internal.Prelude
import Gam.Internal.SpriteSheetCache (SpriteSheetCache)

import qualified SDL


data Env
  = Env
  { window           :: SDL.Window
  , renderer         :: SDL.Renderer
  , spriteSheetCache :: SpriteSheetCache
  } deriving stock (Generic)

newtype Render a
  = Render (ReaderT Env IO a)
  deriving newtype (Applicative, Functor, Monad, MonadIO, MonadReader Env,
                     MonadUnliftIO)

run ::
     SDL.Window
  -> SDL.Renderer
  -> SpriteSheetCache
  -> Render a
  -> IO a
run window renderer spriteSheetCache (Render m) = do
  runReaderT m (Env window renderer spriteSheetCache)
