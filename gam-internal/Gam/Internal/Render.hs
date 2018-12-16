module Gam.Internal.Render where

import Gam.Internal.Prelude

import qualified SDL


data Env
  = Env
  { window   :: SDL.Window
  , renderer :: SDL.Renderer
  } deriving stock (Generic)

newtype Render a
  = Render (ReaderT Env IO a)
  deriving newtype (Applicative, Functor, Monad, MonadIO, MonadReader Env)

run :: SDL.Window -> SDL.Renderer -> Render a -> IO a
run window renderer (Render m) =
  runReaderT m (Env window renderer)
