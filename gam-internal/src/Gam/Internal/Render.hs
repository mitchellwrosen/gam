module Gam.Internal.Render where

import Gam.Internal.FontCache         (FontCache)
import Gam.Internal.FrameCount        (FrameCount)
import Gam.Internal.Prelude
import Gam.Internal.RenderedTextCache (RenderedTextCache)
import Gam.Internal.SpriteSheetCache  (SpriteSheetCache)

import qualified SDL


data Env
  = Env
  { window :: SDL.Window
  , renderer :: SDL.Renderer
  , fontCache :: FontCache
  , renderedTextCache :: RenderedTextCache
  , spriteSheetCache :: SpriteSheetCache
  , frameCount :: IORef FrameCount
  } deriving stock (Generic)

newtype Render a
  = Render (ReaderT Env IO a)
  deriving newtype (Applicative, Functor, Monad, MonadIO, MonadReader Env)

run ::
     SDL.Window
  -> SDL.Renderer
  -> FontCache
  -> RenderedTextCache
  -> SpriteSheetCache
  -> IORef FrameCount
  -> Render a
  -> IO a
run window renderer fontCache renderedTextCache spriteSheetCache frameCount (Render m) = do
  runReaderT m Env
    { window = window
    , renderer = renderer
    , fontCache = fontCache
    , renderedTextCache = renderedTextCache
    , spriteSheetCache = spriteSheetCache
    , frameCount = frameCount
    }
