module Gam.Internal.Prelude
  ( module X
  ) where

import Control.Applicative          as X
import Control.Category             as X ((>>>))
import Control.Lens                 as X (over, view, (%~), (.~), (^.))
import Control.Monad.IO.Class       as X (MonadIO(..))
import Control.Monad.Reader         as X
import Data.Coerce                  as X (Coercible, coerce)
import Data.Foldable                as X (for_)
import Data.Function                as X ((&))
import Data.Generics.Product.Any    as X (the)
import Data.Generics.Product.Fields as X (HasField, field)
import Data.Generics.Product.Typed  as X (HasType)
import Data.Hashable                as X (Hashable, Hashed, hashed, unhashed)
import Data.HashMap.Strict          as X (HashMap)
import Data.Int                     as X
import Data.IntMap.Strict           as X (IntMap)
import Data.IORef                   as X
import Data.Maybe                   as X (catMaybes, fromMaybe)
import Data.StateVar                as X (($=!))
import Data.Text                    as X (Text)
import Data.Word                    as X
import Foreign.C                    as X (CDouble, CInt)
import GHC.Generics                 as X (Generic)
import Prelude                      as X hiding (lookup)
