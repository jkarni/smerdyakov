module SmerdyakovSpec (spec) where

import Smerdyakov
import GHC.TypeLits
import Data.Proxy
import Test.Hspec

data FileExists (dir :: Symbol) (s :: Symbol) = FileExists

instance (Needs (Writeable dir), KnownSymbol s) => Gives (FileExists s) where
  give = writeFile file ""
    where
      file = symbolVal (Proxy :: Proxy s)


