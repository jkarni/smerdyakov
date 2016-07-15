-- | 'Smerdyakov' allows you to declare dependencies or assumptions that your
-- functions make, and have them automatically gather recursively gathered. The
-- essence of it is not unlike the tool @Make@.
module Smerdyakov
  (
  -- * Core functionality
  -- | These classes provide the core functionality of the package.
    Gives(..)
  , Needs
  , make

  -- * Actions
  -- | The actions that are to be performed in fulfilling your dependencies or
  -- assumptions are described in the monad @Action@. This is a free monad that
  -- can perform basic IO functionality.
  --
  -- The set of IO actions defined in 'Action' is still quite limited; if there
  -- is something you need that is not yet available, please open an issue or
  -- send a PR.
  , shellA
  , shellWithErrA
  , openFileA
  , hCloseA
  , interpretIO
  , throwA
  , ActionError(..)
  , Action


  -- * Re-Exports
  , Proxy(Proxy)
  ) where

import Smerdyakov.Internal.Class as X
import Smerdyakov.Internal.FreeIO as X
import Data.Proxy as X
