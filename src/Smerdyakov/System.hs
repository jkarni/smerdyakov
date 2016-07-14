{-# LANGUAGE CPP #-}
module Smerdyakov.System where

import Data.Monoid ((<>))
import Control.Monad.Error.Class
import Control.Monad
import GHC.TypeLits
import GHC.Generics (Generic)
import Data.String (IsString(..))
import System.Exit (ExitCode(..))

import Smerdyakov.Internal.Class
import Smerdyakov.Internal.FreeIO

data Executable (s :: Symbol) = Executable String
  deriving (Eq, Show, Read, Generic)

data Sudo = Sudo
  deriving (Eq, Show, Read, Generic)

instance Gives Sudo where
  give = do
    -- This is not ideal, since attempts to sudo are logged and may result in
    -- warnings
    (exitStatus, _, _) <- shellA "sudo -n true"
    case exitStatus of
      ExitFailure _ -> throwError $ ExpectationFailure "Need sudo"
      ExitSuccess   -> return Sudo

instance Gives KnownOS where
  give = do
    uname <- shellWithErrA "uname"
    case uname of
      "Darwin" -> return OSXOS
      "Linux"  -> do
        x <- shellWithErrA "lsb_release -is"
        case x of
          "Ubuntu" -> return UbuntuOS
          "Debian" -> return DebianOS
          "Arch"   -> return ArchOS
          d        -> throwError . ExpectationFailure $ "Unknown distro: " <> d
      d        -> throwError . ExpectationFailure $ "Unknown distro: " <> d

instance (Needs KnownOS, Needs Sudo) => Gives (Executable "ack") where
  give = do
    Sudo <- give
    getPackage "ack-grep"
    return $ Executable "ack-grep"


data PackageDetails
  = PackageDetails
  { homebrew :: String
  , aptGet   :: String
  , pacman   :: String
  } deriving (Eq, Show, Read, Generic)

getPackage :: (Needs KnownOS) => PackageDetails -> Action ()
getPackage deets = do
  os <- give
  let run = void . shellWithErrA
  case os of
    OSXOS    -> run $ "brew install " <> homebrew deets
    UbuntuOS -> give >>= \Sudo -> run $ "sudo apt-get install " <> aptGet deets
    DebianOS -> give >>= \Sudo -> run $ "sudo apt-get install " <> aptGet deets
    ArchOS   -> give >>= \Sudo -> run $ "pacman -S " <> pacman deets


instance IsString PackageDetails where
  fromString s = PackageDetails s s s


-- * Operating systems



data KnownOS
  = DebianOS
  | ArchOS
  | UbuntuOS
  | OSXOS
  deriving (Eq, Show, Read, Generic)

{-
data Debian = Debian
data Arch = Arch
data OSX = OSX
data Linux = Linux
data Homebrew
type family PackageManagerNeeds a where
  PackageManagerNeeds Debian = ()
  PackageManagerNeeds Arch   = ()
  PackageManagerNeeds OSX    = Homebrew
-- This function generates a @Gives@ declaration with the OS and distribution
-- of the machine in which the program is compiled.
mkOSDec :: Q Dec
mkOSDec = do
  [d| instance Gives $osT where give = return $osC |]
  [d| hostOs :: HostOS ; hostOS = $hos |]
  -}
