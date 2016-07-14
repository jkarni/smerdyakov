module Smerdyakov.FreeIO where

import Data.Monoid ((<>))
import Control.Monad.Trans.Except
import GHC.Generics (Generic)
import System.Exit (ExitCode(..))
import System.IO
import Control.Monad.Free

data ActionF x
  = Shell String (ExitCode -> String -> String -> x)
  | OpenFile FilePath IOMode (Handle -> x)
  | HClose Handle x
  deriving (Functor, Generic)

data ActionErr
  = ExpectationFailure String
  deriving (Eq, Show, Read, Generic)

type Action = ExceptT ActionErr (Free ActionF)

shell :: String -> Action (ExitCode, String, String)
shell cmd = liftF $ Shell cmd (,,)

shellWithErr :: String -> Action String
shellWithErr cmd = do
  (e, out, err) <- shell cmd
  case e of
    ExitFailure _ -> throwE $ ExpectationFailure $ "Process exited non-zero: " <> err
    ExitSuccess   -> return out

openFile :: FilePath -> IOMode -> Action Handle
openFile file mode = liftF $ OpenFile file mode id

hClose :: Handle -> Action ()
hClose hdl = liftF $ HClose hdl ()
