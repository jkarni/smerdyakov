module Smerdyakov.Internal.FreeIO where

import Data.Monoid ((<>))
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import GHC.Generics (Generic)
import System.Exit (ExitCode(..))
import System.Process (readCreateProcessWithExitCode, shell)
import System.IO
import Control.Monad.Free

-- | Free monad with IO actions that are allowed in defining @give@.
--
-- You are quite likely to hit limitations; if so, send a PR adding new
-- constructors and helpers!
data ActionF e x
  = Shell String (ExitCode -> String -> String -> x)
  | OpenFile FilePath IOMode (Handle -> x)
  | HClose Handle x
  | Throw e
  | Catch x (e -> x)
  deriving (Functor, Generic)

-- | An error that can be thrown in the 'Action' monad.
data ActionError
  = ExpectationFailure String
  deriving (Eq, Show, Read, Generic)

type Action = Free (ActionF ActionError)

instance {-# OVERLAPPING #-} MonadError ActionError Action where
  throwError e = liftF $ Throw e
  catchError e f = catchE e f

-- | Runs the shell command, returning the exit code, stdout, and stderr.
shellA :: String -> Action (ExitCode, String, String)
shellA cmd = liftF $ Shell cmd (,,)

-- | Like 'shellA', but calls @throwError@ with stderr in case the command exited
-- unsucessfully, and otherwise returns just stdout.
shellWithErrA :: String -> Action String
shellWithErrA cmd = do
  (e, out, err) <- shellA cmd
  case e of
    ExitFailure _ -> throwError . ExpectationFailure
                   $ "Process exited non-zero: " <> err
    ExitSuccess   -> return out

-- | Like 'openFile', but for 'Action'.
openFileA :: FilePath -> IOMode -> Action Handle
openFileA file mode = liftF $ OpenFile file mode id

-- | Like 'hClose', but for 'Action'
hCloseA :: Handle -> Action ()
hCloseA hdl = liftF $ HClose hdl ()

catchE :: Action a -> (e -> Action a) -> Action a
catchE = undefined -- TODO

-- | Interpret an 'Action' in a 'MonadIO'.
interpretIO :: (MonadIO m, MonadError ActionError m) => Action a -> m a
interpretIO (Pure a) = return a
interpretIO (Free v) = case v of
  Shell cmd r -> do
    (a,b,c) <- liftIO $ readCreateProcessWithExitCode (shell cmd) ""
    interpretIO $ r a b c
  OpenFile fp mode r -> liftIO (openFile fp mode) >>= interpretIO . r
  HClose hdl r -> liftIO (hClose hdl) >> interpretIO r
  Throw e -> throwError e
  Catch e f -> catchError (interpretIO e) (interpretIO . f)
