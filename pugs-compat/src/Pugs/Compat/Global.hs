module Pugs.Compat.Global (
    _GlobalFinalizer,
    safeMode
) where

import Data.IORef
import System.IO.Unsafe
import Pugs.Compat.Posix
import Pugs.Compat.Monads

{-|
Returns @True@ if the environment variable @PUGS_SAFEMODE@ is set to a
true value. Most IO primitives are disabled under safe mode.
-}
safeMode :: Bool
safeMode = case (inlinePerformIO $ getEnv "PUGS_SAFEMODE") of
    Nothing     -> False
    Just ""     -> False
    Just "0"    -> False
    _           -> True

{-# NOINLINE _GlobalFinalizer #-}
_GlobalFinalizer :: IORef (IO ())
_GlobalFinalizer = unsafePerformIO $ newIORef (return ())

