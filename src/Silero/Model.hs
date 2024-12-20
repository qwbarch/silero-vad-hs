module Silero.Model (
  SileroModel (..),
  loadModel,
) where

import Foreign (FunPtr, Ptr)
import Paths_SileroVAD (getDataFileName)
import System.Directory (doesFileExist)
import System.Posix (RTLDFlags (RTLD_NOW), dlopen, dlsym)

-- |
-- Holds state to be used for voice activity detection.
-- **Warning**: This is **NOT** thread-safe due to this mutating state internally.
newtype SileroModel = SileroModel
  { api :: Ptr ()
  }

loadModel :: IO (Maybe SileroModel)
loadModel = do
  filePath <- getDataFileName "lib/onnxruntime/lib/libonnxruntime.so"
  print =<< doesFileExist filePath
  putStrLn filePath
  dl <- dlopen filePath [RTLD_NOW]
  api <- c_ortGetApiBase =<< dlsym dl "OrtGetApiBase"
  pure . Just $ SileroModel api

foreign import ccall "dynamic"
  c_ortGetApiBase :: FunPtr (IO (Ptr ())) -> IO (Ptr ())
