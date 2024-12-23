module Silero.Model (
  SileroModel (..),
  loadModel,
  releaseModel,
  detectSpeech,
) where

import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as Vector
import Foreign (FunPtr, Ptr)
import Foreign.C (CString, withCString)
import Paths_silero_vad (getDataFileName)
import System.Posix (RTLDFlags (RTLD_NOW), dlopen, dlsym)

foreign import ccall "silero_vad.h load_model" c_load_model :: FunPtr () -> CString -> IO (Ptr ())

foreign import ccall "silero_vad.h release_model" c_release_model :: Ptr () -> IO ()

foreign import ccall "silero_vad.h detect_speech" c_detect_speech :: Ptr () -> Int -> Ptr Float -> IO Float

-- |
-- Holds state to be used for voice activity detection.
-- **Warning**: This is **NOT** thread-safe due to this mutating state internally.
newtype SileroModel = SileroModel
  { api :: Ptr ()
  }

loadModel :: IO SileroModel
loadModel = do
  dl <- flip dlopen [RTLD_NOW] =<< getDataFileName "lib/onnxruntime/lib/libonnxruntime.so"
  api <- dlsym dl "OrtGetApiBase"
  modelPath <- getDataFileName "lib/silero-vad/silero_vad.onnx"
  vad <-
    withCString modelPath $
      c_load_model api
  return $ SileroModel vad

releaseModel :: SileroModel -> IO ()
releaseModel = c_release_model . api

detectSpeech :: SileroModel -> Vector Float -> IO Float
detectSpeech model samples = do
  Vector.unsafeWith samples $
    c_detect_speech model.api (Vector.length samples)
