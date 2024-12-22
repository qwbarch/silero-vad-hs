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

foreign import ccall "silero_vad.h init_silero" c_init_silero :: FunPtr () -> CString -> IO (Ptr ())

foreign import ccall "silero_vad.h release_silero" c_release_silero :: Ptr () -> IO ()

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
      c_init_silero api
  return $ SileroModel vad

releaseModel :: SileroModel -> IO ()
releaseModel = c_release_silero . api

detectSpeech :: SileroModel -> Vector Float -> IO Float
detectSpeech model samples = do
  Vector.unsafeWith samples $
    c_detect_speech model.api (Vector.length samples)
