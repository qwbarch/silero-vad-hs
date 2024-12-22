module Silero.Model (
  SileroModel (..),
  loadModel,
  releaseModel,
  detectSpeech,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.ByteString.Unsafe (unsafeUseAsCString)
import Foreign (FunPtr, Ptr)
import Foreign.C (CString, withCString)
import Paths_silero_vad (getDataFileName)
import System.Posix (RTLDFlags (RTLD_NOW), dlopen, dlsym)

foreign import ccall "silero_vad.h init_silero" c_init_silero :: FunPtr () -> CString -> IO (Ptr ())

foreign import ccall "silero_vad.h release_silero" c_release_silero :: Ptr () -> IO ()

foreign import ccall "silero_vad.h detect_speech" c_detect_speech :: Ptr () -> Int -> CString -> IO Float

-- |
-- Holds state to be used for voice activity detection.
-- **Warning**: This is **NOT** thread-safe due to this mutating state internally.
newtype SileroModel = SileroModel
  { api :: Ptr ()
  }

loadModel :: IO SileroModel
loadModel = do
  modelPath <- getDataFileName "lib/silero-vad/silero_vad.onnx"
  libPath <- getDataFileName "lib/onnxruntime/lib/libonnxruntime.so"
  dl <- dlopen libPath [RTLD_NOW]
  api <- dlsym dl "OrtGetApiBase"
  vad <-
    withCString modelPath $
      c_init_silero api
  pure $ SileroModel vad

releaseModel :: SileroModel -> IO ()
releaseModel = c_release_silero . api

detectSpeech :: SileroModel -> ByteString -> IO Float
detectSpeech model pcmData = do
  unsafeUseAsCString pcmData $
    c_detect_speech model.api (ByteString.length pcmData)
