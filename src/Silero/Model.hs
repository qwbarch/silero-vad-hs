module Silero.Model (
  windowSize,
  SileroModel (..),
  withModel,
  detectSpeech,
  loadModel,
  releaseModel,
  resetModel,
) where

import Control.Exception.Lifted (bracket)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as Vector
import Foreign (FunPtr, Ptr)
import Foreign.C (CString, withCString)
import Paths_silero_vad (getDataFileName)
import System.Posix (RTLDFlags (RTLD_NOW), dlopen, dlsym)

-- | v5 model only supports this window size.
windowSize :: Int
windowSize = 512

foreign import ccall "silero_vad.h init_silero" c_init_silero :: FunPtr () -> CString -> IO (Ptr ())

foreign import ccall "silero_vad.h release_silero" c_release_silero :: Ptr () -> IO ()

foreign import ccall "silero_vad.h detect_speech" c_detect_speech :: Ptr () -> Int -> Ptr Float -> IO Float

foreign import ccall "silero_vad.h reset" c_reset :: Ptr () -> IO ()

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

-- | Safely acquire a **SileroModel**.
withModel :: (MonadBaseControl IO m, MonadIO m) => (SileroModel -> m a) -> m a
withModel =
  bracket
    (liftIO loadModel)
    (liftIO . releaseModel)

-- |
-- Reset the internal state of the model.
-- This should be called if you want to re-use the same model for new audio.
resetModel :: (MonadIO m) => SileroModel -> m ()
resetModel = liftIO . c_reset . api

-- |
-- Detect if speech is found in a single audio window.
-- **Note:** This must contain 512 samples, or it *will* error.
detectSpeech :: (MonadIO m) => SileroModel -> Vector Float -> m Float
detectSpeech model samples = do
  when (Vector.length samples /= windowSize) $
    error ("Window size must be length: " <> show windowSize)
  liftIO . Vector.unsafeWith samples $
    c_detect_speech model.api (Vector.length samples)
