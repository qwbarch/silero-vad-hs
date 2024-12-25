{-# LANGUAGE CPP #-}

module Silero.Model (
  SileroModel (..),
  detectSpeech,
  windowSize,
  resetModel,
  sampleRate,
  withModel,
) where

import Data.Int (Int64)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as Vector
import Foreign.Storable (Storable (..))
import GHC.Generics (Generic)
import GHC.IO (unsafeDupablePerformIO)
import Paths_silero_vad (getDataFileName)
import UnliftIO (MonadIO (liftIO), MonadUnliftIO, bracket)

#if defined (linux_HOST_OS) || defined (darwin_HOST_OS)

import System.Posix (RTLDFlags (RTLD_NOW), dlopen, dlsym)
import Foreign (FunPtr, Ptr, castPtr)
import Foreign.C (CString, withCString)

#else


import System.Win32 (getProcAddress, loadLibrary)
import Foreign (FunPtr, Ptr, castPtr, castPtrToFunPtr)
import Foreign.C (CWString, withCWString)

#endif

foreign import ccall "model.h get_window_length" c_get_window_length :: IO Int64

foreign import ccall "model.h get_sample_rate" c_get_sample_rate :: IO Int64

foreign import ccall "model.h release_model" c_release_model :: Ptr () -> IO ()

foreign import ccall "model.h reset_model" c_reset_model :: Ptr () -> IO ()

foreign import ccall "model.h detect_speech" c_detect_speech :: Ptr () -> Ptr Float -> IO Float

#if defined (linux_HOST_OS) || defined (darwin_HOST_OS)

foreign import ccall "model.h load_model" c_load_model :: FunPtr () -> CString -> IO (Ptr ())

#else

foreign import ccall "model.h load_model" c_load_model :: FunPtr () -> CWString -> IO (Ptr ())

#endif

windowSize :: Int
windowSize = fromIntegral $ unsafeDupablePerformIO c_get_window_length

sampleRate :: Int
sampleRate = fromIntegral $ unsafeDupablePerformIO c_get_sample_rate

-- |
-- Holds state to be used for voice activity detection.
-- **Warning**: This is **NOT** thread-safe due to this mutating state internally.
newtype SileroModel = SileroModel
  { api :: Ptr ()
  }
  deriving (Generic)

instance Storable SileroModel where
  sizeOf _ = sizeOf (undefined :: Ptr ())
  alignment _ = alignment (undefined :: Ptr ())
  peek ptr = do
    apiPtr <- peek (castPtr ptr)
    return $ SileroModel apiPtr
  poke ptr (SileroModel apiPtr) = poke (castPtr ptr) apiPtr

libraryPath :: FilePath
#if defined(linux_HOST_OS)

libraryPath = "lib/onnxruntime/linux-x64/lib/libonnxruntime.so"

#elif defined(darwin_HOST_OS)
  #if defined(aarch64_HOST_ARCH)

  libraryPath = "lib/onnxruntime/osx-arm64/lib/libonnxruntime.dylib"

  #else

  libraryPath = "lib/onnxruntime/osx-x64/lib/libonnxruntime.dylib"

  #endif

#else

libraryPath = "lib/onnxruntime/windows-x64/lib/onnxruntime.dll"

#endif

loadApi :: IO (FunPtr ())
#if defined (linux_HOST_OS) || defined (darwin_HOST_OS)

loadApi = do
  dl <- flip dlopen [RTLD_NOW] =<< getDataFileName libraryPath
  dlsym dl "OrtGetApiBase"

#else

loadApi = do
  library <- loadLibrary =<< getDataFileName libraryPath
  castPtrToFunPtr <$> getProcAddress library "OrtGetApiBase"

#endif

getModelPath :: IO String
getModelPath = getDataFileName "lib/silero-vad/silero_vad.onnx"

#if defined (linux_HOST_OS) || defined (darwin_HOST_OS)

withModelPath :: (CString -> IO a) -> IO a
withModelPath runModelPath = do
  modelPath <- getModelPath
  withCString modelPath $ runModelPath

#else

withModelPath :: (CWString -> IO a) -> IO a
withModelPath runModelPath = do
  modelPath <- getModelPath
  withCWString modelPath runModelPath

#endif

loadModel :: IO SileroModel
loadModel = do
  api <- loadApi
  vad <- withModelPath $ c_load_model api
  return $ SileroModel vad

-- | Reset the internal state of the model. This should be called when giving fresh audio samples.
releaseModel :: SileroModel -> IO ()
releaseModel = c_release_model . api

withModel :: (MonadUnliftIO m) => (SileroModel -> m a) -> m a
withModel =
  bracket
    (liftIO loadModel)
    (liftIO . releaseModel)

resetModel :: (MonadIO m) => SileroModel -> m ()
resetModel = liftIO . c_reset_model . api

-- |
-- Detect if speech is found within the given audio samples.
-- This has the following requirements:
-- - Must be 16khz sample rate.
-- - Must be mono-channel.
-- - Must be 16-bit audio.
-- - Must contain exactly 512 samples.
detectSpeech :: (MonadIO m) => SileroModel -> Vector Float -> m Float
detectSpeech (SileroModel api) samples
  | Vector.length samples /= windowSize =
      return 0.0
  | otherwise =
      liftIO . Vector.unsafeWith samples $
        c_detect_speech api
