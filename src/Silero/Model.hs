{-# LANGUAGE CPP #-}

module Silero.Model (
  SileroModel (..),
  detectSpeech,
  windowLength,
  resetModel,
  sampleRate,
  withModel,
) where

import Data.Int (Int64)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as Vector
import Foreign.Storable (Storable (..))
import GHC.Generics (Generic)
import GHC.IO (unsafeDupablePerformIO, unsafePerformIO)
import Paths_silero_vad (getDataFileName)
import UnliftIO (MonadIO (liftIO), MonadUnliftIO, bracket)

#if defined (linux_HOST_OS) || defined (darwin_HOST_OS)

import System.Posix (RTLDFlags (RTLD_NOW), dlsym, dlopen )
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

windowLength :: Int
windowLength = fromIntegral $ unsafeDupablePerformIO c_get_window_length

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

libraryPath = "lib/onnxruntime/linux-x64/libonnxruntime.so"

#elif defined(darwin_HOST_OS) && defined(aarch64_HOST_ARCH)

libraryPath = "lib/onnxruntime/mac-arm64/libonnxruntime.dylib"

#elif defined(darwin_HOST_OS) && !defined(aarch64_HOST_ARCH)

libraryPath = "lib/onnxruntime/mac-x64/libonnxruntime.dylib"

#else

libraryPath = "lib/onnxruntime/windows-x64/onnxruntime.dll"

#endif

{-# NOINLINE onnxruntime #-}
onnxruntime :: FunPtr ()
#if defined (linux_HOST_OS) || defined (darwin_HOST_OS)

onnxruntime =
  unsafePerformIO $
    getDataFileName libraryPath
      >>= flip dlopen [RTLD_NOW]
      >>= (liftIO . flip dlsym "OrtGetApiBase")

#else

onnxruntime =
  unsafePerformIO $
    getDataFileName libraryPath
      >>= loadLibrary
      >>= (fmap castPtrToFunPtr . flip getProcAddress "OrtGetApiBase")

#endif

getModelPath :: IO String
getModelPath = getDataFileName "lib/silero_vad.onnx"

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

-- | **Warning: SileroModel holds internal state and is NOT thread safe.**
withModel :: (MonadUnliftIO m) => (SileroModel -> m a) -> m a
withModel runModel = do
  bracket
    (SileroModel <$> liftIO (withModelPath $ c_load_model onnxruntime))
    (liftIO . c_release_model . api)
    runModel

-- | **Warning: SileroModel holds internal state and is NOT thread safe.**
resetModel :: (MonadIO m) => SileroModel -> m ()
resetModel = liftIO . c_reset_model . api

-- |
-- Detect if speech is found within the given audio samples.
-- This has the following requirements:
-- - Must be 16khz sample rate.
-- - Must be mono-channel.
-- - Must be 16-bit audio.
-- - Must contain exactly 512 samples.
--
-- | **Warning: SileroModel holds internal state and is NOT thread safe.**
detectSpeech :: (MonadIO m) => SileroModel -> Vector Float -> m Float
detectSpeech (SileroModel api) samples
  | Vector.length samples /= windowLength =
      return 0.0
  | otherwise =
      liftIO . Vector.unsafeWith samples $
        c_detect_speech api
