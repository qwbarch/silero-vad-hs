module Silero.Model (
  SileroModel (..),
  loadModel,
  releaseModel,
  detectSpeech,
  windowSize,
  resetModel,
) where

import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as Vector
import Foreign (FunPtr, Ptr, Storable, castPtr)
import Foreign.C (CString, withCString)
import Foreign.Storable (Storable (..))
import GHC.Generics (Generic)
import GHC.IO (unsafeDupablePerformIO)
import Paths_silero_vad (getDataFileName)
import System.Posix (RTLDFlags (RTLD_NOW), dlopen, dlsym)

foreign import ccall "model.h get_window_size" c_get_window_size :: IO Int

foreign import ccall "model.h load_model" c_load_model :: FunPtr () -> CString -> IO (Ptr ())

foreign import ccall "model.h release_model" c_release_model :: Ptr () -> IO ()

foreign import ccall "model.h reset_model" c_reset_model :: Ptr () -> IO ()

foreign import ccall "model.h detect_speech" c_detect_speech :: Ptr () -> Ptr Float -> IO Float

windowSize :: Int
windowSize = unsafeDupablePerformIO c_get_window_size

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

resetModel :: SileroModel -> IO ()
resetModel = c_reset_model . api

detectSpeech :: SileroModel -> Vector Float -> IO Float
detectSpeech model samples =
  Vector.unsafeWith samples $
    c_detect_speech model.api
