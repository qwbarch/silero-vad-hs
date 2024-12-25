module Silero.Detector (
  VoiceDetector (..),
  SpeechSegment (..),
  detectSegments,
  createVad,
  withVad,
) where

import Control.Applicative (Applicative (liftA2))
import Control.Exception (bracket, finally)
import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Int (Int32)
import Data.Vector.Storable (Storable, Vector)
import qualified Data.Vector.Storable as Vector
import Foreign (Ptr, Storable (..), free, malloc, nullPtr, peekArray, with)
import Foreign.C (CFloat (..))
import Foreign.Storable.Generic (GStorable)
import GHC.Generics (Generic)
import Silero.Model (SileroModel (..), sampleRate, withModel)
import UnliftIO (MonadUnliftIO)

data VoiceDetector = VoiceDetector
  { model :: SileroModel
  , startThreshold :: Float
  , endThreshold :: Float
  , minSpeechSamples :: Float
  , maxSpeechSamples :: Float
  , speechPadSamples :: Float
  , minSilenceSamples :: Float
  , minSilenceSamplesAtMaxSpeech :: Float
  }
  deriving (Generic, GStorable)

-- |
-- Create a **VoiceDetector**.
-- **Warning: SileroModel holds internal state and is NOT thread safe.**
createVad :: SileroModel -> VoiceDetector
createVad model =
  VoiceDetector
    { model = model
    , startThreshold = 0.5
    , endThreshold = 0.35
    , minSpeechSamples = fromIntegral sampleRate / 1000.0 * 250.0 -- 250ms.
    , maxSpeechSamples = 1.0 / 0.0
    , speechPadSamples = fromIntegral sampleRate / 1000.0 * 30.0 -- 30ms.
    , minSilenceSamples = fromIntegral sampleRate / 1000.0 * 100.0 -- 100ms.
    , minSilenceSamplesAtMaxSpeech = fromIntegral sampleRate / 1000.0 * 98.0 -- 98ms
    }

-- |
-- Create a **VoiceDetector**.
-- **Warning: SileroModel holds internal state and is NOT thread safe.**
withVad :: (MonadUnliftIO m) => (VoiceDetector -> m a) -> m a
withVad runVad = withModel (runVad . createVad)

data SpeechSegment = SpeechSegment
  { startIndex :: Int32
  , endIndex :: Int32
  , startTime :: CFloat
  , endTime :: CFloat
  }
  deriving (Show, Read, Eq, Ord, Generic, GStorable)

foreign import ccall "detector.h detect_segments"
  c_detect_segments ::
    Ptr VoiceDetector ->
    Int -> -- samplesLength
    Ptr Float -> -- samples
    Ptr Int -> -- outSegmentsLength
    Ptr (Ptr SpeechSegment) -> -- outSegments
    IO ()

-- |
-- Detect the segments where speech starts and ends.
-- This implicitly resets the model after it finishes.
detectSegments :: (MonadIO m) => VoiceDetector -> Vector Float -> m [SpeechSegment]
detectSegments vad samples =
  liftIO . with vad $ \vadPtr ->
    Vector.unsafeWith samples $ \samplesPtr ->
      withPtr @Int $ \segmentsLengthPtr ->
        withPtr @(Ptr SpeechSegment) $ \segmentsPtr -> do
          poke samplesPtr 0
          poke segmentsPtr nullPtr
          flip finally (free =<< peek segmentsPtr) $ do
            c_detect_segments
              vadPtr
              (Vector.length samples)
              samplesPtr
              segmentsLengthPtr
              segmentsPtr
            join $
              liftA2
                peekArray
                (peek segmentsLengthPtr)
                (peek segmentsPtr)
  where
    withPtr :: forall a b. (Storable a) => (Ptr a -> IO b) -> IO b
    withPtr = bracket (malloc @a) free
