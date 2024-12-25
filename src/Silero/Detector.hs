module Silero.Detector (
  VoiceDetector (..),
  detectSegments,
  defaultVad,
) where

import Control.Applicative (Applicative (liftA2))
import Control.Exception (bracket, finally)
import Control.Monad (join)
import Data.Int (Int32)
import Data.Vector.Storable (Storable, Vector)
import qualified Data.Vector.Storable as Vector
import Foreign (Ptr, Storable (..), free, malloc, nullPtr, peekArray, with)
import Foreign.C (CFloat (..))
import Foreign.Storable.Generic (GStorable)
import GHC.Generics (Generic)
import Silero.Model (SileroModel (..), resetModel)

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

defaultVad :: SileroModel -> VoiceDetector
defaultVad model =
  VoiceDetector
    { model = model
    , startThreshold = 0.5
    , endThreshold = 0.35
    , minSpeechSamples = sampleRate / 1000.0 * 250.0 -- 250ms.
    , maxSpeechSamples = 1.0 / 0.0
    , speechPadSamples = sampleRate / 1000.0 * 30.0 -- 30ms.
    , minSilenceSamples = sampleRate / 1000.0 * 100.0 -- 100ms.
    , minSilenceSamplesAtMaxSpeech = sampleRate / 1000.0 * 98.0 -- 98ms
    }
  where
    sampleRate = 16_000.0

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

detectSegments :: VoiceDetector -> Vector Float -> IO [SpeechSegment]
detectSegments vad samples = do
  let withPtr :: forall a b. (Storable a) => (Ptr a -> IO b) -> IO b
      withPtr = bracket (malloc @a) free
  with vad $ \vadPtr ->
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
            resetModel vad.model
            join $
              liftA2
                peekArray
                (peek segmentsLengthPtr)
                (peek segmentsPtr)
