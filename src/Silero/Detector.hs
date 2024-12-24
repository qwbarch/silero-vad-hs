module Silero.Detector (
  VoiceDetector (..),
  detectSegments,
  defaultVad,
) where

import Control.Applicative (Applicative (liftA2))
import Control.Exception (bracket, finally)
import Control.Monad (join)
import Data.Vector.Storable (Storable, Vector)
import qualified Data.Vector.Storable as Vector
import Foreign (Ptr, Storable (..), free, malloc, nullPtr, peekArray)
import Foreign.C (CFloat (..))
import Silero.Model (SileroModel (SileroModel), windowSize)
import TH.Derive (Deriving, derive)
import TH.Derive.Storable ()

data VoiceDetector = VoiceDetector
  { startThreshold :: Float
  , endThreshold :: Float
  , minSpeechSamples :: Float
  , maxSpeechSamples :: Float
  , speechPadSamples :: Float
  , minSilenceSamples :: Float
  , minSilenceSamplesAtMaxSpeech :: Float
  }
  deriving (Show, Read, Eq, Ord)

defaultVad :: VoiceDetector
defaultVad =
  VoiceDetector
    { startThreshold = 0.5
    , endThreshold = 0.35
    , minSpeechSamples = sampleRate / 1000.0 * 250.0 -- 250ms.
    , maxSpeechSamples = sampleRate / maxFloat - fromIntegral windowSize - 2 * speechPadSamples
    , speechPadSamples = speechPadSamples
    , minSilenceSamples = sampleRate / 1000.0 * 100.0 -- 100ms.
    , minSilenceSamplesAtMaxSpeech = sampleRate / 1000.0 * 98.0 -- 98ms
    }
  where
    sampleRate = 16_000.0
    maxFloat = fromIntegral . snd $ floatRange @Float 0.0
    speechPadSamples = sampleRate / 1000.0 * 30.0 -- 30ms.

data SpeechSegment = SpeechSegment
  { startIndex :: Int
  , endIndex :: Int
  , startTime :: Float
  , endTime :: Float
  }
  deriving (Show, Read, Eq, Ord)

$($(derive [d|instance Deriving (Storable SpeechSegment)|]))

foreign import ccall "detector.h detect_segments"
  c_detect_segments ::
    Ptr () ->
    Float -> -- startThreshold
    Float -> -- endThreshold
    Float -> -- minSpeechSamples
    Float -> -- maxSpeechSamples
    Float -> -- speechPadSamples
    Float -> -- minSilenceSamples
    Float -> -- minSilenceSamplesAtMaxSpeech
    Int -> -- samplesLength
    Ptr Float -> -- samples
    Ptr Int -> -- outSegmentsLength
    Ptr SpeechSegment -> -- outSegments
    IO ()

detectSegments :: VoiceDetector -> SileroModel -> Vector Float -> IO [SpeechSegment]
detectSegments vad (SileroModel model) samples = do
  putStrLn "detectSegments start"
  let withPtr :: forall a b. (Storable a) => (Ptr a -> IO b) -> IO b
      withPtr = bracket (malloc @a) free

  Vector.unsafeWith samples $ \samplesPtr ->
    withPtr @Int $ \segmentsLengthPtr ->
      withPtr @SpeechSegment $ \segmentsPtr -> do
        poke samplesPtr 0
        putStrLn "before c_detect_segments"
        c_detect_segments
          model
          (realToFrac vad.startThreshold)
          vad.endThreshold
          vad.minSpeechSamples
          vad.maxSpeechSamples
          vad.speechPadSamples
          vad.minSilenceSamples
          vad.minSilenceSamplesAtMaxSpeech
          (Vector.length samples)
          samplesPtr
          segmentsLengthPtr
          segmentsPtr
        putStrLn "after c_detect_segments"
        x <- peek segmentsLengthPtr
        putStrLn $ "segments length: " <> show x
        join $
          liftA2
            peekArray
            (peek segmentsLengthPtr)
            (pure segmentsPtr)
