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
import Foreign (Ptr, Storable (..), free, malloc, nullPtr, peekArray)
import Foreign.C (CFloat (..), CInt)
import Foreign.Storable.Generic (GStorable)
import GHC.Generics (Generic)
import GHC.Real (infinity)
import Silero.Model (SileroModel (SileroModel), windowSize)

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
    , maxSpeechSamples = 1 / 0
    , speechPadSamples = speechPadSamples
    , minSilenceSamples = sampleRate / 1000.0 * 100.0 -- 100ms.
    , minSilenceSamplesAtMaxSpeech = sampleRate / 1000.0 * 98.0 -- 98ms
    }
  where
    sampleRate = 16_000.0
    speechPadSamples = sampleRate / 1000.0 * 30.0 -- 30ms.

data SpeechSegment = SpeechSegment
  { startIndex :: Int32
  , endIndex :: Int32
  , startTime :: CFloat
  , endTime :: CFloat
  }
  deriving (Show, Read, Eq, Ord, Generic, GStorable)

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
    IO (Ptr SpeechSegment)

detectSegments :: VoiceDetector -> SileroModel -> Vector Float -> IO [SpeechSegment]
detectSegments vad (SileroModel model) samples = do
  putStrLn "detectSegments start"
  let withPtr :: forall a b. (Storable a) => (Ptr a -> IO b) -> IO b
      withPtr = bracket (malloc @a) free

  Vector.unsafeWith samples $ \samplesPtr ->
    withPtr @Int $ \segmentsLengthPtr -> do
      poke samplesPtr 0
      putStrLn "before c_detect_segments"
      segments <-
        c_detect_segments
          model
          -- (realToFrac vad . startThreshold)
          undefined
          undefined
          undefined
          undefined
          undefined
          undefined
          undefined
          undefined
          undefined
          undefined
      --  vad
      -- . endThreshold
      --   vad
      -- . minSpeechSamples
      --   vad
      -- . maxSpeechSamples
      --   vad
      -- . speechPadSamples
      --   vad
      -- . minSilenceSamples
      --   vad
      -- . minSilenceSamplesAtMaxSpeech
      --  (Vector.length samples)
      --  samplesPtr
      --  segmentsLengthPtr
      putStrLn "after c_detect_segments"
      samplesLength <- peek segmentsLengthPtr
      peekArray
        samplesLength
        segments
