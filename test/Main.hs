{-# LANGUAGE OverloadedLists #-}

import Control.Applicative (Applicative (liftA2))
import Control.Monad (join)
import Data.Function ((&))
import Data.Functor (void)
import qualified Data.Vector.Storable as Vector
import Data.WAVE (WAVE (..), getWAVEFile, sampleToDouble)
import Paths_silero_vad (getDataFileName)
import Silero.Detector (SpeechSegment (..), detectSegments, withVad)
import Silero.Model (detectSpeech, windowSize, withModel)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

main :: IO ()
main = defaultMain $ testGroup "silero-vad" testTree
  where
    loadSamples = do
      wav <- getWAVEFile =<< getDataFileName "lib/jfk.wav"
      pure $
        concat wav.waveSamples
          & Vector.fromList
          & Vector.map (realToFrac . sampleToDouble)
    testTree =
      [ testCase "detectSegments should provide valid speech segments" . withVad $ \vad -> do
          samples <- loadSamples
          segments <- detectSegments vad samples
          let expected =
                [ SpeechSegment
                    { startIndex = 4640
                    , endIndex = 35296
                    , startTime = 0.29
                    , endTime = 2.206
                    }
                , SpeechSegment
                    { startIndex = 57376
                    , endIndex = 69600
                    , startTime = 3.586
                    , endTime = 4.35
                    }
                , SpeechSegment
                    { startIndex = 86048
                    , endIndex = 122336
                    , startTime = 5.378
                    , endTime = 7.646
                    }
                , SpeechSegment
                    { startIndex = 130592
                    , endIndex = 169952
                    , startTime = 8.161
                    , endTime = 10.622
                    }
                ]
          segments @?= expected
      , testCase "detectSegments should be equal, given the same input after resetting" . withVad $ \vad -> do
          samples <- loadSamples
          segments1 <- detectSegments vad samples
          segments2 <- detectSegments vad samples
          segments1 @?= segments2
      , testCase "detectSegments should not throw on inputs smaller than windowLength" . withVad $ \vad -> do
          void $ detectSegments vad []
          void $ detectSegments vad [1, 2, 3, 4, 5]
      , testCase "detectSpeech should return 0 when not given windowLength samples" . withModel $ \model -> do
          let test actual expected =
                join $ liftA2 (@?=) (detectSpeech model actual) (pure expected)
          test [] 0.0
          test (Vector.replicate (windowSize - 1) 1.0) 0.0
          test (Vector.replicate (windowSize + 1) -1.0) 0.0
      ]
