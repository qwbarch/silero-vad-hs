import Data.Function ((&))
import Data.Int (Int32)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as Vector
import Data.WAVE (WAVE (..), WAVESample, getWAVEFile, sampleToDouble)
import Paths_silero_vad (getDataFileName)
import Silero.Detector (VoiceDetector (..), defaultVad, detectSegments)
import Silero.Model (detectSpeech, loadModel, releaseModel, windowSize)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

main :: IO ()
main = defaultMain $ testGroup "Project" testTree
  where
    testTree =
      [ testCase "Hello world!" $ do
          model <- loadModel

          wav <- getWAVEFile =<< getDataFileName "lib/jfk.wav"
          let samples :: Vector Float
              samples =
                concat wav.waveSamples
                  & Vector.fromList
                  & Vector.map (realToFrac . sampleToDouble)

          -- print defaultVad
          print windowSize
          segments <- detectSegments defaultVad model samples

          print segments

          releaseModel model
          () @?= ()
      ]
