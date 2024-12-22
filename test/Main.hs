import Data.Function ((&))
import Data.Int (Int32)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as Vector
import Data.WAVE (WAVE (..), getWAVEFile)
import Paths_silero_vad (getDataFileName)
import Silero.Model (detectSpeech, loadModel, releaseModel)
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
                  & Vector.map ((\sample -> sample / fromIntegral (maxBound :: Int32)) . fromIntegral)
              xs = Vector.take (round @Float $ 16000.0 * 0.02) samples

          prob <- detectSpeech model xs

          putStrLn $ "probability: " <> show prob

          releaseModel model
          () @?= ()
      ]
