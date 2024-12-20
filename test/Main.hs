import Silero.Model (loadModel)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

main :: IO ()
main = defaultMain $ testGroup "Project" testTree
  where
    testTree =
      [ testCase "Hello world!" $ do
          loadModel >>= \case
            Nothing -> putStrLn "failed to load model"
            Just _ -> putStrLn "model loaded"
          () @?= ()
      ]
