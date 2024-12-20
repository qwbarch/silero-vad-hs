import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

main :: IO ()
main = defaultMain $ testGroup "Project" testTree
 where
  testTree =
    [ testCase "Hello world!" $
        () @?= ()
    ]
