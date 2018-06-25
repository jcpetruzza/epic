import qualified Spec.Patch as Patch

import Test.Tasty


main :: IO ()
main = defaultMain $
  testGroup "All tests" [
    Patch.tests
  ]
