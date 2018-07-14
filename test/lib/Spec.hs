import qualified Spec.LinesOfText as LinesOfText
import qualified Spec.Match       as Match
import qualified Spec.Patch       as Patch

import Test.Tasty


main :: IO ()
main = defaultMain $
  testGroup "All tests" [
    Patch.tests
  , Match.tests
  , LinesOfText.tests
  ]
