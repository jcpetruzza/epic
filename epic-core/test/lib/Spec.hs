import qualified Spec.LinesOfText as LinesOfText
import qualified Spec.Match       as Match
import qualified Spec.Patch       as Patch
import qualified Spec.Streaming   as Streaming
import qualified Spec.Surface     as Surface

import Test.Tasty


main :: IO ()
main = defaultMain $
  testGroup "All tests" [
    Patch.tests
  , Match.tests
  , LinesOfText.tests
  , Surface.tests
  , Streaming.tests
  ]
