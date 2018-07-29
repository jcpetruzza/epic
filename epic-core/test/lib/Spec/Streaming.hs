module Spec.Streaming
  ( tests )

where

import LinesOfText ( LinesOfText )
import Streaming

import Arbitrary (PrintableText(..))
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import           Pipes
import qualified Pipes.Prelude
import           System.IO ( IOMode(..), hClose, withFile, )
import qualified System.IO.Temp as Temp

tests :: TestTree
tests = testGroup "Streaming"
  [ testOutputInputHunks
  ]


testOutputInputHunks :: TestTree
testOutputInputHunks
  = testProperty "Can input what was output" $
      forAll (listsOfLenAtMost 5) $ \hunks ->
        ioProperty $
          Temp.withSystemTempFile "test_output_input_hunks.hunks" $ \fp h -> do
            runEffect $
              Pipes.each hunks >-> Streaming.hOutputHunks @LinesOfText h

            hClose h

            readHunks <- withFile fp ReadMode $ \h' ->
              Pipes.Prelude.toListM $ Streaming.hInputHunks @LinesOfText h'

            pure $ hunks === readHunks


listsOfLenAtMost :: Arbitrary a => Int -> Gen [a]
listsOfLenAtMost n
  = do
      l <- choose (0, n)
      vectorOf l arbitrary
