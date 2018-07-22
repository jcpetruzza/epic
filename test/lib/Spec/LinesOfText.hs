{-# LANGUAGE OverloadedStrings #-}
module Spec.LinesOfText
  ( tests
  )

where

import LinesOfText
import Match (UpToLoc(..), MatchFragment(..))
import SrcLoc (RowCol(..), Span(..))

import Arbitrary (PrintableText(..))
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import qualified Data.Text.Lazy as LText
import qualified Data.Vector as V

tests :: TestTree
tests = testGroup "LinesOfText"
  [ testFragmentUpTo
  , testTextFragments
  , testToFromText
  ]

testFragmentUpTo :: TestTree
testFragmentUpTo = testGroup "fragmentUpTo"
  [ testCase "Same line" $
      runTest
        [ fragmentUpTo (LtLoc  (RowCol 0 3))
        , fragmentUpTo (LEqLoc (RowCol 0 7))
        ]
        "hello, world\nfifafufa"
        ["hel", "lo, w"]

  , testCase "Multi-line" $
      runTest
        [ fragmentUpTo (LtLoc  (RowCol 1 3))
        , fragmentUpTo (LEqLoc (RowCol 3 1))
        ]
        "hello, world\nfifafufa\nboooom\npim"
        ["hello, world\nfif", "afufa\nboooom\npi"]

  , testCase "Left-end" $
      runTest
        [ fragmentUpTo (LtLoc  (RowCol 0 0))
        , fragmentUpTo (LEqLoc (RowCol 0 0))
        , fragmentUpTo (LtLoc  (RowCol 1 0))
        , fragmentUpTo (LEqLoc (RowCol 3 0))
        ]
        "hello, world\nfifafufa\nboooom\npim"
        ["", "h", "ello, world\n", "fifafufa\nboooom\np"]

  , testCase "Right-end" $
      runTest
        [ fragmentUpTo (LtLoc  (RowCol 0 11))
        , fragmentUpTo (LEqLoc (RowCol 0 11))
        , fragmentUpTo (LEqLoc (RowCol 1 8))
        ]
        "hello, world\nfifafufa\nboooom\npim"
        ["hello, worl", "d", "\nfifafufa\n"]
  ]
  where
    runTest frags input expected
      = let actual = runFragmenter (RowCol 0 0) input (sequence frags)
        in actual @?= (Right expected)


testTextFragments :: TestTree
testTextFragments = testGroup "textFragments/fromFragments"
  [ testCase "Simple case" $ do
      let initial = "hello, world\nfifafufa\nboooom\npim\n"

          actual
            = textFragments
                [ UnmatchedFragment (LtLoc  $ RowCol 1 1)
                , MatchedFragment "X" (Span (RowCol 1 1) (RowCol 2 2))
                , MatchedFragment "Y" (Span (RowCol 2 3) (RowCol 3 0))
                , UnmatchedFragment (LtLoc $ RowCol 4 0)
                ]
                initial
          expected
            = [ UnmatchedFragment "hello, world\nf"
              , MatchedFragment "X" "ifafufa\nboo"
              , MatchedFragment "Y" "oom\np"
              , UnmatchedFragment "im\n"
              ]

      actual @?= Right expected
      (fromFragments <$> actual) @?= Right initial
  ]


testToFromText :: TestTree
testToFromText = testGroup "Conversions to text"
  [ testProperty "fromText . toText = id" $ \(PrintableText t) ->
      (LinesOfText.toText $ LinesOfText.fromText t) === t

  , testProperty "fromLazyText . toLazyText = id" $ \(PrintableText st) ->
      let t = LText.fromStrict st
      in (LinesOfText.toLazyText $ LinesOfText.fromLazyText t) === t
  ]
