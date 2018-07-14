{-# LANGUAGE OverloadedStrings #-}
module Spec.LinesOfText
  ( tests
  )

where

import LinesOfText
import Match (UpToLoc(..), MatchFragment(..))
import SrcLoc (RowCol(..), Span(..))

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Vector as V

tests :: TestTree
tests = testGroup "LinesOfText"
  [ testFragmentUpTo
  , testTextFragments
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
        ["", "h", "ello, world", "fifafufa\nboooom\np"]

  , testCase "Right-end" $
      runTest
        [ fragmentUpTo (LtLoc  (RowCol 0 11))
        , fragmentUpTo (LEqLoc (RowCol 0 11))
        , fragmentUpTo (LEqLoc (RowCol 1 7))
        ]
        "hello, world\nfifafufa\nboooom\npim"
        ["hello, worl", "d", "fifafufa"]
  ]
  where
    runTest frags input expected
      = let actual = runFragmenter (RowCol 0 0) input (sequence frags)
        in actual @?= (Right expected)


testTextFragments :: TestTree
testTextFragments = testGroup "textFragments/fromFragments"
  [ testCase "Simple case" $ do
      let initial = "hello, world\nfifafufa\nboooom\npim"

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
              , UnmatchedFragment "im"
              ]

      actual @?= Right expected
      (fromFragments <$> actual) @?= Right initial
  ]
