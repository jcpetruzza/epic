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
import Data.Semigroup

tests :: TestTree
tests = testGroup "LinesOfText"
  [ testFragmentUpTo
  , testTextFragments
  , testToFromText
  , testMonoid
  , testSpans
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

testMonoid :: TestTree
testMonoid = testGroup "Monoid instance"
  [ testProperty "mempty <> l = l" $ \l ->
      mempty @LinesOfText <> l == l

  , testProperty "l <> mempty = l" $ \l ->
      l <> mempty @LinesOfText == l

  , testProperty "(a <> b) <> c = a <> (b <> c)" $ \a b c ->
      (a <> b) <> c == a <> (b <> c :: LinesOfText)

  , testProperty "toText l <> toText r = toText (l <> r)" $ \l r ->
      toText l <> toText r == toText (l <> r)
  ]

testSpans :: TestTree
testSpans = testGroup "Span calculations"
  [ testProperty "Span under concatenation" $ \l r s0 ->
      let sl  = LinesOfText.totalSpanFrom s0 l
          sr  = LinesOfText.totalSpanFrom (spanEnd sl) r
          slr = Span {spanStart = spanStart sl, spanEnd = spanEnd sr}
      in LinesOfText.totalSpanFrom s0 (l <> r) == slr
  ]
