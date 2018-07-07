module Spec.Match
  ( tests
  )

where

import Match
import SrcLoc (RowCol(..), Span(..))

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty  as ListNE

tests :: TestTree
tests = testGroup "Match"
  [ testBuildMatch
  , testMatchFragments
  ]

testBuildMatch :: TestTree
testBuildMatch = testGroup "Construction"
  [ testCase "Union empty" $ do
      let
        sl = mkSpan 2 3 5 6
        sr = mkSpan 1 2 3 4

      match <- expectRight $ matchNoVar sl `matchUnion` matchNoVar sr

      matchSpan match @?= Span (spanStart sr) (spanEnd sl)

  , testCase "Union of disjoint" $ do
      let
        spanX = mkSpan 2 3 5 1
        spanY = mkSpan 7 0 7 7

      match <- expectRight $ matchVar "X" spanX `matchUnion` matchVar "Y" spanY

      matchSpan match @?= Span (spanStart spanX) (spanEnd spanY)
      HM.lookup "X" (matchVars match) @?= Just spanX
      HM.lookup "Y" (matchVars match) @?= Just spanY

  , testCase "Union of overlapping not ok" $ do
      let
        spanX = mkSpan 2 3 5 1
        spanY = mkSpan 4 0 7 7
        match = matchVar "X" spanX `matchUnion` matchVar "Y" spanY

      match @?= Left (MatchingVarsOverlap "X" spanX "Y" spanY)

  , testCase "Union of dup same ok" $ do
      let
        matchX = matchVar "X" (mkSpan 2 3 5 1)
        matchY = matchVar "Y" (mkSpan 7 0 7 7)

      match  <- expectRight $ matchX `matchUnion` matchY
      match' <- expectRight $ match  `matchUnion` matchX

      match' @?= match

  , testCase "Union of dup overlapping not ok" $ do
      let
        spanX  = mkSpan 2 3 5 1
        spanX' = mkSpan 2 3 5 2
        matchX = matchVar "X" spanX `matchUnion` matchVar "X" spanX'

      matchX @?= Left (MatchingVarTwoValues "X" spanX spanX')

   , testCase "Union of dup disjoint not ok" $ do
      let
        spanX  = mkSpan 2 3 5 1
        spanX' = mkSpan 9 0 9 9
        matchX = matchVar "X" spanX `matchUnion` matchVar "X" spanX'

      matchX @?= Left (MatchingVarTwoValues "X" spanX spanX')
  ]

testMatchFragments :: TestTree
testMatchFragments = testGroup "matchFragments"
  [ testCase "Empty match" $ do
      let s = mkSpan 1 1 2 3
      matchFragments (matchNoVar s) @?= [UnmatchedFragment (LEqLoc $ spanEnd s)]

  , testCase "Single var" $ do
      let s = mkSpan 1 1 2 3
      matchFragments (matchVar "X" s) @?= [MatchedFragment "X" s]

  , testCase "Consecutive vars" $ do
      let
        spanX   = mkSpan 1 1 2 3
        spanY   = mkSpan 2 4 3 5
        spanAll = mkSpan 0 0 6 6

      match <- expectRight $
                 matchUnions $ nonEmpty
                   [matchNoVar spanAll, matchVar "X" spanX, matchVar "Y" spanY]

      matchFragments match @?= [
          UnmatchedFragment (LtLoc $ spanStart spanX)
        , MatchedFragment "X" spanX
        , MatchedFragment "Y" spanY
        , UnmatchedFragment (LEqLoc $ spanEnd spanAll)
        ]

  , testCase "Non-consecutive vars" $ do
      let
        spanX   = mkSpan 1 1 2 3
        spanY   = mkSpan 2 9 3 5
        spanAll = mkSpan 0 0 6 6

      match <- expectRight $
                 matchUnions $ nonEmpty
                   [matchNoVar spanAll, matchVar "X" spanX, matchVar "Y" spanY]

      matchFragments match @?= [
          UnmatchedFragment (LtLoc $ spanStart spanX)
        , MatchedFragment "X" spanX
        , UnmatchedFragment (LtLoc $ spanStart spanY)
        , MatchedFragment "Y" spanY
        , UnmatchedFragment (LEqLoc $ spanEnd spanAll)
        ]
  ]

mkSpan :: Int -> Int -> Int -> Int -> Span
mkSpan r1 c1 r2 c2
  = Span (RowCol r1 c1) (RowCol r2 c2)

expectRight :: Show e => Either e a -> IO a
expectRight = \case
  Left  e -> assertFailure $ "UNEXPECTED Left: " ++ show e
  Right a -> pure a

nonEmpty :: [a] -> ListNE.NonEmpty a
nonEmpty = ListNE.fromList
