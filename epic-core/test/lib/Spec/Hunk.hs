module Spec.Hunk
  ( tests
  )

where

import Assignments
import Hunk
import LinesOfText
import SrcLoc (coveredBy, Span(..), relativeTo)


import Arbitrary (validHunkSpans)
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests = testGroup "Hunk"
  [ testArbitraryInstance
  , testFocus
  ]

testArbitraryInstance :: TestTree
testArbitraryInstance = testGroup "Arbitrary instance sanity"
  [
  testProperty "hunk span" $ \h ->
      let s = hunkSpan h
      in LinesOfText.totalSpan (hunkBody h) == Just (relativeTo (spanStart s) s)

  , testProperty "implicit vals span" $ \h ->
      let valSpans
            = [s | (_, ImplicitVal s) <- Assignments.toList (hunkAssigns h)]
          _ = h :: Hunk LinesOfText
      in all (`coveredBy` hunkSpan h) valSpans

  ]


testFocus :: TestTree
testFocus = testGroup "focus"
  [ testProperty "Self focus" $ \h ->
      focus (hunkSpan h) h === pure h

  , testProperty "Reduces span" $ \h ->
      forAll (validHunkSpans h) $ \s ->
        (hunkSpan <$> focus s h) === pure s

  , testProperty "Reduces body" $ \h ->
      forAll (validHunkSpans h) $ \s ->
        let bodySpan = LinesOfText.totalSpan . hunkBody
        in (bodySpan <$> focus s h) === pure (Just (relativeTo (spanStart s) s))

  , testProperty "Preserves var values" $ \h ->
      forAll (validHunkSpans h) $ \s ->
        let vars = hunkVars h ++ ["dummyVar"]
        in forAll (elements vars) $ \v ->
             (hunkVarValues v <$> focus s h) === pure (hunkVarValues v h)

  , testProperty "Preserves covered spans" $ \h ->
      forAll (validHunkSpans h) $ \s ->
        let vars = hunkVars h ++ ["dummyVar"]
            coveredVarSpans v = filter (`coveredBy` s) . hunkVarSpans v
        in forAll (elements vars) $ \v ->
             (hunkVarSpans v <$> focus s h) === pure (coveredVarSpans v h)

  ]
