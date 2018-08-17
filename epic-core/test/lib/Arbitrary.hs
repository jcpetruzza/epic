{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards      #-}
module Arbitrary
  ( PrintableText(..)
  , validHunkSpans
  )

where

import Assignments
import Hunk
import LinesOfText
import SrcLoc (addRows, Located(..), relocateTo, RowCol(..), Span(..), Src(..))

import           Control.Monad ( guard )
import qualified Data.Text as Text
import qualified System.FilePath as FilePath
import Test.Tasty.QuickCheck

nat :: Gen Int
nat = getNonNegative <$> arbitrary

-- {{ PrintableText ------------------------------------------------------------
newtype PrintableText
  = PrintableText { getPrintableText :: Text.Text }
  deriving (Eq)

instance Show PrintableText where
  show = show . getPrintableText

instance Arbitrary PrintableText where
  arbitrary = PrintableText . Text.pack . getPrintableString <$> arbitrary

instance CoArbitrary PrintableText where
  coarbitrary (PrintableText t)
    = coarbitrary (Text.unpack t)
-- }} PrintableText ------------------------------------------------------------

-- {{ RowCol -------------------------------------------------------------------
instance Arbitrary (RowCol Int) where
  arbitrary = RowCol <$> nat <*> nat

instance CoArbitrary (RowCol Int) where
  coarbitrary RowCol{..} = coarbitrary row . coarbitrary col
-- }} RowCol -------------------------------------------------------------------


-- {{ Span ---------------------------------------------------------------------
instance Arbitrary Span where
  arbitrary
    = do
        spanStart <- arbitrary
        relSpanEnd <- arbitrary
        let spanEnd
              = RowCol
                  { row = row spanStart + row relSpanEnd
                  , col = if row relSpanEnd == 0
                            then col spanStart + col relSpanEnd
                            else col relSpanEnd
                  }
        pure Span{..}

instance CoArbitrary Span where
  coarbitrary Span{..}
    = coarbitrary spanStart . coarbitrary spanEnd

-- }} Span ---------------------------------------------------------------------

-- {{ Src ---------------------------------------------------------------------
instance Arbitrary a => Arbitrary (Src a) where
  arbitrary
    = Src <$> genFilePath <*> arbitrary

instance CoArbitrary a => CoArbitrary (Src a) where
  coarbitrary (Src x y)
    = coarbitrary x . coarbitrary y

genFilePath :: Gen FilePath
genFilePath
  = FilePath.joinPath <$> listOf1 fileName
  where
    validPathChar
      = elements $ ['a'..'z'] ++ ['0'..'9'] ++ ['-', '_', '.']

    fileName
      = listOf1 validPathChar

-- }} Src ---------------------------------------------------------------------

-- {{ Located ------------------------------------------------------------------
instance Arbitrary a => Arbitrary (Located a) where
  arbitrary
    = Located <$> arbitrary <*> arbitrary

instance CoArbitrary a => CoArbitrary (Located a) where
  coarbitrary (Located x y)
    = coarbitrary x . coarbitrary y
-- }} Located ------------------------------------------------------------------


-- {{ Assignments.Val ----------------------------------------------------------
instance Arbitrary Assignments.Val where
  arbitrary
    = oneof
        [ ExplicitVal . getPrintableText <$> arbitrary
        , ImplicitVal <$> arbitrary
        ]

instance CoArbitrary Assignments.Val where
  coarbitrary = \case
    ExplicitVal x -> variant (0 :: Int) . coarbitrary (Text.unpack x)
    ImplicitVal x -> variant (1 :: Int) . coarbitrary x

-- }} Assignments.Val ----------------------------------------------------------


-- {{ Assignments.Var ----------------------------------------------------------
instance Arbitrary Assignments.Var where
  arbitrary
    = do
        c <- oneof [upper, lower, under]
        rest <- listOf $ oneof [upper, lower, digit, under]
        pure $ Assignments.Var $ Text.pack $ c:rest
    where
      digit = choose ('0', '9')
      upper = choose ('A', 'Z')
      lower = choose ('a', 'z')
      under = pure '_'

instance CoArbitrary Assignments.Var where
  coarbitrary (Assignments.Var x)
    = coarbitrary $ Text.unpack x
-- }} Assignments.Var ----------------------------------------------------------


-- {{ Assignments --------------------------------------------------------------
instance Arbitrary Assignments where
  arbitrary
    = Assignments.fromList <$> arbitrary

instance CoArbitrary Assignments where
  coarbitrary
    = coarbitrary . Assignments.toList
-- }} Assignments --------------------------------------------------------------


-- {{ LinesOfText --------------------------------------------------------------
instance Arbitrary LinesOfText where
  arbitrary
    = LinesOfText.fromText . getPrintableText <$> arbitrary

instance CoArbitrary LinesOfText where
  coarbitrary
    = coarbitrary . Text.unpack . LinesOfText.toText
-- }} LinesOfText --------------------------------------------------------------

-- {{ Hunk ---------------------------------------------------------------------
instance (Arbitrary a, IsLinesOfText a) => Arbitrary (Hunk a) where
  arbitrary
    = do
        let asNonEmptyLot a
              = do let lot = toLinesOfText a
                   guard (not $ LinesOfText.isEmpty lot)
                   pure (a, lot)

        (a, lot) <- arbitrary `suchThatMap` asNonEmptyLot
        offset <- nat
        let
          lastLine
            = numberOfLines lot - 1
            + fromEnum (lastLineIncludesNewline lot)

          spanInside
            = do
                r0 <- choose (0, lastLine)
                c0 <- choose (0, LinesOfText.numberOfColsAt r0 lot - 1)
                r1 <- choose (r0, lastLine)
                c1 <- if r1 == r0
                        then choose (c0, LinesOfText.numberOfColsAt r1 lot - 1)
                        else choose (0,  LinesOfText.numberOfColsAt r1 lot - 1)
                pure $ offsetSpan $ Span (RowCol r0 c0) (RowCol r1 c1)

          genVal
            = oneof
                [ ExplicitVal . getPrintableText <$> arbitrary
                , ImplicitVal <$> spanInside
                ]

          offsetSpan
            = addRows offset

          Just lotSpan
            = LinesOfText.totalSpan lot

        src <- Src <$> genFilePath <*> pure (offsetSpan lotSpan)
        ass <- Assignments.fromList <$> listOf ((,) <$> arbitrary <*> genVal)

        pure $ Hunk ass (Located src a)

instance CoArbitrary a => CoArbitrary (Hunk a) where
  coarbitrary (Hunk x y)
    = coarbitrary x . coarbitrary y

validHunkSpans :: IsLinesOfText a => Hunk a -> Gen Span
validHunkSpans h
  = do
      let getPos = choose (0, numPos - 1)
      (a, b) <- (,) <$> getPos <*> getPos

      let startPos = min a b
          endPos   = max a b
      pure $
        relocateTo (spanStart $ hunkSpan h)$
          Span (allPos !! startPos) (allPos !! endPos)

  where
    lot
      = toLinesOfText (hunkBody h)

    numPos
      = sum
          [ LinesOfText.numberOfColsAt r lot
          | r <- [0..LinesOfText.numberOfLines lot - 1]
          ]

    allPos
      = [ RowCol r c
        | r <- [0..LinesOfText.numberOfLines lot - 1]
        , c <- [0..LinesOfText.numberOfColsAt r lot - 1]
        ]
-- }} Hunk ---------------------------------------------------------------------
