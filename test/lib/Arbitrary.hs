{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards      #-}
module Arbitrary
  ( PrintableText(..)
  )

where

import Assignments
import Hunk
import LinesOfText
import SrcLoc (Located(..), RowCol(..), Span(..), Src(..))

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
    = Src
        <$> (FilePath.joinPath <$> listOf1 fileName)
        <*> arbitrary
    where
      validPathChar
        = elements $ ['a'..'z'] ++ ['0'..'9'] ++ ['-', '_', '.']

      fileName
        = listOf1 validPathChar

instance CoArbitrary a => CoArbitrary (Src a) where
  coarbitrary (Src x y)
    = coarbitrary x . coarbitrary y

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
    ExplicitVal x -> variant 0 . coarbitrary (Text.unpack x)
    ImplicitVal x -> variant 1 . coarbitrary x

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
instance Arbitrary a => Arbitrary (Hunk a) where
  arbitrary
    = Hunk <$> arbitrary <*> arbitrary

instance CoArbitrary a => CoArbitrary (Hunk a) where
  coarbitrary (Hunk x y)
    = coarbitrary x . coarbitrary y

-- }} Hunk ---------------------------------------------------------------------
