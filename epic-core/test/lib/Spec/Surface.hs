{-# LANGUAGE AllowAmbiguousTypes #-}
module Spec.Surface
  ( tests
  )

where

import Assignments
import Hunk
import LinesOfText
import Surface
import SrcLoc (RowCol(..), Span(..))

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Arbitrary ()

import           Control.Monad.Writer.Strict ( execWriter, tell )
import qualified Data.Attoparsec.Text   as P
import           Data.Either            (isLeft)
import qualified Data.Text              as Text
import qualified Data.Text.Lazy         as LText
import qualified Data.Text.Lazy.Builder as LTB

tests :: TestTree
tests = testGroup "Surface"
  [ testGroup "RowCol"
      [ testCase "Surface example" $
          RowCol 5 3 `hasSurface` "5,3"
      , roundtrips @(RowCol Int)
      ]


  , testGroup "Span"
      [ testCase "Surface example" $
          mkSpan (5,3) (7,0) `hasSurface` "5,3-7,0"
      , roundtrips @Span
      ]

  , testGroup "Text" $
      [ testCase "Surface empty text" $
          txt "" `hasSurface` ">\n#-\n"
      , testCase "Surface single \\n" $
          txt "\n" `hasSurface` ">\n#\n"
      , testCase "Surface two lines ending in \\n" $
          txt "foo\nbar\n" `hasSurface` ">foo\n>bar\n#\n"
      , testCase "Surface two lines ending with no \\n" $
          txt "foo\nbar" `hasSurface` ">foo\n>bar\n#-\n"
      , roundtrips' (Text.pack . getPrintableString)
      ]

  , testGroup "Assignments.Val" $
      [ testCase "Surface implicit" $
          ImplicitVal (mkSpan (5,3) (7,0))
            `hasSurface` "@5,3-7,0\n"

      , testCase "Surface explicit" $
          ExplicitVal "foo\nbar"
            `hasSurface`  "#\n>foo\n>bar\n#-\n"

      , roundtrips @Assignments.Val
      ]

  , testGroup "Assignments.Var" $
      [ testCase "Surface var" $
          Assignments.Var "fooBar_99" `hasSurface` "fooBar_99"

      , testCase "Surface var starts with _" $
          Assignments.Var "_fooBar_99" `hasSurface` "_fooBar_99"

      , testCase "Can't start with #" $
          wontParse @Assignments.Var "#foo"

      , testCase "Can't start with >" $
          wontParse @Assignments.Var ">foo"

      , testCase "Can't start with @" $
          wontParse @Assignments.Var "@foo"

      , testCase "Can't start with digit" $
          wontParse @Assignments.Var "9foo"

      , roundtrips @Assignments.Var
      ]

  , testGroup "Assignments" $
      [ testCase "Surface empty" $
          Assignments.empty `hasSurface` ""

      , testCase "Surface singleton implicit" $
          Assignments.fromList [("X", ImplicitVal $ mkSpan (5,3) (7,0))]
            `hasSurface` "!X -> @5,3-7,0\n"

      , testCase "Surface singleton explicit" $
          Assignments.fromList [("X", ExplicitVal "foo")]
            `hasSurface` "!X -> #\n>foo\n#-\n"

      , testCase "Surface many elem" $
          Assignments.fromList
            [ ("X", ExplicitVal "foo")
            , ("X", ImplicitVal $ mkSpan (5,3) (7,0))
            , ("X", ExplicitVal "")
            ]
            `hasSurface`
              "!X -> #\n>foo\n#-\n!X -> @5,3-7,0\n!X -> #\n>\n#-\n"

      , roundtrips @Assignments
      ]


  , testGroup "Hunk" $
      [ testCase "No assign, no text" $
          mkHunk "./foo.c" (mkSpan (0,5) (3,1)) mempty Text.empty
           `hasSurface`
             "&./foo.c\n@0,5-3,1\n>\n#-\n"

      , testCase "Assign, no text" $
          mkHunk "./foo.c" (mkSpan (0,5) (3,1)) (Assignments.fromList [("X", "foo")]) Text.empty
           `hasSurface`
             "&./foo.c\n@0,5-3,1\n>\n#-\n!X -> #\n>foo\n#-\n"

      , testCase "Assign, text" $
          mkHunk "./foo.c" (mkSpan (0,5) (3,1)) (Assignments.fromList [("X", "foo")]) (txt "bar\nfighters\n")
           `hasSurface`
             "&./foo.c\n@0,5-3,1\n>bar\n>fighters\n#\n!X -> #\n>foo\n#-\n"

      , roundtrips @(Hunk LinesOfText)
      ]

  ]
  where
    txt = Text.pack

toLazyTextSurface :: Surface a => a -> LText.Text
toLazyTextSurface
  = LTB.toLazyText
      . execWriter
      . runSurfaceBuilderWith (tell . LTB.fromText)
      . buildSurface

hasSurface :: Surface a => a -> String -> Assertion
x `hasSurface` s
  = toLazyTextSurface x @?= LText.pack s

wontParse :: forall a . Surface a => String -> Assertion
wontParse s
  = assertBool "Left _ expected" $
      isLeft $ P.parseOnly @a (parseSurface <* P.endOfInput) (Text.pack s)

roundtrips :: forall a . (Surface a, Eq a, Show a, Arbitrary a) => TestTree
roundtrips = roundtrips' (id @a)


roundtrips'
  :: forall a b
  .  (Surface a, Eq a, Show a, Arbitrary b, Show b)
  => (b -> a)
  -> TestTree
roundtrips' f =
  testProperty "Roundtrips" $ \b ->
    let a       = f b
        encoded = LText.toStrict $ toLazyTextSurface a
        decoded = P.parseOnly (parseSurface <* P.endOfInput) encoded
    in decoded === Right a

mkSpan :: (Int, Int) -> (Int, Int) -> Span
mkSpan (x,y) (z,w)
  = Span (RowCol x y) (RowCol z w)
