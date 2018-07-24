{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RecordWildCards #-}
module Surface
  ( Surface (..)
  )

where

import           Assignments
import           Hunk
import           LinesOfText (LinesOfText)
import qualified LinesOfText
import           SrcLoc (Located(..), RowCol(..), Span(..), Src(..))

import           Control.Monad (void, when)
import           Data.Attoparsec.Text       ((<?>))
import qualified Data.Attoparsec.Text       as P
import qualified Data.Char                  as Char
import           Data.Maybe (isJust)
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Lazy             as LText
import qualified Data.Text.Lazy.Builder     as LTB
import qualified Data.Text.Lazy.Builder.Int as LTB

-- | Class of types with a surface representation
class Surface a where
  buildSurface :: a -> LTB.Builder
  parseSurface :: P.Parser a


instance Surface Int where
  buildSurface = LTB.decimal
  parseSurface = P.signed P.decimal

instance a ~ Int => Surface (RowCol a) where
  buildSurface RowCol{..}
    = buildSurface row <> LTB.singleton ',' <> buildSurface col

  parseSurface
    = do
        row <- parseSurface
        void $ P.char ','
        col <- parseSurface
        pure RowCol{..}


instance Surface Span where
  buildSurface Span{..}
    = buildSurface spanStart <> chr '-' <> buildSurface spanEnd

  parseSurface
    = do
        spanStart <- parseSurface
        void $ P.char '-'
        spanEnd <- parseSurface
        pure Span{..}

instance Surface LinesOfText where
  buildSurface
    = buildSurface . LinesOfText.toLazyText

  parseSurface
    = LinesOfText.fromLazyText <$> parseSurface

instance Surface Text where
  buildSurface
    = buildSurface . LText.fromStrict

  parseSurface
    = LText.toStrict <$> parseSurface

instance Surface LText.Text where
  buildSurface txt
    = builtSurface
        <> chr '#'
        <> (if lastEndsMidLine then chr '-' else mempty)
        <> chr '\n'
    where
      (builtSurface, lastEndsMidLine) = go mempty txt
        where
          go acc t
            = let (l,t') = LText.break ((==) '\n') t
                  acc' = acc <> toBuilder l
              in case LText.uncons t' of
                   Nothing -> (acc', True)
                   Just (_, t'')
                     | LText.null t'' -> (acc', False)
                     | otherwise      -> go acc' t''

      toBuilder l
        = chr '>' <> LTB.fromLazyText l <> chr '\n'

  parseSurface
    = LTB.toLazyText <$> do
        void $ P.char '>'
        firstLine <- acceptLine
        go (LTB.fromText firstLine)
    where
      go (!acc)
        = do
            eot <- satisfyMaybe isEOT <?> "> or #"
            if not eot
              then do
                line <- acceptLine
                go $ acc <> chr '\n' <> LTB.fromText line
              else do
                lastLineHadNoEOL <- ('-' ==) <$> P.peekChar'
                when lastLineHadNoEOL $
                  void $ P.char '-'
                void (P.satisfy P.isEndOfLine <?> "\\n")
                if lastLineHadNoEOL
                  then pure acc
                  else pure $ acc <> chr '\n'

      isEOT = \case
        '>' -> pure False
        '#' -> pure True
        _   -> Nothing  -- bad input


instance Surface Assignments.Var where
  buildSurface (Assignments.Var x)
    = LTB.fromText x

  parseSurface
    = do
        initialChar <- P.peekChar'
        when (not $ Char.isAlpha initialChar || initialChar == '_') $
          fail "letter or _"

        Assignments.Var <$> (
            P.takeWhile1 (\c -> Char.isAlphaNum c || c == '_')
              <?> "letter, digit or _"
          )


instance Surface Assignments.Val where
  buildSurface = \case
    ExplicitVal t -> "#\n"   <> buildSurface t
    ImplicitVal s -> chr '@' <> buildSurface s <> chr '\n'

  parseSurface
    = do
        isExplicit <- satisfyMaybe mIsExplicit <?> "@ or #"
        if isExplicit
          then ExplicitVal <$> (P.endOfLine *> parseSurface)
          else ImplicitVal <$> (parseSurface <* P.endOfLine)
    where
      mIsExplicit = \case
        '#' -> pure True
        '@' -> pure False
        _   -> Nothing


instance Surface Assignments where
  buildSurface ms
    = foldMap surfaceArrow (reverse $ Assignments.toList ms)
    where
      surfaceArrow (x, v)
        = chr '!' <> buildSurface x <> " -> " <> buildSurface v

  parseSurface
    = Assignments.fromList <$> P.many' parseArrow
    where
      parseArrow
        = do void $ P.char '!'
             var <- parseSurface
             void $ P.string " -> "
             val <- parseSurface
             pure (var, val)



instance Surface a => Surface (Hunk a) where
  buildSurface Hunk{..}
    = surfaceHunkLoc <> surfaceText <> buildSurface hunkVars
    where
    surfaceHunkLoc
      = mconcat
          [ chr '&', LTB.fromString $ srcFilename $ location $ hunkLines, chr '\n'
          , chr '@', buildSurface $ src $ location $ hunkLines, chr '\n'
          ]

    surfaceText
      = buildSurface (element hunkLines)

  parseSurface
    = do
        fileName <- P.char '&' *> acceptLine
        span_ <- P.char '@' *> parseSurface <* P.endOfLine
        text <- parseSurface
        assgns <- parseSurface
        pure $ mkHunk (Text.unpack fileName) span_ assgns text



-- {{ Utilities ----------------------------------------------------------------
chr :: Char -> LTB.Builder
chr = LTB.singleton

acceptLine :: P.Parser Text
acceptLine
  = (P.takeTill (P.isEndOfLine) <* P.endOfLine) <?> "line with trailing \\n"


satisfyMaybe :: (Char -> Maybe a) -> P.Parser a
satisfyMaybe classify
  = do Just a <- P.satisfyWith classify isJust
       pure a

-- {{ Utilities ----------------------------------------------------------------
