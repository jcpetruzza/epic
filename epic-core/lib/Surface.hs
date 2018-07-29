{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoDeriveAnyClass           #-}
{-# LANGUAGE RecordWildCards            #-}
module Surface
  ( Surface (..)

  , SurfaceBuilder
  , runSurfaceBuilderWith
  , emit
  )

where

import           Assignments
import           Hunk
import           LinesOfText (LinesOfText)
import qualified LinesOfText
import           SrcLoc (Located(..), RowCol(..), Span(..), Src(..))

import           Control.Monad (void, when)
import           Control.Monad.Reader ( ask, ReaderT, runReaderT )
import           Control.Monad.Trans ( lift )
import           Data.Attoparsec.Text       ((<?>))
import qualified Data.Attoparsec.Text       as P
import qualified Data.Char                  as Char
import           Data.Maybe (isJust)
import           Data.Semigroup ( Semigroup(..) )
import           Data.String ( IsString(..) )
import           Data.Text (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Lazy             as LText
import qualified Data.Text.Lazy.Builder     as LTB

-- | A monad to build surface representation by appending
--   fragments of 'Text' that are 'emit'ted
newtype SurfaceBuilder m a
  = SurfaceBuilder (ReaderT (Text -> m ()) m a)
  deriving ( Functor, Applicative, Monad )

-- | Add a new 'Text' fragment to the current surface representation
emit :: Monad m => Text -> SurfaceBuilder m ()
emit t = SurfaceBuilder $ do f <- ask; lift (f t)

runSurfaceBuilderWith :: (Text -> m ()) -> SurfaceBuilder m a -> m a
runSurfaceBuilderWith f (SurfaceBuilder b)
  = runReaderT b f

instance (Monad m, a ~ ()) => IsString (SurfaceBuilder m a) where
  fromString = emit . Text.pack

instance (Semigroup a, Applicative m) => Semigroup (SurfaceBuilder m a) where
  l <> r
    = (<>) <$> l <*> r

instance (Semigroup a, Monoid a, Applicative m) => Monoid (SurfaceBuilder m a) where
  mempty  = pure mempty
  mappend = (<>)

-- | Class of types with a surface representation
class Surface a where
  buildSurface :: Monad m => a -> SurfaceBuilder m ()
  parseSurface :: P.Parser a


instance Surface Int where
  buildSurface = fromString . show
  parseSurface = P.signed P.decimal

instance a ~ Int => Surface (RowCol a) where
  buildSurface RowCol{..}
    = buildSurface row <> chr ',' <> buildSurface col

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
                  acc' = acc <> emitLine l
              in case LText.uncons t' of
                   Nothing -> (acc', True)
                   Just (_, t'')
                     | LText.null t'' -> (acc', False)
                     | otherwise      -> go acc' t''

      emitLine l
        = chr '>' <> emit (LText.toStrict l) <> chr '\n'

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
                go $ acc <> LTB.singleton '\n' <> LTB.fromText line
              else do
                lastLineHadNoEOL <- ('-' ==) <$> P.peekChar'
                when lastLineHadNoEOL $
                  void $ P.char '-'
                void (P.satisfy P.isEndOfLine <?> "\\n")
                if lastLineHadNoEOL
                  then pure acc
                  else pure $ acc <> LTB.singleton '\n'

      isEOT = \case
        '>' -> pure False
        '#' -> pure True
        _   -> Nothing  -- bad input


instance Surface Assignments.Var where
  buildSurface (Assignments.Var x)
    = emit x

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
          [ chr '&', fromString $ srcFilename $ location $ hunkLines, chr '\n'
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
chr :: Monad m => Char -> SurfaceBuilder m ()
chr = emit . Text.singleton

acceptLine :: P.Parser Text
acceptLine
  = (P.takeTill (P.isEndOfLine) <* P.endOfLine) <?> "line with trailing \\n"


satisfyMaybe :: (Char -> Maybe a) -> P.Parser a
satisfyMaybe classify
  = do Just a <- P.satisfyWith classify isJust
       pure a

-- {{ Utilities ----------------------------------------------------------------
