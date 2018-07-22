module LinesOfText
  ( -- * Conversion
    LinesOfText (..)
  , fromText
  , toText
  , fromLazyText
  , toLazyText

    -- * Fragments
  , textFragments
  , fromFragments


    -- * Fragmenter
  , Fragmenter
  , runFragmenter
  , fragmentUpTo
  )

where

import Match
import SrcLoc (Loc, RowCol(..), Span(..))

import           Control.Monad              ( when )
import           Control.Monad.Except       ( ExceptT, throwError, runExceptT )
import           Control.Monad.State.Strict ( State, evalState, get, put )
import qualified Data.HashMap.Strict        as HM
import qualified Data.List                  as List
import           Data.Maybe                 ( catMaybes )
import           Data.String
import           Data.Text                  ( Text )
import qualified Data.Text                  as Text
import qualified Data.Text.Lazy             as LText
import qualified Data.Text.Lazy.Builder     as Builder
import qualified Data.Vector                as V

newtype LinesOfText
  = LinesOfText {asVectorOfText :: V.Vector Text}
  deriving (Eq, Show)

instance IsString LinesOfText where
  fromString =  fromText . Text.pack

fromText :: Text -> LinesOfText
fromText = LinesOfText . V.fromList . Text.lines

fromLazyText :: LText.Text -> LinesOfText
fromLazyText = LinesOfText . V.fromList . map LText.toStrict . LText.lines


toText :: LinesOfText -> Text
toText
  = Text.unlines . V.toList . asVectorOfText

toLazyText :: LinesOfText -> LText.Text
toLazyText
  = LText.unlines . map LText.fromStrict . V.toList . asVectorOfText

instance Matchable LinesOfText where
  matched m lot
    = do
        lotFrags <- textFragments (matchFragments m) lot
        pure Matched
          { matchedMatch   = m
          , matchedContext = lot
          , matchedVars    = HM.fromList $ catMaybes $ map mgetMatched lotFrags
          , matchedApply   = \m' -> fromFragments $ map (mapFrag m') lotFrags
          }
    where
        mgetMatched = \case
          MatchedFragment x v -> pure (x, v)
          UnmatchedFragment{} -> Nothing

        mapFrag m' ori = case ori of
          MatchedFragment x _ -> maybe ori (MatchedFragment x) $ HM.lookup x m'
          UnmatchedFragment{} -> ori

instance Matchable Text where
  matched m
    = fmap (mapMatched toText fromText) . matched m . fromText

instance Matchable LText.Text where
  matched m
    = fmap (mapMatched toLazyText fromLazyText) . matched m . fromLazyText


textFragments
  :: [MatchFragment UpToLoc Span]
  -> LinesOfText
  -> Either OutOfRange [MatchFragment LinesOfText LinesOfText]
textFragments locFragments ls
  = runFragmenter (RowCol 0 0) ls $ mapM getFragment locFragments
  where
    getFragment = \case
      UnmatchedFragment upTo ->
        UnmatchedFragment <$> fragmentUpTo upTo

      MatchedFragment x s -> do
        expectToBeIn (spanStart s)
        MatchedFragment x <$> fragmentUpTo (LEqLoc $ spanEnd s)


fromFragments :: [MatchFragment LinesOfText LinesOfText] -> LinesOfText
fromFragments
  = fromLazyText . Builder.toLazyText . mconcat . map fragmentToBuilder
  where
    fragmentToBuilder = \case
      UnmatchedFragment lot -> linesOfTextToBuilder lot
      MatchedFragment _ lot -> linesOfTextToBuilder lot

    linesOfTextToBuilder =
      mconcat
        . List.intersperse (Builder.singleton '\n')
        . map Builder.fromText
        . V.toList
        . asVectorOfText


type Fragmenter
  = ExceptT OutOfRange (State FragmenterState)

data FragmenterState
  = FragmenterState !Loc !LinesOfText

runFragmenter :: Loc -> LinesOfText -> Fragmenter a -> Either OutOfRange a
runFragmenter loc ls
  = flip evalState (FragmenterState loc ls) . runExceptT

expectToBeIn :: Loc -> Fragmenter ()
expectToBeIn expectedLoc
  = do
      FragmenterState curLoc _ <- get
      when (curLoc /= expectedLoc) $
        throwError $ OutOfRange expectedLoc

fragmentUpTo :: UpToLoc -> Fragmenter LinesOfText
fragmentUpTo upTo
  = do
      FragmenterState curLoc (LinesOfText ls) <- get

      let lastRow = upToRow upTo
          sliceLen = lastRow - row curLoc + 1

      if sliceLen == 0
        then pure (LinesOfText V.empty)
        else do
          lastLine <- maybe outOfRange pure $ ls V.!? (sliceLen - 1)

          (incl, mexcl, newLoc) <- splitLastLine lastLine

          let fragment = (V.slice 0 (sliceLen - 1) ls) `V.snoc` incl
              sliceDropped = V.drop sliceLen ls
              rest = maybe sliceDropped (`V.cons` sliceDropped) mexcl

          put $ FragmenterState newLoc (LinesOfText rest)
          pure (LinesOfText fragment)
  where
    outOfRange
      = case upTo of
          LtLoc  loc -> throwError $ OutOfRange loc
          LEqLoc loc -> throwError $ OutOfRange loc


    splitLastLine lastLine
      = case upTo of
          LtLoc loc | col loc == 0 ->
            pure (lastLine, Nothing, loc)

          LtLoc loc ->
            splitBefore loc

          LEqLoc loc ->
            splitBefore (nextCol loc)
      where
        splitBefore loc = do
          FragmenterState curLoc _ <- get

          let inclLen
                | row curLoc == row loc = col loc - col curLoc
                | otherwise             = col loc

              (incl, excl) = Text.splitAt inclLen lastLine

          when (Text.compareLength incl inclLen /= EQ)
            outOfRange

          pure $! if Text.null excl
            then (incl, Nothing, nextLine loc)
            else (incl, Just excl, loc)

    nextLine loc
      = RowCol (row loc + 1) 0

    nextCol loc
      = loc{col = col loc + 1}