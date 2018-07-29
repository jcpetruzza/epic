module LinesOfText
  ( -- * Conversion
    LinesOfText (..)
  , fromText
  , toText
  , fromLazyText
  , toLazyText

    -- * Properties
  , lastLineIncludesNewline
  , totalSpan
  , totalSpanFrom

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
import           Data.Semigroup             ( Semigroup(..) )
import           Data.String
import           Data.Text                  ( Text )
import qualified Data.Text                  as Text
import qualified Data.Text.Lazy             as LText
import qualified Data.Text.Lazy.Builder     as Builder
import qualified Data.Vector                as V

newtype LinesOfText
  = LinesOfText {asVectorOfText :: V.Vector Text} -- INVARIANT: Non-empty
  deriving (Eq, Show)

instance IsString LinesOfText where
  fromString =  fromText . Text.pack

fromText :: Text -> LinesOfText
fromText = LinesOfText . V.fromList . lines'
  where
    lines' = go
      where
        go t
          = case Text.findIndex ('\n' ==) t of
              Nothing -> [t]
              Just i ->
                let (l, t') = Text.splitAt (i+1) t
                in l : if Text.null t' then [] else go t'

fromLazyText :: LText.Text -> LinesOfText
fromLazyText = LinesOfText . V.fromList . map LText.toStrict . lines'
  where
    lines' = go
      where
        go t
          = let (l, t') = LText.break ('\n' ==) t
            in case LText.uncons t' of
                 Nothing -> [l]
                 Just (_, t'') ->
                   l `LText.snoc` '\n' : if LText.null t'' then [] else go t''

toText :: LinesOfText -> Text
toText
  = mconcat . V.toList . asVectorOfText

toLazyText :: LinesOfText -> LText.Text
toLazyText
  = mconcat . map LText.fromStrict . V.toList . asVectorOfText

instance Semigroup LinesOfText where
  (LinesOfText l) <> (LinesOfText r)
    = if lastLineIncludesNewline (LinesOfText l)
        then LinesOfText (l <> r)
        else LinesOfText $ mconcat
               [ V.init l
               , V.singleton (V.last l <> V.head r)
               , V.tail r
               ]

instance Monoid LinesOfText where
  mempty  = LinesOfText (V.singleton "")
  mappend = (<>)

-- | The last line will not always include a final newline.
lastLineIncludesNewline :: LinesOfText -> Bool
lastLineIncludesNewline (LinesOfText ls)
  = (snd <$> Text.unsnoc (V.last ls)) == Just '\n'

totalSpan :: LinesOfText -> Span
totalSpan
  = totalSpanFrom (RowCol 0 0)

-- | Return the 'Span' assuming the top-left character is at the given 'Loc'.
totalSpanFrom :: Loc -> LinesOfText -> Span
totalSpanFrom start lot@(LinesOfText ls)
  = Span
      { spanStart
          = start
      , spanEnd
          = if lastLineIncludesNewline lot
              then RowCol
                     { row = row start + V.length ls
                     , col = 0
                     }
              else RowCol
                     { row = row start + V.length ls - 1
                     , col = if isSingleLine
                               then Text.length (V.last ls) + col start
                               else Text.length (V.last ls)
                     }
      }
  where
    isSingleLine = V.length ls == 1


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
        then pure mempty
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