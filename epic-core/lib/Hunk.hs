module Hunk
  ( Hunk(..)
  , mkHunk
  , hunkFilename
  , hunkSpan
  , hunkBody
  , hunkVars, hunkVarValues, hunkVarSpans
  , focus
  )

where

import Assignments
import LinesOfText (IsLinesOfText(..), LinesOfText, UpToLoc(..), OutOfRange(..))
import qualified LinesOfText
import SrcLoc (coveredBy, Located(..), Span(..), Src(..))

import Control.Monad (void, when)
import Control.Monad.Except (Except, liftEither, MonadError, throwError)
import Data.Text (Text)

-- | A fragment of a file, perhaps with some assignmentsa.
data Hunk a
  = Hunk
      { hunkAssigns :: Assignments
      , hunkLines   :: Located a
      }
  deriving (Eq, Show, Functor)

mkHunk :: FilePath -> Span -> Assignments -> a -> Hunk a
mkHunk fp loc ms a
  = Hunk
      { hunkAssigns = ms
      , hunkLines   = Located (Src fp loc) a
      }

hunkFilename :: Hunk a -> FilePath
hunkFilename
  = srcFilename . location . hunkLines

hunkSpan :: Hunk a -> Span
hunkSpan
  = src . location . hunkLines

hunkVars :: Hunk a -> [Assignments.Var]
hunkVars
  = fmap fst . Assignments.toList . hunkAssigns

hunkVarValues :: IsLinesOfText a => Assignments.Var -> Hunk a -> [Text]
hunkVarValues x h
  = getValue <$> Assignments.lookup x (hunkAssigns h)
  where
    getValue = \case
      Assignments.ExplicitVal t ->
        t
      Assignments.ImplicitVal s ->
        case cut s (toLinesOfText <$> h) of
          Right lot -> LinesOfText.toText lot
          Left err -> error $ "hunkVarValues/bad implicit " ++ show err

hunkVarSpans :: Assignments.Var -> Hunk a -> [Span]
hunkVarSpans x
  = concatMap getSpan . Assignments.lookup x . hunkAssigns
  where
    getSpan = \case
      Assignments.ExplicitVal _ -> []
      Assignments.ImplicitVal s -> [s]

hunkBody :: Hunk a -> a
hunkBody
  = element . hunkLines

-- | Given a 'Span' @s@ that should be within that of the given 'Hunk',
--   return a new 'Hunk' whose span is @s@. Implicit assignments that
--   fall withing @s@ remain implicit, those that don't are turned into
--   explicit ones.
focus :: Span -> Hunk LinesOfText -> Except OutOfRange (Hunk LinesOfText)
focus s h
  = do
      when ( spanStart s < spanStart hs ) $
        throwError (OutOfRange $ spanStart s)

      when ( spanEnd s > spanEnd hs ) $
        throwError (OutOfRange $ spanEnd s)

      ls <- cut s h
      vars <- Assignments.traverseVal ensureInScope (hunkAssigns h)
      pure $ mkHunk (hunkFilename h) s vars ls
  where
    hs = hunkSpan h

    ensureInScope val = case val of
      ImplicitVal vs
        | vs `coveredBy` s -> pure val
        | otherwise -> ExplicitVal . LinesOfText.toText <$> cut vs h
      ExplicitVal{} -> pure val

cut :: MonadError OutOfRange m => Span -> Hunk LinesOfText -> m LinesOfText
cut spanToCut h
  = liftEither $
      LinesOfText.runFragmenter (spanStart $ hunkSpan h) (hunkBody h) $ do
        void $ LinesOfText.fragmentUpTo (LtLoc $ spanStart spanToCut)
        LinesOfText.fragmentUpTo (LEqLoc $ spanEnd spanToCut)
