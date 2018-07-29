module Hunk
  ( Hunk(..)
  , mkHunk
  , hunkFilename
  , hunkSpan
  , hunkBody
  )

where

import Assignments
import  SrcLoc (Located(..), Span(..), Src(..))

-- | A fragment of a file, perhaps with some assignmentsa.
data Hunk a
  = Hunk
      { hunkVars  :: Assignments
      , hunkLines :: Located a
      }
  deriving (Eq, Show, Functor)

mkHunk :: FilePath -> Span -> Assignments -> a -> Hunk a
mkHunk fp loc ms a
  = Hunk
      { hunkVars = ms
      , hunkLines = Located (Src fp loc) a
      }

hunkFilename :: Hunk a -> FilePath
hunkFilename
  = srcFilename . location . hunkLines

hunkSpan :: Hunk a -> Span
hunkSpan
  = src . location . hunkLines

hunkBody :: Hunk a -> a
hunkBody
  = element . hunkLines
