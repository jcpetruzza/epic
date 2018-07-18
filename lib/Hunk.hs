module Hunk
  ( Hunk(..)
  , mkHunk
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
