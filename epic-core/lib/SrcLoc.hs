module SrcLoc
  ( Src(..)
  , RowCol(..)
  , Loc, RelLoc
  , rightOf
  , Span(..)
  , overlap, overlapsWith, coveredBy
  , Located(..)
  )

where

import Algebra.Lattice (JoinSemiLattice(..), Lattice, MeetSemiLattice(..))
import Control.Monad (guard)
import Data.Aeson (ToJSON, FromJSON)
import Data.Maybe (isJust)
import Data.Monoid hiding ((<>))
import Data.Semigroup
import GHC.Generics (Generic)


-- | An annotation about a source file.
data Src a = Src
    { srcFilename :: FilePath
    , src :: a
    }
  deriving
    ( Eq, Ord
    , Show
    , Functor, Foldable, Traversable
    , Generic
    , FromJSON, ToJSON
    )

-- Row/Column data
data RowCol a = RowCol
  { row :: a
  , col :: a
  }
  deriving
    ( Eq, Ord
    , Show
    , Functor, Foldable, Traversable
    , Generic
    , FromJSON, ToJSON
    )

instance Semigroup a => Semigroup (RowCol a) where
  l <> r = RowCol
    { row = row l <> row r
    , col = col l <> col r
    }

instance (Semigroup a, Monoid a) => Monoid (RowCol a) where
  mempty = RowCol mempty mempty
  mappend = (<>)


-- | A location as row/column index
type Loc = RowCol Int

rightOf :: Loc -> Loc
rightOf loc
  = loc{col = col loc + 1}

-- | A location, relative to some 'Loc'
type RelLoc = RowCol (Sum Int)

-- | A portion of a source file, spanning one or more lines and zero or more columns.
data Span = Span
    { spanStart :: Loc
    , spanEnd   :: Loc
    }
  deriving
    ( Eq, Ord
    , Show
    , Generic
    , FromJSON, ToJSON
    )

instance JoinSemiLattice Span where
  -- | Minimum span covering both.
  l \/ r = Span
    { spanStart = spanStart l `min` spanStart r
    , spanEnd   = spanEnd   l `max` spanEnd   r
    }

instance MeetSemiLattice Span where
  -- | Itersection if overlapping, or internal area
  --   when disjoint
  l /\ r
    = let
        maxStart = spanStart l `max` spanStart r
        minEnd   = spanEnd   l `min` spanEnd   r
      in
        Span
          { spanStart = maxStart `min` minEnd
          , spanEnd   = maxStart `max` minEnd
          }

overlap :: Span -> Span -> Maybe Span
overlap l r
  = do
      let candidate = l /\ r
      guard (l /\ candidate == candidate)
      pure candidate

overlapsWith :: Span -> Span -> Bool
l `overlapsWith` r
  = isJust (l `overlap` r)

coveredBy :: Span -> Span -> Bool
l `coveredBy` r
  = l /\ r == l

data Located a
  = Located
      { location :: Src Span
      , element  :: a
      }
  deriving
    ( Eq, Ord
    , Show
    , Functor, Foldable, Traversable
    , Generic
    , FromJSON, ToJSON
    )
