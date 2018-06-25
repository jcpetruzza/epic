module SrcLoc
  ( Src(..)
  , RowCol(..)
  , Loc, RelLoc
  , Span(..)
  , Located(..)
  )

where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Monoid hiding ((<>))
import Data.Semigroup


-- | An annotation about a source file.
data Src a = Src
    { srcFilename :: String
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
