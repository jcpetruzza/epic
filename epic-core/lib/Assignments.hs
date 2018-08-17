module Assignments
  ( -- * Types
   Assignments
  , Var(..)
  , Val(..)

  -- * Construction
  , empty
  , add
  , forget
  , fromList

  -- * Mapping
  , mapVal
  , traverseVal

  -- * Queries
  , Assignments.lookup
  , toList
  )

where

import           SrcLoc (Span)

import           Data.Hashable
import qualified Data.HashMap.Strict        as HM
import qualified Data.List.NonEmpty         as ListNE
import           Data.Semigroup (Semigroup(..))
import           Data.String
import           Data.Text ( Text )
import           GHC.Generics ( Generic )

-- | A map from variables to 1 or more values
newtype Assignments
  = Assignments {asMap :: HM.HashMap Var (ListNE.NonEmpty Val)}
  deriving (Eq, Show)

empty :: Assignments
empty = Assignments HM.empty

-- | Assign another 'Val' to 'Var'.
add :: Var -> Val -> Assignments -> Assignments
add x v ass
  = Assignments $ HM.insertWith (<>) x (pure v) (asMap ass)

-- | Remove all assignments to the given 'Var'
forget :: Var -> Assignments -> Assignments
forget x ass
  = Assignments $ HM.delete x (asMap ass)

lookup :: Var -> Assignments -> [Val]
lookup x ass
  = maybe [] ListNE.toList $ HM.lookup x (asMap ass)

fromList :: [(Var, Val)] -> Assignments
fromList kvs
  = Assignments (HM.fromListWith (<>) [(k, pure v) | (k,v) <- kvs])

toList :: Assignments -> [(Var, Val)]
toList (Assignments hm)
  = [(x,v) | (x, vs) <- HM.toList hm, v <- ListNE.toList vs]

mapVal :: (Val -> Val) -> Assignments -> Assignments
mapVal f
  = Assignments . fmap (fmap f). asMap

traverseVal :: Applicative f => (Val -> f Val) -> Assignments -> f Assignments
traverseVal f
  = fmap Assignments . traverse (traverse f). asMap

instance Semigroup Assignments where
  l <> r
    = Assignments (HM.unionWith (<>) (asMap l) (asMap r))

instance Monoid Assignments where
  mempty  = empty
  mappend = (<>)

newtype Var
  = Var Text
  deriving
    ( Eq, Ord, Hashable
    , Show
    , Generic
    )

instance IsString Var where
  fromString = Var . fromString

-- | A value can be explicit text or a implicitly defined by a span on a hunk
data Val
  = ExplicitVal Text
  | ImplicitVal Span
  deriving (Eq, Show)

instance IsString Val where
  fromString = ExplicitVal . fromString
