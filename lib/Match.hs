{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types        #-}
module Match
  ( Match
  , matchSpan
  , matchVars

  , matchNoVar
  , matchVar
  , matchUnion
  , matchUnions
  , MatchUnionError (..)

  , MatchFragment (..)
  , matchFragments

  , UpToLoc (..)
  , upToRow

  , Matched (..)
  , mapMatched
  , Matchable (..)
  , OutOfRange (..)

  , MatchVar (..)
  , MatchVarsTo
  )

where

import           SrcLoc              ( Loc, RowCol(..), Span(..), overlapsWith, rightOf )

import           Algebra.Lattice     ( (\/) )
import           Control.Monad       ( foldM, unless, when )
import           Data.Function       ( on )
import           Data.Hashable       ( Hashable )
import qualified Data.HashMap.Strict as HM
import qualified Data.List           as List
import qualified Data.List.NonEmpty  as ListNE
import           Data.String
import           Data.Text           ( Text )
import           GHC.Generics        ( Generic )


data Match
  = Match
      { matchSpan :: Span
      , matchVars :: MatchVarsTo Span
      }
 deriving (Eq, Show, Generic)

data MatchUnionError
  = MatchingVarsOverlap MatchVar Span MatchVar Span
  | MatchingVarTwoValues MatchVar Span Span
  deriving (Eq, Show, Generic)


matchNoVar :: Span -> Match
matchNoVar s
  = Match
      { matchSpan = s
      , matchVars = HM.empty
      }


matchVar :: MatchVar -> Span -> Match
matchVar var s
  = Match
      { matchSpan = s
      , matchVars = HM.singleton var s
      }

matchUnion :: Match -> Match -> Either MatchUnionError Match
matchUnion l r
  = do
      checkMultipleDefinitions
      checkOverlapping
      pure Match
        { matchSpan = matchSpan l \/ matchSpan r
        , matchVars = matchVars l `HM.union` matchVars r
        }
  where
    checkMultipleDefinitions
      = do
          let checkSameDefinition k x y
                = unless (x == y) $ Left $ MatchingVarTwoValues k x y
          sequence_ $
            HM.intersectionWithKey checkSameDefinition (matchVars l) (matchVars r)


    checkOverlapping
      = do let checkOverlaps (vl, sl) (vr, sr)
                 = when (vl /= vr && sl `overlapsWith` sr) $
                     Left $ MatchingVarsOverlap vl sl vr sr

               sortedBySpan
                 = List.sortBy (compare `on` snd) . HM.toList

           sequence_ $
             mergeWith checkOverlaps (const [pure ()])
               (sortedBySpan $ matchVars l)
               (sortedBySpan $ matchVars r)


    mergeWith :: Ord a => (a -> a -> r) -> ([a] -> [r]) -> [a] -> [a] -> [r]
    mergeWith f g = go
      where
        go [] ys = g ys
        go xs [] = g xs
        go allxs@(x:xs) allys@(y:ys)
          | x <= y    = f x y : go xs allys
          | otherwise = f x y : go allxs ys

matchUnions :: ListNE.NonEmpty Match -> Either MatchUnionError Match
matchUnions matches
  = foldM matchUnion (ListNE.head matches) (ListNE.tail matches)

data MatchFragment a b
  = UnmatchedFragment a
  | MatchedFragment MatchVar b
  deriving (Eq, Show, Generic)

data UpToLoc
  = LtLoc  Loc
  | LEqLoc Loc
  deriving (Eq, Show, Generic)

upToRow :: UpToLoc -> Int
upToRow = \case
  LtLoc  loc ->
    if col loc == 0 then row loc - 1 else row loc
  LEqLoc loc -> row loc

matchFragments :: Match -> [MatchFragment UpToLoc Span]
matchFragments m
  = case orderedMatches of
      [] -> go []
      (_,s):_
        | beg == spanStart s -> go orderedMatches
        | otherwise -> UnmatchedFragment (LtLoc $ spanStart s) : go orderedMatches
  where
    orderedMatches = List.sortBy (compare `on` snd) $ HM.toList (matchVars m)

    beg = spanStart $ matchSpan m
    end = spanEnd   $ matchSpan m

    go = \case
      [] ->
        [UnmatchedFragment (LEqLoc end)]

      [(v,s)] | spanEnd s == end ->
        [MatchedFragment v s]

      [(v,s)] ->
        [MatchedFragment v s, UnmatchedFragment (LEqLoc end)]

      (v1,s1):rest@((_,s2):_) | rightOf (spanEnd s1) < spanStart s2 ->
        MatchedFragment v1 s1 : UnmatchedFragment (LtLoc $ spanStart s2) : go rest

      (v1,s1):rest ->
        MatchedFragment v1 s1 : go rest


data Matched a
  = Matched
      { matchedMatch   :: Match
      , matchedContext :: a
      , matchedVars    :: MatchVarsTo a
      , matchedApply   :: MatchVarsTo a -> a
      }

mapMatched :: (a -> b) -> (b -> a) -> Matched a -> Matched b
mapMatched fab fba ma
  = Matched
      { matchedMatch   = matchedMatch ma
      , matchedContext = fab (matchedContext ma)
      , matchedVars    = fab <$> matchedVars ma
      , matchedApply   = \mvtb -> fab $ matchedApply ma (fba <$> mvtb)
      }


class Matchable a where
  matched :: Match -> a -> Either OutOfRange (Matched a)

data OutOfRange
  = OutOfRange Loc
  deriving (Eq, Show, Generic)

type MatchVarsTo a
  = HM.HashMap MatchVar a

newtype MatchVar
  = MatchVar Text
  deriving
    ( Eq, Ord, Hashable
    , Show
    , Generic
    )

instance IsString MatchVar where
  fromString = MatchVar . fromString
