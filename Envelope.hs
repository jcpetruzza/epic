{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveTraversable  #-}
module Envelope
  ( Enveloped(..)
  )

where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

import SrcLoc

data Enveloped t a
  = Enveloped
      { payloadType :: t
      , payload :: a
      }
  deriving (Eq, Functor, Foldable, Traversable, Generic, FromJSON, ToJSON)
