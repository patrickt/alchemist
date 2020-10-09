{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Alchemist.Internal.Types
  ( Experiment (..)
  , Candidate (..)
  , Result (..)
  , Observation (..)
  ) where

import Data.Text (Text)
import Control.Exception (SomeException)

data Experiment m a = Experiment
  { enabled :: m Bool,
    control :: m a,
    candidates :: [Candidate m a],
    raised :: Text -> SomeException -> m a,
    name :: Text,
    comparator :: Eq a => a -> a -> Bool
    publish :: Result m a -> m ()
  }

data Candidate m a = Candidate
  { action :: m a
  , name :: m a
  }

data Result m a = Result
  { observations :: [Observation m a]
  , control :: m a
  , experiment :: Experiment m a
  , ignored :: [Observation m a]
  , mismatched :: [Observation m a]
  }

data Observation m a = Observation
  { duration :: Double,
    exception :: Maybe SomeException,
    experiment :: Experiment m a,
    name :: Text,
    value :: a
  }
