{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}

module Alchemist.Internal.Types
  ( Experiment (..),
    Candidate (..),
    Result (..),
    Observation (..),
  )
where

import Control.Exception (SomeException)
import Data.Text (Text)
import Data.Time.Clock

data Experiment m a = Experiment
  { candidates :: [Candidate m a],
    comparator :: Eq a => a -> a -> Bool,
    control :: m a,
    enabled :: m Bool,
    name :: Text,
    publish :: Result m a -> m (),
    raised :: Text -> SomeException -> m a
  }

data Candidate m a = Candidate
  { action :: m a,
    name :: Text
  }

data Result m a = Result
  { observations :: [Observation m a],
    control :: a,
    ignored :: [Observation m a],
    mismatched :: [Observation m a]
  }

data Observation m a = Observation
  { duration :: NominalDiffTime,
    experiment :: Experiment m a,
    value :: Either SomeException a
  }
