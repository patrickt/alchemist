{-# LANGUAGE DuplicateRecordFields #-}

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

-- | A representation of an experiment to be run.
-- Though you can create these values manually, it may be more
-- convenient to use the combinators present in modules like 'Alchemist.IO'
-- and 'Alchemist.Catch', which are geared towards common monad setups.
--
-- To add candidate actions, use the 'try' combinator:
--
-- @
--   res <- new "sample" (putStrLn "Sample value")
--     & try (putStrLn "One alternative")
--     & try (putStrLn "Another alternative")
--     & run
-- @
--
-- To run an experiment, use one of the modules that discharges them to a given monad:
-- - Alchemist.Catch
-- - Alchemist.IO
--
-- Each of these modules contains a @new@ function that serves as a
-- smart constructor for a given monad's 'Experiment' type. By
-- default, the returned experiment has its fields set as follows:
--
-- * @enabled@: 'pure' 'True'
-- * @comparator@: '=='
-- * @control@: provided
-- * @name@: provided
-- * @publish@: 'pure' '()'
-- * @raised@: 'Control.Exception.throw'
--


data Experiment m a = Experiment
  { candidates :: [Candidate m a],
     -- ^ Each experiment has zero ore more _candidates_. When an experiment is run, each candidate's action is run and compared with the control.
    comparator :: a -> a -> Bool,
     -- ^ Determines whether the
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
    mismatched :: [Observation m a]
  }

data Observation m a = Observation
  { duration :: NominalDiffTime,
    experiment :: Experiment m a,
    value :: Either SomeException a
  }
