{-# LANGUAGE DuplicateRecordFields #-}

module Alchemist.Internal.Types
  ( Experiment (..),
    Candidate (..),
    Result (..),
    Observation (..),
  )
where

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
data Experiment e m a = Experiment
  { -- | Every experiment has a _control_ value, which represents the original or standard behavior of the code in question. When an experiment
    control :: m a,
    -- | Each experiment has zero ore more _candidates_, representing new control paths that we may want to evaluate. If a given candidate is executed, its result will be compared (using 'comparator') with the result of executing the 'control'.
    candidates :: [Candidate m a],
    -- | This is used to compare the result of candidate execution with the control. This is usually the '==' function in a monadic context, but it can be overridden should you require more fine-grained behavior.
    comparator :: a -> a -> m Bool,
    enabled :: m Bool,
    name :: Text,
    publish :: Result e m a -> m (),
    raised :: Text -> e -> m a
  }

data Candidate m a = Candidate
  { action :: m a,
    name :: Text
  }

data Result e m a = Result
  { observations :: [Observation e m a],
    control :: a,
    mismatched :: [Observation e m a]
  }

data Observation e m a = Observation
  { duration :: NominalDiffTime,
    experiment :: Experiment e m a,
    value :: Either e a
  }
