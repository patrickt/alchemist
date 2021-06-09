{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE KindSignatures #-}
module Alchemist.Internal.Types
  ( Experiment (Experiment, control, candidates, comparator, enabled, name, raised),
    Candidate (..),
    Result (..),
    Observation (..),
  )
where

import Data.Text (Text)
import Data.Time.Clock
import Data.Kind (Type)

-- | A representation of an experiment to be run. Though you can create these values manually, it may be more
-- convenient to use the combinators present in modules like "Alchemist.IO" and "Alchemist.Catch", which are
-- geared towards common monads.
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
--
-- - "Alchemist.Catch"
-- - "Alchemist.IO"
--
-- These modules reexport all the definitions that you'll need to use 'Experiment' values, though you may want
-- to import them @qualified@ to avoid name collisions.
data Experiment (m :: Type -> Type) e a = Experiment
  { -- | Every 'Experiment' has a /control/ value, which represents the original or standard behavior of the
    -- code in question. When an 'enabled' experiment is run, its 'control' is always executed, along with
    -- zero or more (depending on randomness) of the 'candidates'.
    control :: m a,
    -- | Each experiment has zero ore more /candidates/, representing new control paths that we may want to
    -- evaluate. If a given candidate is executed, its result will be compared (using 'comparator') with the
    -- result of executing the 'control', and information about that candidate will be recorded in a given
    -- 'Observation'.
    candidates :: [Candidate m a],
    -- | This is used to compare the result of candidate execution with the control. This is usually the '=='
    -- function in a monadic context, but it can be overridden should you require more fine-grained behavior.
    comparator :: a -> a -> m Bool,
    -- | This determines whether the candidates in an experiment should be run at all. A disabled experiment
    -- will run only its control. This defaults to @pure True@.
    enabled :: m Bool,
    -- | Every experiment is associated with a textual identifier. These names should be nonempty and unique to a given experiment.
    name :: Text,
    raised :: Text -> e -> m a
  }

data Candidate (m :: Type -> Type) a = Candidate
  { action :: m a,
    name' :: Text
  }

data Result (m :: Type -> Type) e a = Result
  { observations :: [Observation m e a],
    control' :: a,
    mismatched :: [Observation m e a]
  }

data Observation (m :: Type -> Type) e a = Observation
  { duration :: NominalDiffTime,
    experiment :: Experiment m e a,
    value :: Either e a
  }
