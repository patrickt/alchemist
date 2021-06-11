{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_HADDOCK not-home hide #-}

module Alchemist.Internal.Types
  ( module Alchemist.Internal.Types,
  )
where

import Data.Kind (Type)
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime)
import Alchemist.Candidate

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
    -- evaluate. If a given candidate is executed, its result will be compared (using '==') with the
    -- result of executing the 'control', and information about that candidate will be recorded in a given
    -- 'Observation'.
    candidates :: [Candidate m a],
    -- | This determines whether the candidates in an experiment should be run at all. A disabled experiment
    -- will run only its control. This defaults to @pure True@.
    enabled :: m Bool,
    -- | Every experiment is associated with a textual identifier. These names should be nonempty and unique to a given experiment.
    name :: Text,
    -- | Experiments proactively handle errors that occur during their execution. This function describes
    -- how the given monad should try to run a given candidate. One example valid signature is "Control.Exception.try".
    attempt :: m a -> m (Either e a),
    -- | When a candidate finishes executing, it will call this function, passing information about its execution
    -- in the 'Observation' parameter. This can be useful for logging information about individual candidates.
    report :: Observation m e a -> m ()
  }

-- | A 'Result' contains all relevant information about an executed 'Experiment'.
-- All recorded 'Observation' values are stored, and those whose associated candidates
-- failed or produced a result not equal to the control are also present in 'mismatched'.
data Result (m :: Type -> Type) e a = Result
  { observations :: [Observation m e a],
    controlValue :: a,
    mismatched :: [Observation m e a]
  }

instance Semigroup a => Semigroup (Result m e a) where
  Result o c m <> Result o' c' m' = Result (o <> o') (c <> c') (m <> m')

instance Monoid a => Monoid (Result m e a) where
  mempty = Result [] mempty []

data Observation (m :: Type -> Type) e a = Observation
  { duration :: NominalDiffTime,
    parent :: Experiment m e a,
    candidate :: Candidate m a,
    value :: Either e a
  }
