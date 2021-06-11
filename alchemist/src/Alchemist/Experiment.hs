{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Alchemist.Experiment
  ( Experiment (..),

    -- * Fluent constructors
    withCandidate,
    withReporting,
    disable,
  )
where

import Data.Text (Text)
import Alchemist.Candidate
import Alchemist.Internal.Types

-- | Add a new candidate action, with an associated name to the
-- provided 'Experiment'. When the resulting 'Experiment' is invoked,
-- the runner will execute the provided m action and report the
-- results via its 'report' function.
withCandidate :: Text -> m a -> Experiment m e a -> Experiment m e a
withCandidate m c e = e {candidates = Candidate c m : candidates e}

-- | Defines the reporting function that a given experiment should use
-- to record or log information about the result of executing its
-- candidates.
withReporting :: forall e m a . (Observation m e a -> m ()) -> Experiment m e a -> Experiment m e a
withReporting f e = e { report = f }

-- | Disable an experiment. A disabled experiment will still run its
-- control but will not execute any of its candidates.
--
-- @ disable = set #enabled (pure False) @
--
disable :: Applicative m => Experiment m e a -> Experiment m e a
disable e = e {enabled = pure False }
