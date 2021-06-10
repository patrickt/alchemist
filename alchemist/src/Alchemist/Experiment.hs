{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Alchemist.Experiment
  ( Experiment (..),

    -- * Fluent constructors
    candidate,
    reportingWith,
    runIf,
  )
where

import Data.Text (Text)
import Alchemist.Internal.Types

-- | Add a new candidate action to the provided 'Experiment', using a
-- default name (@<experiment>@). When the resulting 'Experiment' is
-- invoked, the runner will execute the provided m action and report
-- the results via the
candidate :: Text -> m a -> Experiment m e a -> Experiment m e a
candidate m c e = e {candidates = Candidate c m : candidates e}

-- | Defines the reporting function that a given experiment should use
-- to record or log information about the result of executing its
-- candidates.
reportingWith :: (Result m e a -> m ()) -> Experiment m e a -> Experiment m e a
reportingWith f e = e { report = f }

-- | Conditionally enable or disable (via the @enabled@ field)an
-- experiment. An experiment that is disabled will always return its
-- control value and will not call its publish function.
runIf :: m Bool -> Experiment m e a -> Experiment m e a
runIf x e = e {enabled = x}
