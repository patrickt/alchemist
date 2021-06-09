{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Alchemist.Experiment
  ( Experiment (..),

    -- * Fluent constructors
    try,
    try',
    handling,
    reporting,
    runIf,
  )
where

import Alchemist.Internal.Types (Experiment (..), Result, Candidate (..))
import Control.Exception (SomeException)
import Data.Text (Text)

-- | Add a new candidate action to the provided 'Experiment', using a
-- default name (@<experiment>@). When the resulting 'Experiment' is
-- invoked, the runner will execute the provided m action and report
-- the results via the experiment's @publish@ function.
try :: m a -> Experiment m e a -> Experiment m e a
try = try' "<candidate>"

-- | As 'try', but taking an argument to name the candidate action.
try' :: Text -> m a -> Experiment m e a -> Experiment m e a
try' m c e = e {candidates = Candidate c m : candidates e}

-- | Set the exception handler (the @raised@ field) for a given
-- experiment. This will be invoked when any exception occurs in IO.
handling :: (Text -> e -> m a) -> Experiment m e a -> Experiment m e a
handling f e = e {raised = f}

-- | Set the publish handler (the @publish@ field) for a given
-- experiment. When a given candidate completes, its yielded value and
-- timing statistics are passed in a 'Result' to the provided
-- function.
reporting :: (Result m e a -> m ()) -> Experiment m e a -> Experiment m e a
reporting f e = e {publish = f}

-- | Conditionally enable or disable (via the @enabled@ field)an
-- experiment. An experiment that is disabled will always return its
-- control value and will not call its publish function.
runIf :: m Bool -> Experiment m e a -> Experiment m e a
runIf x e = e {enabled = x}
