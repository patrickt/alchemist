{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

-- |
--
-- Module      : Alchemist
-- Copyright   : (c) Patrick Thomson, 2021
-- License     : MIT
-- Maintainer  : Patrick Thomson
-- Stability   : experimental
-- Portability : POSIX
--
-- This module exports all the things you need to run 'Experiment's in the 'IO' monad.
module Alchemist.IO
  ( -- * Constructing and configuring experiments
    experiment,

    -- ** Reexports from 'Experiment'
    Alchemist.Experiment.withCandidate,
    Alchemist.Experiment.withReporting,
    Alchemist.Experiment.disable,

    -- * Running experiments
    run,

    -- * Re-exports
    (&),
  )
where

import Alchemist.Candidate
import Alchemist.Experiment
import Alchemist.Internal.Shuffle
import Alchemist.Observation
import Alchemist.Result
import Control.Exception qualified as Exc
import Control.Monad (filterM)
import Control.Monad.IO.Class
import Data.Function ((&))
import Data.Monoid
import Data.Text (Text)
import Data.Time.Clock

-- | Creates an 'Experiment' suitable for running an action in 'IO'.
-- By default, this experiment is 'Alchemist.Experiment.enabled' and
-- has no 'Alchemist.Experiment.candidates'. The report function
-- performs no action.
--
-- You will generally pass the result of 'experiment' to one or more
-- occurrences of the 'withCandidate' combinator.
experiment ::
  Exc.Exception e =>
  -- | the name of this experiment
  Text ->
  -- | the control (default) action to run
  IO a ->
  Experiment IO e a
experiment n c =
  Experiment
    { enabled = pure True,
      control = c,
      candidates = [],
      attempt = Exc.try,
      name = n,
      report = getAp <$> mempty
    }

execute :: Experiment IO e a -> Candidate IO a -> IO (Observation IO e a)
execute e c = do
  start <- liftIO getCurrentTime
  val <- attempt e (action c)
  end <- liftIO getCurrentTime

  pure
    Observation
      { duration = diffUTCTime end start,
        parent = e,
        value = val,
        candidate = c
      }

run :: Eq a => Experiment IO e a -> IO a
run = fmap fst . runWithResult

-- | Run an 'Experiment' in the 'IO' monad.
runWithResult :: Eq a => Experiment IO e a -> IO (a, Result IO e a)
runWithResult e = do
  on <- enabled e
  normal <- control e
  if not on
    then pure (normal, Result [] normal [])
    else do
      shuffled <- permute (candidates e)
      datums <- traverse (execute e) shuffled
      let inquire c = case value c of
            Left _ -> pure True
            Right ok -> pure (ok /= normal)
      wrong <- filterM inquire datums
      let res = Result datums normal wrong
      pure (normal, res)
