{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Alchemist.IO
  ( experiment,
    run,

    -- * Re-exports
    (&),
    module Alchemist.Experiment,
  )
where

import Alchemist.Experiment
import Alchemist.Internal.Shuffle
import Alchemist.Internal.Types
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

run :: (Exc.Exception e, Eq a) => Experiment IO e a -> IO a
run = fmap fst . runWithResult

-- | Run an 'Experiment' in the 'IO' monad.
runWithResult :: (Exc.Exception e, Eq a) => Experiment IO e a -> IO (a, Result IO e a)
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
