{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Alchemist.IO
  ( withControl,
    run,
    runReporting,

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
withControl ::
  forall e a.
  (Exc.Exception e, Eq a) =>
  -- | the name of this experiment
  Text ->
  -- | the control (default) action to run
  IO a ->
  Experiment IO e a
withControl n c =
  Experiment
    { enabled = pure True,
      control = c,
      candidates = [],
      attempt = Exc.try,
      name = n,
      report = getAp <$> mempty,
      comparator = \x y -> pure (x == y)
    }

execute :: Experiment IO e a -> Candidate IO a -> IO (Observation IO e a)
execute e c = do
  start <- liftIO getCurrentTime
  val <- attempt e (action c)
  end <- liftIO getCurrentTime

  pure
    Observation
      { duration = diffUTCTime end start,
        experiment = e,
        value = val,
        candidate = c
      }

runReporting :: (Exc.Exception e) => (Result IO e a -> IO ()) -> Experiment IO e a -> IO a
runReporting p e = do
  on <- enabled e
  normal <- control e
  if not on
    then pure normal
    else do
      shuffled <- permute (candidates e)
      datums <- traverse (execute e) shuffled
      let inquire c = case value c of
            Left _ -> pure True
            Right ok -> not <$> comparator e ok normal
      wrong <- filterM inquire datums
      let res = Result datums normal wrong
      normal <$ p res

run :: (Exc.Exception e) => Experiment IO e a -> IO a
run = fmap fst . runWithResult

-- | Run an 'Experiment' in the 'IO' monad.
runWithResult :: (Exc.Exception e) => Experiment IO e a -> IO (a, Result IO e a)
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
            Right ok -> not <$> comparator e ok normal
      wrong <- filterM inquire datums
      let res = Result datums normal wrong
      pure (normal, res)
