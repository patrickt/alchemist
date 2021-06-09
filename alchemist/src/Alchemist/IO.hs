{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Alchemist.IO
  ( new,
    run,
    runReporting,
    ExperimentIO,

    -- * Re-exports
    module Alchemist.Experiment,
    (&),
  )
where

import Alchemist.Candidate hiding (name)
import Alchemist.Experiment
import Alchemist.Internal.Shuffle
import Alchemist.Observation
import Alchemist.Result hiding (control)
import Control.Exception (SomeException)
import Control.Exception qualified as Exc
import Control.Monad (filterM)
import Control.Monad.IO.Class
import Data.Function ((&))
import Data.Text (Text)
import Data.Time.Clock

type ExperimentIO = Experiment IO SomeException

-- | Creates an 'Experiment' suitable for running an action in 'IO'. By default,
-- this experiment is 'enabled', has no 'candidates', and rethrows exceptions
-- encountered in its 'control'.
new ::
  (Eq a) =>
  -- | the name of this experiment
  Text ->
  -- | the control (default) action to run
  IO a ->
  ExperimentIO a
new n c =
  Experiment
    { enabled = pure True,
      control = c,
      candidates = [],
      raised = const Exc.throw,
      name = n,
      comparator = \x y -> pure (x == y),
      publish = const (pure ())
    }

execute :: (Exc.Exception e) => Experiment IO e a -> Candidate IO a -> IO (Observation IO e a)
execute e c = do
  start <- liftIO getCurrentTime
  val <- Exc.try (action c)
  end <- liftIO getCurrentTime

  pure
    Observation
      { duration = diffUTCTime end start,
        experiment = e,
        value = val
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

-- | Run an 'Experiment' in the 'IO' monad.
run :: (Exc.Exception e) => Experiment IO e a -> IO a
run e = do
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
      publish e res
      if null datums
        then pure normal
        else either (raised e (name e)) pure (value (head datums))
