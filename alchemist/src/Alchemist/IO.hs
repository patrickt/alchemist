{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Alchemist.IO
  ( new,
    run,
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
import Control.Monad.IO.Class
import Data.Function ((&))
import Data.Text (Text)
import Data.Time.Clock
import System.Random

type ExperimentIO = Experiment SomeException IO

-- | Creates an 'Experiment' suitable for running an action in 'IO'.
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
      comparator = (==),
      publish = const (pure ())
    }

execute :: ExperimentIO a -> Candidate IO a -> IO (Observation SomeException IO a)
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

-- | Run an 'Experiment' in the 'IO' monad.
run :: ExperimentIO a -> IO a
run e = do
  on <- enabled e
  normal <- control e
  if not on
    then pure normal
    else do
      shuffled <- permute (candidates e)
      datums <- traverse (execute e) shuffled
      let inquire c = either (const True) (not . comparator e normal) (value c)
      let wrong = filter inquire datums
      let res = Result datums normal wrong
      publish e res
      if null datums
        then pure normal
        else either (raised e (name e)) pure (value (head datums))
