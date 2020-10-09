{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Alchemist.IO
  ( Experiment (..)
  , new
  , try
  , run
  -- * Re-exports
  , (&)
  ) where

import Control.Exception (throw)
import Control.Monad.IO.Class
import System.Random
import Data.Text (Text)
import Data.Function ((&))
import Alchemist.Experiment
import Alchemist.Candidate
import Alchemist.Observation
import Data.Time.Clock

new :: Text -> IO a -> Experiment IO a
new n c =
  Experiment
    { enabled = pure True,
      control = c,
      candidates = [],
      raised = const throw,
      name = n,
      comparator = (==),
      publish = const (pure ())
    }

try :: m a -> Experiment m a -> Experiment m a
try c e = e { candidates = Candidate c "experiment" : candidates e }

execute :: MonadIO m => Experiment m a -> Candidate m a -> m (Observation m a)
execute e c = do
  start <- liftIO getCurrentTime
  val <- action c
  end <- liftIO getCurrentTime

  pure Observation{}


run :: MonadIO m => Experiment m a -> m a
run e
  | null (candidates e) = control e
  | otherwise = do
      on <- enabled e
      should <- randomIO
      normal <- control e
      if (not on || not should)
        then pure normal
        else undefined
