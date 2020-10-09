{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DisambiguateRecordFields #-}

module Alchemist
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

new :: Monad m => Text -> m a -> Experiment m a
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
try c e = e { candidates = c : candidates e }

run :: MonadIO m => Experiment m a -> m a
run e
  | null (candidates e) = control e
  | otherwise = do
      on <- enabled e
      should <- randomIO
      if (not on || not should)
        then control e
        else do
          idx <- randomRIO (0, length (candidates e) - 1)
          action ((candidates e) !! idx)
