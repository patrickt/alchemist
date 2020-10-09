{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Alchemist.IO
  ( Experiment (..)
  , new
  , try
  , run
  -- * Re-exports
  , (&)
  ) where

import Control.Exception qualified as Exc
import Control.Monad.IO.Class
import System.Random
import Data.Text (Text)
import Data.Function ((&))
import Alchemist.Experiment
import Alchemist.Result hiding (control)
import Alchemist.Candidate hiding (name)
import Alchemist.Observation
import Data.Time.Clock

-- | Creates an 'Experiment' suitable for running an action in 'IO'.
-- The resulting 'Experiment' has its fields set as follows:
--
-- * @enabled@: 'pure' 'True'
-- * @comparator@: '=='
-- * @control@: provided
-- * @name@: provided
-- * @publish@: 'pure' '()'
-- * @raised@: 'Control.Exception.throw'
--
-- To add candidate actions, use the 'try' combinator:
--
-- @
--   res <- new "sample" (putStrLn "Sample value")
--     & try (putStrLn "One alternative")
--     & try (putStrLn "Another alternative")
--     & run
-- @
new
  :: Text -- ^ the name of this experiement
  -> IO a -- ^ the control (default) action to run
  -> Experiment IO a
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

-- | Add a new candidate action to the provided 'Experiment'.
-- When the resulting 'Experiment' is invoked, the runner
-- will execute the provided IO action and report the results
-- via the experiment's @publish@ function.
try :: IO a -> Experiment IO a -> Experiment IO a
try c e = e { candidates = Candidate c "experiment" : candidates e }

execute :: Experiment IO a -> Candidate IO a -> IO (Observation IO a)
execute e c = do
  start <- liftIO getCurrentTime
  val <- Exc.try (action c)
  end <- liftIO getCurrentTime

  pure Observation
    { duration = diffUTCTime end start
    , experiment = e
    , value = val
    }

-- | Run an 'Experiment' in the 'IO' monad.
run ::  Experiment IO a -> IO a
run e
  | null (candidates e) = control e
  | otherwise = do
      on <- enabled e
      should <- randomIO
      normal <- control e
      if (not on || not should)
        then pure normal
        else do
          datums <- traverse (execute e) (candidates e)
          let res = Result datums normal [] []
          publish e res
          case value (head datums) of
            Left x -> raised e (name e) x
            Right v -> pure v
