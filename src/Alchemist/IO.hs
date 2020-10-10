{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Alchemist.IO
  ( Experiment (..),
    new,
    try,
    try',
    handling,
    reporting,
    runIf,
    run,

    -- * Re-exports
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
new ::
  -- | the name of this experiement
  Text ->
  -- | the control (default) action to run
  IO a ->
  Experiment IO a
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

execute :: Experiment IO a -> Candidate IO a -> IO (Observation IO a)
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
run :: Eq a => Experiment IO a -> IO a
run e = do
  on <- enabled e
  normal <- control e
  if not on
    then pure normal
    else do
      shuffled <- permute (candidates e)
      datums <- traverse (execute e) shuffled
      let wrong = filter (\c -> either (const True) (/= normal) (value c) ) datums
      let res = Result datums normal wrong
      publish e res
      if null datums
        then pure normal
        else either (raised e (name e)) pure (value (head datums))
