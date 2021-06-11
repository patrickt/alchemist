{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Alchemist.IO
import Control.Monad
import Data.Foldable
import Control.Monad.IO.Class
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Control.Exception (throwIO, SomeException)
import Hedgehog.Range qualified as Range
import Data.IORef


countExecutions :: (MonadIO m, Eq a) => Experiment IO SomeException a -> m (a, Int)
countExecutions runner = do
  let io = liftIO
  ref <- io (newIORef 0)
  let runner' = withReporting (const (modifyIORef ref succ)) runner

  res <- io (run runner')
  val <- io . readIORef $ ref
  pure (res, val)

prop_executesNoActionWithNoCandidates :: Property
prop_executesNoActionWithNoCandidates = property do
  x <- forAll (Gen.int (Range.linear 0 100))

  let runner = experiment "no actions" (pure x)

  (val, amt) <- countExecutions runner
  x === val
  amt === 1

prop_executesNoActionsWhenDisabled :: Property
prop_executesNoActionsWhenDisabled = property do
  x <- forAll (Gen.int (Range.linear 0 100))

  let runner
        = experiment "no actions" (pure x)
        & disable

  (val, amt) <- countExecutions runner
  x === val
  amt === 0

prop_runsAllTryBlocks :: Property
prop_runsAllTryBlocks = property do
  let io = liftIO
  ref <- io (newIORef 0)
  len <- forAll (Gen.int (Range.linear 1 10))

  let values = replicate len (modifyIORef ref succ)
  let runner = experiment "N actions" (liftIO (pure ()))

  let assembled = foldr (withCandidate "") runner values

  void . io . run @() @SomeException $ assembled
  amt <- io . readIORef $ ref
  amt === len

prop_executesNActionsForNMinusOneInvocations :: Property
prop_executesNActionsForNMinusOneInvocations = property do
  len <- forAll (Gen.int (Range.linear 1 10))
  let values = replicate len ()
  let runner = experiment "N actions" (liftIO (pure ()))

  let io = liftIO
  ref <- io (newIORef 0)
  let incr = const (modifyIORef ref succ)

  for_ values (\_ -> io (run . withReporting @SomeException @_ @() incr $ runner))
  amt <- io . readIORef $ ref
  amt === len
  annotateShow (length values)

prop_callsHandlerIO :: Property
prop_callsHandlerIO = property $ do
  let runner = experiment "example" (pure False)
        & withCandidate "always fails" (throwIO (userError "Oh no!"))

  res <- liftIO (run @Bool @SomeException runner)
  res === True

tests :: IO Bool
tests =
  checkParallel $$(discover)

main :: IO ()
main = void tests
