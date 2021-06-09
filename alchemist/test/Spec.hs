{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Alchemist.IO qualified as Alc
import Control.Monad
import Data.Foldable
import Control.Monad.IO.Class
import Data.Function
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Control.Exception (throwIO)
import Hedgehog.Range qualified as Range
import Data.IORef


countExecutions :: MonadIO m => Alc.ExperimentIO a -> m (a, Int)
countExecutions runner = do
  let io = liftIO
  ref <- io (newIORef 0)
  let incr _ = modifyIORef ref succ

  res <- io . Alc.runReporting incr $ runner
  val <- io . readIORef $ ref
  pure (res, val)

prop_executesNoActionWithNoCandidates :: Property
prop_executesNoActionWithNoCandidates = property do
  x <- forAll (Gen.int (Range.linear 0 100))

  let runner = Alc.new "no actions" (pure x)

  (val, amt) <- countExecutions runner
  x === val
  amt === 1

prop_executesNoActionsWhenDisabled :: Property
prop_executesNoActionsWhenDisabled = property do
  x <- forAll (Gen.int (Range.linear 0 100))

  let runner
        = Alc.new "no actions" (pure x)
        & Alc.runIf (pure False)

  (val, amt) <- countExecutions runner
  x === val
  amt === 0

prop_runsAllTryBlocks :: Property
prop_runsAllTryBlocks = property do
  let io = liftIO
  ref <- io (newIORef 0)
  len <- forAll (Gen.int (Range.linear 1 10))

  let values = replicate len (modifyIORef ref succ)
  let runner = Alc.new "N actions" (liftIO (pure ()))

  let assembled = foldr Alc.try runner values

  io . Alc.run $ assembled
  amt <- io . readIORef $ ref
  amt === len

prop_executesNActionsForNMinusOneInvocations :: Property
prop_executesNActionsForNMinusOneInvocations = property do
  len <- forAll (Gen.int (Range.linear 1 10))
  let values = replicate len ()
  let runner = Alc.new "N actions" (liftIO (pure ()))

  let io = liftIO
  ref <- io (newIORef 0)
  let counting = runner & Alc.reporting (const (modifyIORef ref succ))

  for_ values (\_ -> io . Alc.run $ counting)
  amt <- io . readIORef $ ref
  amt === len
  annotateShow (length values)

prop_callsHandlerIO :: Property
prop_callsHandlerIO = property $ do
  let runner = Alc.new "example" (pure False)
        & Alc.try (throwIO (userError "Oh no!"))
        & Alc.handling (\_ _ -> pure True)

  res <- liftIO (Alc.run runner)
  res === True

tests :: IO Bool
tests =
  checkParallel $$(discover)

main :: IO ()
main = void tests
