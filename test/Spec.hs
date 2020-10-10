{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Alchemist.IO qualified as Alc
import Control.Monad
import Control.Monad.IO.Class
import Data.Function
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Data.IORef


countExecutions :: (Eq a, MonadIO m) => Alc.Experiment IO a -> m (a, Int)
countExecutions runner = do
  let io = liftIO
  ref <- io (newIORef 0)
  let counting = runner & Alc.reporting (const (modifyIORef ref succ))

  res <- io . Alc.run $ counting
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

-- prop_executesNActionsForNMinusOneCandidates :: Property
-- prop_executesNActionsForNMinusOneCandidates = property do
--   len <- forAll (Gen.int (Range.linear 3 10))
--   let values = replicate len (pure ())
--   let runner = Alc.new "N actions" (pure ())

--   let withAllTries = foldr Alc.try runner values -- chef_kissing_fingers.png
--   (_, amt) <- countExecutions withAllTries
--   amt === len

prop_callsHandlerIO :: Property
prop_callsHandlerIO = property $ do
  let runner = Alc.new "example" (pure False)
        & Alc.try (error "OH NO!")
        & Alc.handling (\_ _ -> pure True)

  res <- liftIO (Alc.run runner)
  res === True

tests :: IO Bool
tests =
  checkParallel $$(discover)

main :: IO ()
main = void tests
