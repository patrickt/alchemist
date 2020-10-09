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

prop_executesNoActionWithNoCandidates :: Property
prop_executesNoActionWithNoCandidates = property do
  ref <- liftIO (newIORef 0)
  x <- forAll (Gen.int (Range.linear 0 100))

  let runner
        = Alc.new "no actions" (pure x)
        & Alc.reporting (const (modifyIORef ref succ))


  e <- liftIO . Alc.run $ runner
  x === e
  amt <- liftIO $ readIORef ref
  amt === (1 :: Int)

prop_executesNoActionsWhenDisabled :: Property
prop_executesNoActionsWhenDisabled = property do
  ref <- liftIO (newIORef 0)
  x <- forAll (Gen.int (Range.linear 0 100))

  let runner
        = Alc.new "no actions" (pure x)
        & Alc.reporting (const (modifyIORef ref succ))
        & Alc.runIf (pure False)

  e <- liftIO . Alc.run $ runner
  x === e
  amt <- liftIO (readIORef ref)
  amt === (0 :: Int)

tests :: IO Bool
tests =
  checkParallel $$(discover)

main :: IO ()
main = void tests

-- main :: IO ()
-- main = do
--   Alc.new "test" (putStrLn "Standard")
--     & Alc.try (putStrLn "Alternative")
--     & Alc.run
