{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Function
import Alchemist.IO qualified as Alc

main :: IO ()
main = do
  Alc.new "test" (putStrLn "Standard")
    & Alc.try (putStrLn "Alternative")
    & Alc.run
