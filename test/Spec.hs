{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Function
import Control.Alchemist qualified as Alc

main :: IO ()
main = do

  _ <-
    Alc.new "test" (putStrLn "Standard")
    & Alc.try (putStrLn "Alternative")
    & Alc.run

  putStrLn ("Test suite is not implemented" :: String)
