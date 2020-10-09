{- |
Copyright: (c) 2020 Patrick Thomson
SPDX-License-Identifier: MIT
Maintainer: Patrick Thomson <patrick.william.thomson@gmail.com>

A library for refactoring critical paths.
-}

module Alchemist
       ( someFunc
       ) where


someFunc :: IO ()
someFunc = putStrLn ("someFunc" :: String)
