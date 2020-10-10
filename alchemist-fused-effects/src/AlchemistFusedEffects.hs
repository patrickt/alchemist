{- |
Copyright: (c) 2020 Patrick Thomson
SPDX-License-Identifier: MIT
Maintainer: Patrick Thomson <patrick.william.thomson@gmail.com>

A fused-effects interface to the alchemist library.
-}

module AlchemistFusedEffects
       ( someFunc
       ) where


someFunc :: IO ()
someFunc = putStrLn ("someFunc" :: String)
