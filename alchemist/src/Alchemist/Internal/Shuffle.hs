module Alchemist.Internal.Shuffle (permute) where

import Control.Monad
import System.Random

    -- List returning elements in random order
type RandomList a = IO [a]

empty :: RandomList a
empty = return []

singleton :: a -> RandomList a
singleton x = return [x]

    -- Fair merge of random lists
merge :: RandomList a -> RandomList a -> RandomList a
merge rxs rys = do
  xs <- rxs
  ys <- rys
  merge' (length xs, xs) (length ys, ys)
  where
    merge' (0 , [])   (_ , ys)   = return ys
    merge' (_ , xs)   (0 , [])   = return xs
    merge' (nx, x:xs) (ny, y:ys) = do
      k <- randomRIO (1,nx+ny)   -- selection weighted by size
      if k <= nx
        then (x:) `liftM` ((nx-1, xs) `merge'` (ny, y:ys))
        else (y:) `liftM` ((nx, x:xs) `merge'` (ny-1, ys))
    merge' _ _ = error ("Unhandled case in merge function")

    -- Generate a random permutation in O(n log n)
permute :: [a] -> RandomList a
permute = fromList
    where
    fromList []  = empty
    fromList [x] = singleton x
    fromList xs  = (fromList l) `merge` (fromList r)
        where (l,r) = splitAt (length xs `div` 2) xs
