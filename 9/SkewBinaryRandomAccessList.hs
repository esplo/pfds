{-# LANGUAGE InstanceSigs #-}

module SkewBinaryRandomAccessList
  ( module SkewBinaryRandomAccessList
  , module RandomAccessList
  ) where

import Prelude hiding (head, lookup, tail)
import RandomAccessList

data Tree a
  = Leaf a
  | Node a
         (Tree a)
         (Tree a)
  deriving (Show, Eq)

data SkewList a =
  SL [(Int, Tree a)]
  deriving (Show, Eq)

instance RandomAccessList SkewList where
  empty :: SkewList a
  empty = SL []
  isEmpty (SL ts) = null ts
  cons :: a -> SkewList a -> SkewList a
  cons x (SL ((w1, t1):(w2, t2):ts))
    | w1 == w2 = SL ((1 + w1 + w2, Node x t1 t2) : ts)
  cons x (SL ts) = SL ((1, Leaf x) : ts)
  head :: SkewList a -> a
  head (SL []) = error "empty list"
  head (SL ((1, Leaf x):ts)) = x
  head (SL ((w, Node x t1 t2):ts)) = x
  tail (SL []) = error "empty list"
  tail (SL ((1, Leaf x):ts)) = SL ts
  tail (SL ((w, Node x t1 t2):ts)) = SL ((w `div` 2, t1) : (w `div` 2, t2) : ts)
  lookup i (SL ts) = look i ts
    where
      look _ [] = error "Subscript"
      look i ((w, t):ts) =
        if i < w
          then lookTree w i t
          else look (i - w) ts
      lookTree 1 0 (Leaf x) = x
      lookTree 1 _ (Leaf _) = error "Subscript"
      lookTree w 0 (Node x t1 t2) = x
      lookTree w i (Node x t1 t2) =
        if i <= w'
          then lookTree w' (i - 1) t1
          else lookTree w' (i - 1 - w') t2
        where
          w' = w `div` 2
  update :: Int -> a -> SkewList a -> SkewList a
  update i' y (SL ts) = SL (upd i' ts)
        -- upd :: Int -> [(Int, Tree a)] -> [(Int, Tree a)]
    where
      upd _ [] = error "Subscript"
      upd i ((w, t):ts) =
        if i < w
          then (w, updTree w i t) : ts
          else (w, t) : upd (i - w) ts
        -- updTree :: Int -> Int -> Tree a -> Tree a
      updTree 1 0 (Leaf _) = Leaf y
      updTree 1 _ (Leaf _) = error "Subscript"
      updTree w 0 (Node x t1 t2) = Node y t1 t2
      updTree w i (Node x t1 t2) =
        if i <= w'
          then Node x (updTree w' (i - 1) t1) t2
          else Node x t1 (updTree w' (i - 1 - w') t2)
        where
          w' = w `div` 2
