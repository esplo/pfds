-- from SML

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module NonzeroQuaternaryRandomAccessList where

import           RandomAccessList
import           Prelude                 hiding ( head
                                                , tail
                                                , lookup
                                                )

data Tree a = Leaf a | Node [Tree a] deriving (Show, Eq)
type RList a = [Tree a]

-- type instance Elem (RList a) = a

instance RandomAccessList (RList a) where
  empty :: RList a
  empty = []

  isEmpty :: RList a -> Bool
  isEmpty [] = True
  isEmpty _ = False

  cons :: a -> RList a -> RList a
  cons x [] = [Leaf x]
  cons x ([h1]:xs) = [h1, Leaf x] : xs
  cons x ([h1]:xs) = [h1, Leaf x] : xs
  cons x ([h1,h2]:xs) = [h1, h2, Leaf x] : xs
  cons x ([h1,h2,h3]:xs) = [h1, h2, h3, Leaf x] : xs
  cons x ([h1,h2,h3,h4]:xs) = [Leaf x] : [Node [h1, h2, h3, h4]] : xs

  head :: RList a -> a
  head [] = error "empty"
  head ((Leaf x : _) : _) = x
  head ((Node (x:_) : _) : _) = x

  tail :: RList a -> RList a
  tail [] = error "empty"
  tail ((_ : xs) : xxs) = xs : xxs

  lookup :: Int -> RList a -> a
  lookup i [] = error "Subscript"
  lookup i xs = lookupTree i xs

  update :: Int -> a -> RList a -> RList a
  update i _ [] = error "Subscript"
  update i v xs = updateTree i v xs

-- helper functions
rank :: Tree a -> Int
rank (Leaf _) = 0
rank (Node (x : _)) = 1 + rank x

size :: Tree a -> Int
size x = 4 ^ rank x

lookupTree :: Int -> [Tree a] -> a
lookupTree i (x:xs) = if i < size x then lookupNode i x else lookupTree (i-size x) xs
lookupNode :: Int -> Tree a -> a
lookupNode 0 (Leaf x) = x
lookupNode i (Node xs@(x:_)) = lookupNode (i-(size x * df)) (xs !! df)
  where
    df = i `div` size x

updateTree :: Int -> a -> [Tree a] -> [Tree a]
updateTree i v (x:xs) = if i < size x then updateNode i v x : xs else x : updateTree (i-size x) v xs
updateNode :: Int -> a -> Tree a -> Tree a
updateNode 0 v (Leaf x) = (Leaf v)
updateNode i v (Node xs@(x:_)) = updateNode (i-(size x * df)) v (xs !! df)
  where
    df = i `div` size x

-- ゼロなしなので楽？
