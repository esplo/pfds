module BinaryRandomAccessList where
  import Prelude hiding (head, tail, lookup)
  import RandomAccessList

  data Tree a = Leaf a | Node Int (Tree a) (Tree a) deriving (Show, Eq)
  data Digit a = Zero | One (Tree a) deriving (Show, Eq)
  newtype BinaryList a = BL [Digit a] deriving (Show, Eq)

  size :: Tree a -> Int
  size (Leaf _) = 1
  size (Node w _ _) = w

  link :: Tree a -> Tree a -> Tree a
  link t1 t2 = Node (size t1 + size t2) t1 t2

  consTree :: Tree a -> [Digit a] -> [Digit a]
  consTree t [] = [One t]
  consTree t (Zero : ts) = One t : ts
  consTree t1 (One t2 : ts) = Zero : consTree (link t1 t2) ts

  unconsTree :: [Digit a] -> (Tree a, [Digit a])
  unconsTree [] = error "Empty"
  unconsTree [One t] = (t, [])
  unconsTree (One t : ts) = (t, Zero : ts)
  unconsTree (Zero : ts) = let
    (Node _ t1 t2, ts') = unconsTree ts
    in (t1, One t2 : ts')

  instance RandomAccessList BinaryList where
    empty = BL []
    isEmpty (BL ts) = null ts
    cons x (BL ts) = BL (consTree (Leaf x) ts)
    head (BL ts) = let (Leaf x, _) = unconsTree ts in x
    tail (BL ts) = let (_, ts') = unconsTree ts in BL ts'

    lookup i' (BL ts') = look i' ts'
      where
        look _ [] = error "Subscript"
        look i (Zero : ts) = look i ts
        look i (One t : ts) = if i < size t
          then lookTree i t
          else look (i - size t) ts

        lookTree 0 (Leaf x) = x
        lookTree _ (Leaf _) = error "Subscript"
        lookTree i (Node w t1 t2) = if i < w `div` 2
          then lookTree i t1
          else lookTree (i - w `div` 2) t2

    update i' y' (BL ts') = BL (upd i' ts')
      where
        upd _ [] = error "Subscript"
        upd i (Zero : ts) = Zero : upd i ts
        upd i (One t : ts) = if i < size t
          then One (updTree i y' t) : ts
          else One t : upd (i - size t) ts

        updTree :: Int -> a -> Tree a -> Tree a
        updTree 0 y (Leaf _) = Leaf y
        updTree _ _ (Leaf _) = error "Subscript"
        updTree i y (Node w t1 t2) = if i < w `div` 2
          then Node w (updTree i y t1) t2
          else Node w t1 (updTree (i - w `div` 2) y t2)
