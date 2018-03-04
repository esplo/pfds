module ZerolessBinaryRandomAccessList where
  import Prelude hiding (head, tail, lookup)
  import RandomAccessList

  data Tree a = Leaf a | Node Int (Tree a) (Tree a) deriving (Show, Eq)
  data Digit a = One (Tree a) | Two (Tree a) (Tree a) deriving (Show, Eq)
  newtype BinaryList a = BL [Digit a] deriving (Show, Eq)

  size :: Tree a -> Int
  size (Leaf _) = 1
  size (Node w _ _) = w

  link :: Tree a -> Tree a -> Tree a
  link t1 t2 = Node (size t1 + size t2) t1 t2

  consTree :: Tree a -> [Digit a] -> [Digit a]
  consTree t [] = [One t]
  consTree t (One t2 : ts) = Two t t2 : ts
  consTree t (Two t2 t3 : ts) = One t : consTree (link t2 t3) ts

  unconsTree :: [Digit a] -> (Tree a, [Digit a])
  unconsTree [] = error "Empty"
  unconsTree [One t] = (t, [])
  unconsTree (Two t1 t2 : ts) = (t1, One t2 : ts)
  unconsTree (One t : ts) = let
    (Node _ t1 t2, ts') = unconsTree ts
    in (t, Two t1 t2 : ts')

  instance RandomAccessList BinaryList where
    empty = BL []
    isEmpty (BL ts) = null ts
    cons x (BL ts) = BL (consTree (Leaf x) ts)
    head (BL (One (Leaf x) : _)) = x
    head (BL (Two (Leaf x) _ : _)) = x
    tail (BL ts) = let (_, ts') = unconsTree ts in BL ts'

    lookup i' (BL ts') = look i' ts'
      where
        look _ [] = error "Subscript"
        look i (One t : ts) = if i < size t
          then lookTree i t
          else look (i - size t) ts
        look i (Two t1 t2 : ts) = if i < size t1 * 2
          then if i < size t1 then lookTree i t1 else lookTree (i - size t1) t2 
          else look (i - size t1 * 2) ts

        lookTree 0 (Leaf x) = x
        lookTree _ (Leaf _) = error "Subscript"
        lookTree i (Node w t1 t2) = if i < w `div` 2
          then lookTree i t1
          else lookTree (i - w `div` 2) t2

    update i' y' (BL ts') = BL (upd i' ts')
      where
        upd _ [] = error "Subscript"
        upd i (One t : ts) = if i < size t
          then One (updTree i y' t) : ts
          else One t : upd (i - size t) ts
        upd i (Two t1 t2 : ts) = if i < size t1 * 2
          then if i < size t1 then Two (updTree i y' t1) t2 : ts else Two t1 (updTree i y' t2) : ts
          else Two t1 t2 : upd (i - size t1 * 2) ts

        updTree :: Int -> a -> Tree a -> Tree a
        updTree 0 y (Leaf _) = Leaf y
        updTree _ _ (Leaf _) = error "Subscript"
        updTree i y (Node w t1 t2) = if i < w `div` 2
          then Node w (updTree i y t1) t2
          else Node w t1 (updTree (i - w `div` 2) y t2)
