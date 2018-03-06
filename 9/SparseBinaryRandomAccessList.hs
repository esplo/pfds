module SparseBinaryRandomAccessList where
    import Prelude hiding (head, tail, lookup)
    import RandomAccessList
  
    data Tree a = Leaf a | Node Int (Tree a) (Tree a) deriving (Show, Eq)
    newtype BinaryList a = BL [Tree a] deriving (Show, Eq)
  
    size :: Tree a -> Int
    size (Leaf _) = 1
    size (Node w _ _) = w
  
    link :: Tree a -> Tree a -> Tree a
    link t1 t2 = Node (size t1 + size t2) t1 t2
  
    consTree :: Tree a -> [Tree a] -> [Tree a]
    consTree t [] = [t]
    consTree t1 (t2 : ts) = if size t1 == size t2
        then consTree (link t1 t2) ts
        else t1 : t2 : ts
  
    unconsTree :: [Tree a] -> (Tree a, [Tree a])
    unconsTree [] = error "Empty"
    unconsTree (t:ts) = let (v, r) = del t in (v, r ++ ts)
      where
        del :: Tree a -> (Tree a, [Tree a])
        del a@(Leaf _) = (a, [])
        del (Node _ l r) = let
            (t1, t2) = del l
            in (t1, t2 ++ [r])  -- ここの++は引数に与えて再帰することで:になる。補助関数delも不要
  
    instance RandomAccessList BinaryList where
      empty = BL []
      isEmpty (BL ts) = null ts
      cons x (BL ts) = BL (consTree (Leaf x) ts)
      head (BL ts) = let (Leaf x, _) = unconsTree ts in x
      tail (BL ts) = let (_, ts') = unconsTree ts in BL ts'
  
      lookup i' (BL ts') = look i' ts'
        where
          look _ [] = error "Subscript"
          look i (t : ts) = if i < size t
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
          upd i (t : ts) = if i < size t
            then updTree i y' t : ts
            else t : upd (i - size t) ts
  
          updTree :: Int -> a -> Tree a -> Tree a
          updTree 0 y (Leaf _) = Leaf y
          updTree _ _ (Leaf _) = error "Subscript"
          updTree i y (Node w t1 t2) = if i < w `div` 2
            then Node w (updTree i y t1) t2
            else Node w t1 (updTree (i - w `div` 2) y t2)
  
