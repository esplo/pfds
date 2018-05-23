-- from SML

module BinomialHeap where

  import Heap (Heap (..))
  data Tree e = Node Int e [Tree e] deriving (Show)
  newtype BinomialHeap e = BH [Tree e] deriving (Show)
  
  instance Heap BinomialHeap where
    empty = BH []
    isEmpty (BH ts) = null ts
    insert x (BH ts) = BH $ insTree (Node 0 x []) ts
    merge (BH ts1) (BH ts2) = BH $ merge' ts1 ts2
    findMin (BH ts) = root . fst . removeMinTree $ ts
    deleteMin (BH ts) =
      let (Node _ _ ts1, ts2) = removeMinTree ts
      in BH $ merge' (reverse ts1) ts2
  
  rank :: Tree e -> Int
  rank (Node r _ _) = r
  
  root :: Tree e -> e
  root (Node _ x _) = x
  
  link :: Ord e => Tree e -> Tree e -> Tree e
  link t1@(Node r x1 c1) t2@(Node _ x2 c2) =
    if x1 <= x2
      then Node (r+1) x1 (t2:c1)
      else Node (r+1) x2 (t1:c2)
  
  merge' :: Ord e => [Tree e] -> [Tree e] -> [Tree e]
  merge' ts1 [] = ts1
  merge' [] ts2 = ts2
  merge' ts1@(t1:ts1') ts2@(t2:ts2')
    | rank t1 < rank t2 = t1:merge' ts1' ts2
    | rank t2 < rank t1 = t2:merge' ts1 ts2'
    | otherwise = insTree (link t1 t2) (merge' ts1' ts2')
  
  insTree :: Ord e => Tree e -> [Tree e] -> [Tree e]
  insTree t [] = [t]
  insTree t ts@(t':ts') =
    if rank t < rank t'
      then t:ts
      else insTree (link t t') ts'
  
  removeMinTree :: Ord a => [Tree a] -> (Tree a, [Tree a])
  removeMinTree [] = undefined
  removeMinTree [t] = (t, [])
  removeMinTree (t:ts) = let
    (t', ts') = removeMinTree ts
    in if root t <= root t'
      then (t, ts)
      else (t', t:ts')