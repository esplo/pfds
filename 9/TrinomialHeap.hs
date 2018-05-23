module TrinomialHeap where
import           Prelude                 hiding ( (+) )
import           Heap                           ( Heap(..) )

data Tree e =
  Node e
       [(Tree e, Tree e)]
  deriving (Show)

data Digit e =
  Zero
  | One (Tree e)
  | Two (Tree e, Tree e)
  deriving (Show)

newtype TrinomialHeap e =
  TH [Digit e]
  deriving (Show)

instance Heap TrinomialHeap where
  empty = TH []
  isEmpty (TH ts) = null ts
  insert x (TH ts) = TH $ consTree (Node x []) ts
  merge (TH ts1) (TH ts2) = TH $ ts1 + ts2
  -- findMin (TH ts) = root . fst . removeMinTree $ ts
  -- deleteMin (TH ts) =
    -- let (Node _ _ ts1, ts2) = removeMinTree ts
    --  in TH $ merge' (reverse ts1) ts2

val :: Tree e -> e
val (Node e _) = e
ntree :: Tree e -> [(Tree e, Tree e)]
ntree (Node _ a) = a

gmin :: Ord e => Tree e -> Tree e -> Tree e
gmin n1 n2 = if val n1 <= val n2 then n1 else n2

gmax :: Ord e => Tree e -> Tree e -> Tree e
gmax n1 n2 = if val n1 <= val n2 then n2 else n1

gmid :: Ord e => Tree e -> Tree e -> Tree e -> Tree e
gmid a b c = gmin (gmax a b) (gmin (gmax b c) (gmax c a))

consTree :: Ord e => Tree e -> [Digit e] -> [Digit e]
consTree t []                  = [One t]
consTree t (Zero         : xs) = One t : xs
consTree t (One n1       : xs) = Two (gmin t n1, gmax t n1) : xs
consTree t (Two (n1, n2) : xs) = Zero : consTree (link n1 n2 t) xs

link :: Ord e => Tree e -> Tree e -> Tree e -> Tree e
link t1 t2 t3 = Node v ((b, c) : r)
 where
  a = gmin t1 (gmin t2 t3)
  b = gmid t1 t2 t3
  c = gmax t1 (gmax t2 t3)
  v = val a
  r = ntree a

rank :: Tree e -> Int
rank (Node _ ary) = length ary

addTree (One d1) (One d2        ) = Two (gmin d1 d2, gmax d1 d2)
addTree (One d1) (Two (d21, d22)) = if vd1 <= vd21
  then One vd1 ((d21, d22) : rd1)
  else One vd21 ((d1, d22) : rd2)
 where
  vd1  = val d1
  vd21 = val d21
  rd1  = ntree d1
  rd2  = ntree d21

(+) :: Ord e => [Digit e] -> [Digit e] -> [Digit e]
ts1            + []             = ts1
[]             + ts2            = ts2
(d      : ds1) + (Zero   : ds2) = d : (ds1 + ds2)
(Zero   : ds1) + (d      : ds2) = d : (ds1 + ds2)
(One d1 : ds1) + (One d2 : ds2) = (addTree d1 d2) : (ds1 + ds2)
(t1@(One d1) : ds1) + (t2@(Two (d21, d22)) : ds2) =
  Zero : consTree (addTree t1 t2) (ds1 + ds2)
-- TODO:

  -- removeMinTree :: Ord a => [Tree a] -> (Tree a, [Tree a])
  -- removeMinTree [] = undefined
  -- removeMinTree [t] = (t, [])
  -- removeMinTree (t:ts) = let
  --   (t', ts') = removeMinTree ts
  --   in if root t <= root t'
  --     then (t, ts)
  --     else (t', t:ts')
