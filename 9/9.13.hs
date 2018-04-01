import Prelude hiding (cons, head, tail, lookup)

data Tree a = Leaf a | Node Int (Tree a) (Tree a) deriving (Show, Eq)
data Digits a = Zero | Ones [Tree a] | Two (Tree a, Tree a) | Threes [(Tree a, Tree a, Tree a)] | Four (Tree a, Tree a, Tree a, Tree a) deriving (Show, Eq)

size :: Tree a -> Int
size (Leaf _) = 1
size (Node w _ _) = w

size2 :: (Tree a, Tree a) -> Int
size2 (t1,t2) = 2 * size t1

size3 :: (Tree a, Tree a, Tree a) -> Int
size3 (t1,t2,t3) = 3 * size t1

size4 :: (Tree a, Tree a, Tree a, Tree a) -> Int
size4 (t1,t2,t3,t4) = 4 * size t1


link :: Tree a -> Tree a -> Tree a
link t1 t2 = Node (size t1 + size t2) t1 t2

ones :: [Tree a] -> [Digits a] -> [Digits a]
ones [] ds = ds
ones x (Ones xs : ds) = Ones (x ++ xs) : ds
ones x ds = Ones x : ds

threes :: [(Tree a, Tree a, Tree a)] -> [Digits a] -> [Digits a]
threes [] ds = ds
threes x (Threes xs : ds) = Threes (x ++ xs) : ds
threes x ds = Threes x : ds


cons :: a -> [Digits a] -> [Digits a]
cons x ts = fixup (consTree (Leaf x) ts)

consTree :: Tree a -> [Digits a] -> [Digits a]
consTree t [] = [Ones [t]]
consTree t (Zero : ds) = ones [t] ds
consTree t (Ones (u:es) : ds) = Two (t,u) : ones es ds
consTree t (Two (t1,t2) : ds) = threes [(t,t1,t2)] ds
consTree t (Threes ((t1,t2,t3):es) : ds) = Four (t,t1,t2,t3) : threes es ds


tail :: [Digits a] -> [Digits a]
tail ts = fixup r
  where
    (n, r) = unconsTree ts

unconsTree :: [Digits a] -> (Tree a, [Digits a])
unconsTree [Ones [t]] = (t, [])
unconsTree ((Ones (t1:es)) : ds) = (t1, Zero : ones es ds)
unconsTree (Two (t1,t2) : ds) = (t1, ones [t2] ds)
unconsTree ((Threes ((t1,t2,t3):es)) : ds) = (t1, Two (t2,t3) : threes es ds)
unconsTree (Four (t1,t2,t3,t4) : ds) = (t1, threes [(t2,t3,t4)] ds)


fixup :: [Digits a] -> [Digits a]
fixup (Four (t1,t2,t3,t4) : ds) = Two (t1,t2) : consTree (link t3 t4) ds
fixup (Ones i : Four (t1,t2,t3,t4) : ds) = Ones i : Two (t1,t2) : consTree (link t3 t4) ds
fixup (Threes i : Four (t1,t2,t3,t4) : ds) = Threes i : Two (t1,t2) : consTree (link t3 t4) ds

fixup (Zero : ds) = Two (t1,t2) : r
  where
    ((Node _ t1 t2), r) = unconsTree ds
fixup (Ones i : Zero : ds) = Ones i : Two (t1,t2) : r
  where
    ((Node _ t1 t2), r) = unconsTree ds
fixup (Threes i : Zero : ds) = Threes i : Two (t1,t2) : r
  where
    ((Node _ t1 t2), r) = unconsTree ds
fixup ds = ds


head :: [Digits a] -> a
head [] = error "empty"
head [Zero] = error "NG"
head (Ones (Leaf x : es) : ds) = x
head (Two ((Leaf x),_) : ds) = x
head (Threes ((Leaf x, _, _) : es) : ds) = x
head (Four (Leaf x, _, _, _) : ds) = x

lookupTree :: Int -> Tree a -> a
lookupTree 0 (Leaf x) = x
lookupTree i (Leaf x) = error "NG"
lookupTree i (Node w t1 t2) = if i < w `div` 2 then lookupTree i t1 else lookupTree (i - w `div` 2) t2

lookupTree2 :: Int -> (Tree a, Tree a) -> a
lookupTree2 i (t1,t2) = if i < s then lookupTree i t1 else lookupTree (i-s) t2
  where
    s = size t1

lookupTree3 :: Int -> (Tree a, Tree a, Tree a) -> a
lookupTree3 i (t1,t2,t3) = if i < s1 then lookupTree i t1 else if i < s2 then lookupTree (i-s1) t2 else lookupTree (i-s2) t3
  where
    s1 = size t1
    s2 = size t2 + s1

lookupTree4 :: Int -> (Tree a, Tree a, Tree a, Tree a) -> a
lookupTree4 i (t1,t2,t3,t4) = if i < s1 then lookupTree i t1 else if i < s2 then lookupTree (i-s1) t2 else if i < s3 then lookupTree (i-s2) t3 else lookupTree (i-s3) t4
  where
    s1 = size t1
    s2 = size t2 + s1
    s3 = size t3 + s2

lookup :: Int -> [Digits a] -> a
lookup i [] = error "NG"
lookup i (Zero : ds) = lookup i ds
lookup i (Ones [e] : ds) = if i < size e then lookupTree i e else lookup (i - size e) ds
lookup i (Ones (e:es) : ds) = if i < size e then lookupTree i e else lookup (i - size e) (Ones es : ds)
lookup i (Two t : ds) = if i < size2 t then lookupTree2 i t else lookup (i - size2 t) ds
lookup i (Threes [e] : ds) = if i < size3 e then lookupTree3 i e else lookup (i - size3 e) ds
lookup i (Threes (e:es) : ds) = if i < size3 e then lookupTree3 i e else lookup (i - size3 e) (Threes es : ds)
lookup i (Four t : ds) = if i < size4 t then lookupTree4 i t else lookup (i - size4 t) ds

-- TODO: Two (o,o), Ones [(o-o)] をtailすると、 Ones [o, o-o]になってしまう

incNum = 20
decNum = 19

main = do
  test incNum
  print "----------------------"
  test2 decNum

test 0 = do
  return ()
test n = do
  let u = foldl (flip cons) [] [1..n]
  test (n-1)
  print u
  print $ head u
  print $ lookup (n-1) u
  return ()

test2 0 = do
  return ()
test2 n = do
  let nt = [Two (Leaf 20,Leaf 19),Threes [(Node 2 (Leaf 18) (Leaf 17),Node 2 (Leaf 16) (Leaf 15),Node 2 (Leaf 14) (Leaf 13)),(Node 4 (Node 2 (Leaf 12) (Leaf 11)) (Node 2 (Leaf 10) (Leaf 9)),Node 4 (Node 2 (Leaf 8) (Leaf 7)) (Node 2 (Leaf 6) (Leaf 5)),Node 4 (Node 2 (Leaf 4) (Leaf 3)) (Node 2 (Leaf 2) (Leaf 1)))]]
  -- let aa = [Two (Leaf 6,Leaf 5),Zero,Ones [Node 4 (Node 2 (Leaf 4) (Leaf 3)) (Node 2 (Leaf 2) (Leaf 1))]]
  let u = foldl (\x y -> tail x) nt [1..n]
  test2 (n-1)
  print u
  print $ head u
  print $ lookup (decNum - n) u
  return ()
  