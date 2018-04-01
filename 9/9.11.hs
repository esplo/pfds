
data Tree a = Node a [Tree a] deriving (Show, Eq)
data Digit a = Zero | Ones [Tree a] | Two (Tree a) (Tree a) deriving (Show, Eq)
newtype Heap a = BH [Digit a] deriving (Show, Eq)

ones :: [Tree a] -> [Digit a] -> [Digit a]
ones [] ds = ds
ones x (Ones xs : ds) = Ones (x++xs) : ds
ones x ds = Ones x : ds

link :: Tree a -> Tree a -> Tree a
link (Node a1 t1) n2 = Node a1 (n2:t1)

fixup :: [Digit a] -> [Digit a]
fixup ((Two t1 t2) : ds) = Zero : insTree (link t1 t2) ds
fixup (Ones ot : Two t1 t2 : ds) = Ones ot : Zero : insTree (link t1 t2) ds
fixup ds = ds

insTree :: Tree a -> [Digit a] -> [Digit a]
insTree t [] = [Ones [t]]
insTree t (Zero : xs) = ones [t] xs
insTree t (Ones (ot:ds) : xs) = Two t ot : ones ds xs

insert :: a -> Heap a -> Heap a
insert x (BH ts) = BH (fixup (insTree (Node x []) ts))



main = do
  test 10

test 0 = do
  return ()
test n = do
  let u = foldl (flip insert) (BH []) [1..n]
  test (n-1)
  -- print $ tail u
  print u
  return ()