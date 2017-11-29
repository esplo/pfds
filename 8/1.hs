data Color = R | B deriving Show
data Tree a = NT Int Int (Tree' a) deriving Show
data Tree' a = E | T Bool Color (Tree' a) a (Tree' a) deriving Show

data Digit a = One a (Tree' a) | Two a (Tree' a) a (Tree' a)

-- empty :: Tree a
-- empty = E

member :: Ord a => a -> Tree a -> Bool
member _ (NT _ _ E) = False
member x (NT _ _ (T _ _ a y b))
    | x < y = member x (NT 0 0 a)
    | y < x = member x (NT 0 0 b)
    | otherwise = True

balance :: Bool -> Color -> Tree' a -> a -> Tree' a -> Tree' a
balance b0 B (T b1 R (T b2 R a x b) y c) z d = T b1 R (T b2 B a x b) y (T b0 B c z d)
balance b0 B (T b1 R a x (T b2 R b y c)) z d = T b2 R (T b1 B a x b) y (T b0 B c z d)
balance b0 B a x (T b1 R (T b2 R b y c) z d) = T b2 R (T b0 B a x b) y (T b1 B c z d)
balance b0 B a x (T b1 R b y (T b2 R c z d)) = T b1 R (T b0 B a x b) y (T b2 B c z d)
balance b0 c a x b = T b0 c a x b

insert :: Ord t => t -> Tree t -> Tree t
insert x (NT l d s) = NT (l+1) d (T flag B a y b)
  where ins E = T True R E x E
        ins s@(T flag color a y b)
          | x < y = balance flag color (ins a) y b
          | x > y = balance flag color a y (ins b)
          | otherwise = s
        T flag _ a y b = ins s

-- 3.9
incr :: Digit a -> [Digit a] -> [Digit a]
incr (One a t) [] = [One a t]
incr (One a1 t1) (One a2 t2 : ps) = Two a1 t1 a2 t2 : ps
incr (One a1 t1) (Two a2 t2 a3 t3 : ps) = One a1 t1 : incr (One a2 (T True B t2 a3 t3)) ps

add :: [Digit a] -> a -> [Digit a]
add ps a = incr (One a E) ps

link :: Tree' a -> Digit a -> Tree' a
link r (One a t)         = T True B t a r
link r (Two a1 t1 a2 t2) = T True B t2 a2 (T True R t1 a1 r)

fromOrdList :: [a] -> Tree a
fromOrdList ary = NT (length ary) 0 (foldl link E $ foldl add [] ary)

-- 8.1
delete :: Ord a => Tree a -> a -> Tree a
delete (NT l d s) y = check $ NT l (d+1) (delete' s y)
  where
    delete' :: Ord a => Tree' a -> a -> Tree' a
    delete' E _ = error "not found"
    delete' (T flag c a x b) y
      | x < y = T flag c a x (delete' b y)
      | x > y = T flag c (delete' a y) x b
      | otherwise = T False c a x b

check :: Ord a => Tree a -> Tree a
check s@(NT l d t)
  | l >= (d*2) = s
  | l < (d*2) = fromOrdList $ pick t
    where
      pick :: Tree' a -> [a]
      pick E             = []
      pick (T f _ a x b) = (pick a) ++ (fil f x) ++ (pick b)
      fil f a = [a | f]


main :: IO ()
main = do
  let tree = insert 2 (insert 1 (NT 0 0 E))
  let t2 = insert 4 (insert 3 tree)
  print t2
  -- NT 4 0 (T True B (T True B E 1 E) 2 (T True B E 3 (T True R E 4 E)))
  let t3 = delete t2 2
  print t3
  -- NT 4 1 (T False B (T True B E 1 E) 2 (T True B E 3 (T True R E 4 E)))
  let t4 = delete t3 1
  print t4
  -- NT 4 2 (T False B (T False B E 1 E) 2 (T True B E 3 (T True R E 4 E)))
  let t5 = delete t4 3
  print t5
  -- NT 1 0 (T True B E 4 E)
