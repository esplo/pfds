import Prelude hiding (head, tail)

data Tree a = Leaf a | Node Int (Tree a) (Tree a) deriving (Show, Eq)
data Digit a = One (Tree a) | Two (Tree a, Tree a) | Three (Tree a, Tree a, Tree a) deriving (Show)

size :: Tree a -> Int
size (Leaf x) = 1
size (Node w _ _) = w

link :: (Tree a, Tree a) -> Tree a
link (t1,t2) = Node (size t1 + size t2) t1 t2

cons :: a -> [Digit a] -> [Digit a]
cons x xs = cons' (Leaf x) xs

cons' :: Tree a -> [Digit a] -> [Digit a]
cons' x [] = [One x]
cons' x (One t2 : xs) = Two (x, t2) : xs
cons' x (Two (t1,t2) : xs) = Three (x,t1,t2) : xs
cons' x (Three (t1,t2,t3) : xs) = Two (x,t1) : cons' (link (t2,t3)) xs

tail :: [Digit a] -> [Digit a]
tail xs = r
  where
    (_, r) = tail' xs

tail' :: [Digit a] -> (Tree a, [Digit a])
tail' [One x] = (x,[])
tail' (One x : xs) = error "NG"
tail' [Two (t1,t2)] = (t1,[One t2])
tail' (Two (t1,t2) : xs) = (t1, Three (t2,e1,e2) : xs')
  where
    ((Node _ e1 e2), xs') = tail' xs
tail' (Three (t1,t2,t3) : xs) = (t1, Two (t2,t3) : xs)

main = do
  test 10

test 0 = do
  return ()
test n = do
  let u = foldl (\x y -> cons y x) [] [1..n]
  test (n-1)
  print $ tail u
  print u
  return ()
