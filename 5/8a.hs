data Heap a = E | T a [Heap a] deriving Show
data BinTree a =  BE | BT a (BinTree a) (BinTree a) deriving Show

merge :: Ord a => Heap a -> Heap a -> Heap a
merge h E = h
merge E h = h
merge h1@(T x hs1) h2@(T y hs2)
  | x <= y = T x (h2:hs1)
  | otherwise = T y (h1:hs2)

insert :: Ord a => a -> Heap a -> Heap a
insert x = merge (T x [])

toBinary :: Ord a => Heap a -> BinTree a
toBinary E = BE
toBinary (T x xs) = BT x (lpick xs) BE
  where
    lpick ::  Ord a => [Heap a] -> BinTree a
    lpick [] = BE
    lpick (T v vs:ts) = BT v (lpick vs) (lpick ts)

main :: IO ()
main = do
    let heap = insert 4 (insert 1 (insert 2 (insert 7 E)))
    print $ heap
    print $ toBinary heap
