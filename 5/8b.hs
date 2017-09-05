data Heap a = E | T a [Heap a] deriving Show
data BinTree a =  BE | BT a (BinTree a) (BinTree a) deriving Show

mkRoot :: a -> BinTree a -> BinTree a
mkRoot a t = BT a t BE

merge :: Ord a => BinTree a -> BinTree a -> BinTree a
merge BE t = t
merge t BE = t
merge (BT x1 l1 _) (BT x2 l2 _)
  | x1 <= x2 = BT x1 (BT x2 l2 l1) BE
  | otherwise = BT x2 (BT x1 l1 l2) BE

mergePairs :: Ord a => BinTree a -> BinTree a
mergePairs BE = BE
mergePairs h@(BT _ _ BE) = h
mergePairs (BT x1 l1 (BT x2 l2 r2)) = merge (merge h1 h2) (mergePairs r2)
  where
    h1 = mkRoot x1 l1
    h2 = mkRoot x2 l2

-- insert :: Ord a => a -> BinTree a -> BinTree a
-- insert x BE = BT x BE BE
-- insert x h@(BT y a b)
--   | x <= y = BT x h BE
--   | otherwise = BT y (BT x BE a) b
insert :: Ord a => a -> BinTree a -> BinTree a
insert x BE = BT x BE BE
insert x h = merge (BT x BE BE) h

findMin :: BinTree a -> a
findMin BE = error "empty"
findMin (BT x _ BE) = x
findMin _ = error "invalid"

deleteMin :: Ord a => BinTree a -> BinTree a
deleteMin BE = error "empty"
deleteMin (BT _ a BE) = mergePairs a
deleteMin _ = error "invalid"

test :: Int -> (BinTree a -> BinTree a) -> BinTree a -> BinTree a
test 0 _ d = d
test n f d = test (n-1) f (f d)

main :: IO ()
main = do
    let heap = insert 4 (insert 3 (insert 2 (insert 1 BE)))
    let heap2 = insert 4 (insert 1 (insert 3 (insert 2 BE)))
    let heap3 = insert 4 (insert 10 (insert 1 (insert 5 (insert 2 BE))))

    print $ map (\x -> test x deleteMin heap) [0..4]
    print $ map (\x -> test x deleteMin heap2) [0..4]
    print $ map (\x -> test x deleteMin heap3) [0..4]
