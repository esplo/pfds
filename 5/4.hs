data Tree a = E | T (Tree a) a (Tree a) deriving Show

bigger :: Ord a => a -> Tree a -> Tree a
bigger _ E = E
bigger pivot (T a x b)
  | x <= pivot = bigger pivot b
  | otherwise = bigger' a
  where
    bigger' E = T E x b
    bigger' (T a1 y a2)
      | y <= pivot = T (bigger pivot a2) x b
      | otherwise = T (bigger pivot a1) y (T a2 x b)

smaller :: Ord a => a -> Tree a -> Tree a
smaller _ E = E
smaller pivot (T a x b)
  | x > pivot = smaller pivot a
  | otherwise = smaller' b
  where
    smaller' E = T a x E
    smaller' (T b1 y b2)
      | y > pivot = T a x (smaller pivot b1)
      | otherwise = T (T a x b1) y (smaller pivot b2)


main :: IO ()
main = do
    let s = foldl (\y x -> T y x E) E [1..7]
    let s2 = foldr (\x y -> T E x y) E [1..7]
    print s
    print s2
    print $ bigger 0 s
    print $ bigger 2 s
    print $ bigger 3 s
    print $ bigger 4 s
    print $ smaller 2 s2
    print $ smaller 3 s2
    print $ smaller 4 s2
    print $ smaller 5 s2
    print $ smaller 8 s2
