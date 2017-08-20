data Tree a = E | T (Tree a) a (Tree a) deriving Show

partition :: Ord a => a -> Tree a -> (Tree a, Tree a)
partition _ E = (E, E)
partition pivot t@(T a x b) =
  if x <= pivot then
    case b of
      E -> (t, E)
      T b1 y b2 ->
        if y <= pivot then
          let (small, big) = partition pivot b2
          in (T (T a x b1) y small, big)
        else
          let (small, big) = partition pivot b1
          in (T a x small, T big y b2)
  else
    case a of
      E -> (E, t)
      T a1 y a2 ->
        if y <= pivot then
          let (small, big) = partition pivot a2
          in (T a1 y small, T big x b)
        else
          let (small, big) = partition pivot a1
          in (small, T big y (T a2 x b))

insert :: Ord t => t -> Tree t -> Tree t
insert x t = T a x b
  where (a, b) = partition x t

fromList :: Ord a => [a] -> Tree a
fromList [] = E
fromList xs = foldl (flip insert) E xs

sortTree :: Ord a => Tree a -> [a]
sortTree E = []
sortTree (T a x b) = sortTree a ++ [x] ++ sortTree b

makeAndSort :: Ord a => [a] -> [a]
makeAndSort = sortTree . fromList


main :: IO ()
main = do
    print $ makeAndSort [8, 9, 3, 5]
    let tt = makeAndSort [1,2..7]
    print $ tt
    let tt2 = makeAndSort [7,6..1]
    print $ tt2

-- ソート済みの配列に対して
-- 昇順のリストに対し、partitionは(t, E)を返す。
-- この呼び出しは内部でpartitionを呼び出さず、O(1)である。
-- これを要素ごとに繰り返すため、O(n)
-- また、sortTree関数は各要素を1度ずつ辿るのみなのでO(n)
