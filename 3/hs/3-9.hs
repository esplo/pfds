data Color = R | B deriving Show
data Tree a = E | T Color (Tree a) a (Tree a) deriving Show

empty :: Tree a
empty = E

member :: Ord a => a -> Tree a -> Bool
member _ E = False
member x (T _ a y b)
    | x < y = member x a
    | y < x = member x b
    | otherwise = True

balance :: Color -> Tree a -> a -> Tree a -> Tree a
balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance c a x b = T c a x b

insert :: Ord t => t -> Tree t -> Tree t
insert x s = T B a y b
  where ins E = T R E x E
        ins s@(T color a y b)
          | x < y = balance color (ins a) y b
          | x > y = balance color a y (ins b)
          | otherwise = s
        T _ a y b = ins s

-- 3.9
oneTwoNumber :: Int -> [Int]
oneTwoNumber 0 = []
oneTwoNumber n = reverse $ iterate inc' [] !! n
  where
    inc' :: [Int] -> [Int]
    inc' (x:xs)
      | x == 1 = 2 : xs
      | x == 2 = 1 : inc' xs
    inc' _ = [1]

-- return (created tree, remaining list)
compTree :: Ord a => Int -> [a] -> (Tree a, [a])
compTree 0 xs = (E, xs)
compTree n xs = (tree, rr)
  where
    (left, r) = compTree (n-1) xs
    v = head r
    (right, rr) = compTree (n-1) (tail r)
    tree = T B left v right

fromOrdList :: Ord a => [a] -> Tree a
fromOrdList [] = E
fromOrdList t = fst $ work (oneTwoNumber $ length t) t
  where
    work :: Ord a => [Int] -> [a] -> (Tree a, [a])
    work [] xs = (E, xs)
    work (x:xs) l =
      if x == 1 then
        let (ctree, rr) = compTree lxs ta
        in (T B left h ctree, rr)
      else
        let (ctree1, rr1) = compTree lxs ta
            v = head rr1
            (ctree2, rr2) = compTree lxs (tail rr1)
        in (T B (T R left h ctree1) v ctree2, rr2)
      where
        (left, h:ta) = work xs l
        lxs = length xs


-- 3.10 (a)
lbalance :: Color -> Tree a -> a -> Tree a -> Tree a
lbalance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
lbalance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
lbalance c a x b = T c a x b

rbalance :: Color -> Tree a -> a -> Tree a -> Tree a
rbalance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
rbalance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
rbalance c a x b = T c a x b

insert2 :: Ord t => t -> Tree t -> Tree t
insert2 x s = T B a y b
  where ins E = T R E x E
        ins s@(T color a y b)
          | x < y = lbalance color (ins a) y b
          | x > y = rbalance color a y (ins b)
          | otherwise = s
        T _ a y b = ins s

-- 3.10 (b)
llbalance :: Color -> Tree a -> a -> Tree a -> Tree a
llbalance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
llbalance c a x b = T c a x b

lrbalance :: Color -> Tree a -> a -> Tree a -> Tree a
lrbalance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
lrbalance c a x b = T c a x b

rlbalance :: Color -> Tree a -> a -> Tree a -> Tree a
rlbalance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
rlbalance c a x b = T c a x b

rrbalance :: Color -> Tree a -> a -> Tree a -> Tree a
rrbalance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
rrbalance c a x b = T c a x b

insert3 :: Ord t => t -> Tree t -> Tree t
insert3 x s = T B a y b
  where ins E = T R E x E
        ins s@(T color a y b)
          | x < y, isRed insa = llbalance color insa y b
          | x < y = lrbalance color insa y b
          | x > y, isRed insb = rlbalance color a y insb
          | x > y = rrbalance color a y insb
          | otherwise = s
          where
            insa = ins a
            insb = ins b

        isRed (T R (T R _ _ _) _ _) = True
        isRed _ = False

        T _ a y b = ins s


main :: IO ()
main = do
  let tree = ins 2 (ins 1 E)
  print tree
  print (member 1 tree)
  print (member 5 tree)
  let tree2 = ins 4 (ins 3 tree)
  print tree2
  print (member 5 tree2)
  let tree3 = ins 6 (ins 5 tree2)
  print tree3
  print (member 5 tree3)
  -- print (fromOrdList [1, 3, 4, 7, 11])
  let t1 = ins 1 (ins 3 (ins 4 (ins 7 (ins 8 (ins 11 (ins 14 (ins 16 (ins 17 (ins 19 E)))))))))
  print t1
  -- let elems = [1, 3, 4, 7, 8, 11, 14, 16, 17, 19]
  --let t2 = fromOrdList elems
  --print t2

  print $ map oneTwoNumber [1..10]

  print . fromOrdList $ take 1 [1..]
  print . fromOrdList $ take 2 [1..]
  print . fromOrdList $ take 3 [1..]
  print . fromOrdList $ take 4 [1..]
  print . fromOrdList $ take 5 [1..]
  print . fromOrdList $ take 6 [1..]
  print . fromOrdList $ take 7 [1..]
  print . fromOrdList $ take 8 [1..]

  where
    ins = insert3
