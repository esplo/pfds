data Tree a = Nil | Node (Tree a) a (Tree a) deriving Show

complete :: Ord a => a -> Integer -> Tree a
complete _ 0 = Nil
complete x d = do
    let ltree = complete x (d-1)
    Node ltree x ltree

create2 :: Ord a => a -> Integer -> (Tree a, Tree a)
create2 x 0 = (Nil, Node Nil x Nil)
create2 x m = do
    let mx = m `div` 2
    let mi = (m-1) `div` 2
    let (xl, xr) = create2 x mx
    let (il, ir) = create2 x mi
    (Node xl x il, Node xr x il)

balanced :: Ord a => a -> Integer -> Tree a
balanced _ 0 = Nil
balanced x m = do
    let (l, _) = create2 x m
    l

main :: IO ()
main = do
    -- let tree = Node (Node Nil 1 Nil) 2 (Node (Node Nil 3 Nil) 4 (Node Nil 5 Nil))
    -- print tree

    let bt = balanced 1
    print (bt 3)
    print (bt 4)
    print (bt 5)
    print (bt 6)
    print (bt 7)
