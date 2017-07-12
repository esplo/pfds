data Tree a = Nil | Node (Tree a) a (Tree a) deriving Show

member :: Ord a => a -> Tree a -> Bool
member _ Nil = False
member x (Node a y b)
    | x < y = member x a
    | x > y = member x b
    | otherwise = True

member2 :: Ord a => a -> Tree a -> Tree a -> Bool
member2 _ Nil Nil = False
member2 x Nil (Node _ v _) = x == v
member2 x n@(Node l y r) v
    | x <= y = member2 x l n
    | otherwise = member2 x r v

insert:: Ord a => a -> Tree a -> Tree a
insert x Nil = Node Nil x Nil
insert x t@(Node a y b)
    | x < y = Node (insert x a) y b
    | x > y = Node a y (insert x b)
    | otherwise = t

main :: IO ()
main = do
    let tree = Node (Node Nil 1 Nil) 2 (Node (Node Nil 3 Nil) 4 (Node Nil 5 Nil))
    print (member2 0 tree Nil == member 0 tree)
    print (member2 5 tree Nil == member 5 tree)

    let tree2 = insert 7 tree
    print (member2 6 tree2 Nil == member 6 tree2)
    print (member2 7 tree2 Nil == member 7 tree2)
