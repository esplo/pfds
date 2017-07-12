import Control.Exception
import Data.Maybe

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

_insert:: Ord a => a -> Tree a -> Tree a -> Maybe (Tree a)
_insert _ Nil Nil = Just Nil
_insert x Nil (Node _ y _)
    | x == y = Nothing
    | otherwise = Just (Node Nil x Nil)
_insert x t@(Node a y b) _
    | x <= y = case _insert x a t of
        Just v -> Just (Node v y b)
        Nothing -> Nothing
    | otherwise = case _insert x b t of
        Just v -> Just (Node a y v)
        Nothing -> Nothing

insert :: Ord a => a -> Tree a -> Tree a
insert x t = fromMaybe t (_insert x t Nil)


main :: IO ()
main = do
    let tree = Node (Node Nil 1 Nil) 2 (Node Nil 5 (Node Nil 7 Nil))
    print tree
    print (member2 6 tree Nil)
    print (member2 7 tree Nil)

    let tree2 = insert 7 tree
    print tree2
    print (member2 6 tree2 Nil)
    print (member2 7 tree2 Nil)

    let tree3 = insert 3 (insert 6 tree)
    print tree3
    print (member2 6 tree3 Nil)
    print (member2 7 tree3 Nil)
