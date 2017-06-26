data Map a b = Nil | Node (Map a b) (a, b) (Map a b) deriving Show

empty :: Ord a => Map a b
empty = Nil

bind :: Ord a => a -> b -> Map a b -> Map a b
bind k v Nil = Node Nil (k, v) Nil
bind k v (Node a td@(x, _) b)
    | k < x = Node (bind k v a) td b
    | k > x = Node a td (bind k v b)
    | otherwise = Node a (k, v) b

mylookup :: Ord a => a -> Map a b -> Maybe b
mylookup _ Nil = Nothing
mylookup x (Node a y b)
    | x < fst y = mylookup x a
    | x > fst y = mylookup x b
    | otherwise = Just (snd y)

main :: IO ()
main = do
    let mp = (Node (Node Nil (1, "one") Nil) (2, "two") Nil)
    case mylookup 1 mp of
        Just v -> print v
        Nothing -> print "not found"
    case mylookup 3 mp of
        Just v -> print v
        Nothing -> print "not found"

    let mp2_1 = bind 7 "seven" (bind 2 "two" (bind 1 "one" empty))
    case mylookup 1 mp2_1 of
        Just v -> print v
        Nothing -> print "not found"
    case mylookup 3 mp2_1 of
        Just v -> print v
        Nothing -> print "not found"
    case mylookup 7 mp2_1 of
        Just v -> print v
        Nothing -> print "not found"
