data Stream a = Nil | Cons a (Stream a) deriving Show

conc :: Stream a -> Stream a -> Stream a
conc Nil t = t
conc (Cons x xs) t = Cons x (conc xs t)

stake :: Int -> Stream a -> Stream a
stake 0 _ = Nil
stake _ Nil = Nil
stake n (Cons x xs) = Cons x (stake (n-1) xs)

-- 4.2
ssort :: Ord a => Stream a -> Stream a
ssort Nil = Nil
ssort s = ssort' Nothing s
  where
    ssort' :: Ord a => Maybe a -> Stream a -> Stream a
    ssort' _ Nil = Nil
    ssort' prev l =
      pick (findmin prev Nothing l)
      where
        pick Nothing = Nil
        pick (Just a) = Cons a (ssort' (Just a) l)

    findmin :: Ord a => Maybe a -> Maybe a -> Stream a -> Maybe a
    findmin Nothing Nothing (Cons x xs) = findmin Nothing (Just x) xs
    findmin Nothing (Just a) (Cons x xs)
      | a < x = findmin Nothing (Just a) xs
      | otherwise = findmin Nothing (Just x) xs
    findmin _ res Nil = res -- end
    findmin p@(Just u) Nothing (Cons x xs)
      | u < x = findmin p (Just x) xs
      | otherwise = findmin p Nothing xs
    findmin p@(Just u) (Just a) (Cons x xs)
      | u < x, a <= x = findmin p (Just a) xs
      | u < x, x < a = findmin p (Just x) xs
      | otherwise = findmin p (Just a) xs


main :: IO ()
main = do
    let s = Cons 15 (Cons 10 (Cons 20 Nil))
    let s2 = Cons 1 (Cons 50 Nil)
    print s
    print s2
    let s3 = conc s s2
    print s3
    print (stake 2 s)
    print (stake 2 (ssort s3))
