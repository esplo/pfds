import Prelude hiding (head, tail, lookup)

data Digit = One | Two deriving (Show)

inc :: [Digit] -> [Digit]
inc [] = [One]
inc (One : ds) = Two : ds
inc (Two : ds) = One : inc ds

dec :: [Digit] -> [Digit]
dec [] = error "Empty"
dec [One] = []
dec (One : ds) = Two : dec ds
dec (Two : ds) = One : ds


main :: IO ()
main = do
    let u = foldl (\x y -> inc x) [] [0..10] -- 11
    print u
    print $ dec u
    let u2 = foldl (\x y -> inc x) [] [0..14] -- 15
    print u2
    print $ dec u2
