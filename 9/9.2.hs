import BinaryRandomAccessList
import RandomAccessList

create :: Int -> a -> BinaryList a
create 0 _ = BL []
create n x = BL (reverse (make n (lookupLargest 1)))
  where
    make 0 _ = []
    make n' i = if n' >= i
        then One (ctree i) : make (n' - i) (i `div` 2)
        else Zero : make n' (i `div` 2)
    lookupLargest i = if i <= n
        then lookupLargest (i * 2)
        else i `div` 2
    ctree 1 = Leaf x
    ctree n' = 
        let t = ctree (n' `div` 2)
        in Node n' t t

main :: IO ()
main = do
    print $ create 10 1
    print $ create 2 1
