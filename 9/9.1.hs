import BinaryRandomAccessList
import RandomAccessList

import Prelude hiding (drop)

drop :: Int -> BinaryList a -> BinaryList a
drop k (BL []) = if k < 0 then error "Empty" else BL []
drop k (BL (Zero : d)) = drop k (BL d)
drop k (BL (One t : ts)) = if k >= size t
  then drop (k - size t) (BL ts)
  else BL(reverse (dropTree k t) ++ ts)
  where
    dropTree :: Int -> Tree a -> [Digit a]
    dropTree 0 t' = [One t']
    dropTree k' (Node ns l r) = if k' <= ns `div` 2 
        then One r : dropTree k' l 
        else Zero : dropTree (k' - (ns `div` 2)) l
    dropTree k' (Leaf _) = if k' > 1 then error "Empty" else []

main :: IO ()
main = do
    let t = cons 0 (cons 1 empty) :: BinaryList Int 
    print t
    print $ drop 1 t

    let u = foldl (flip cons) empty [0..4] :: BinaryList Int
    print u
    print $ drop 2 u