import SparseBinaryRandomAccessList
import RandomAccessList

import Prelude hiding (head, tail, lookup)

main :: IO ()
main = do
    let u = foldl (flip cons) empty [0..4] :: BinaryList Int
    print u
    print $ head u
    let u2 = tail u
    print u2
    print $ head u2
    let u3 = tail u2
    print u3
    print $ head u3
    