import ZerolessBinaryRandomAccessList
import RandomAccessList

import Prelude hiding (head, tail, lookup)

main = do
    let u = foldl (flip cons) empty [0..10] :: BinaryList Int
    print u
    print $ head u
    print $ tail u
    print $ lookup 2 u
