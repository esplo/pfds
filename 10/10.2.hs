import Prelude hiding (head,tail,lookup)
import AltBinaryRandomAccessList
import RandomAccessList

main :: IO ()
main = do
    let u = foldl (\z x -> cons x z) empty [0..10] :: BinaryList Int
    print u
    let u' = update 5 (-1) u
    print u'
    print $ lookup 3 u'

-- これはただのAltBinaryRandomAccessList
-- 時間が許せば冗長二進数にする
