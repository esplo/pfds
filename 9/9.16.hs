import           Prelude                 hiding ( head
                                                , tail
                                                )
import           DeletableHeap                  ( DeletableHeap
                                                , delete
                                                )
import           BinomialHeap
import           Heap                           ( Heap(..) )

main :: IO ()
main = do
  let q0 = empty :: DeletableHeap BinomialHeap Int
  let q1 = insert 2 (insert 1 (insert 0 q0))
  print q1
  let d1 = delete 1 q1
  print d1
  -- deleted
  let d2 = delete 0 d1
  print d2
