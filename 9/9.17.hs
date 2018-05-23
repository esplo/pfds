import           Prelude                 hiding ( head
                                                , tail
                                                )
import           TrinomialHeap
import           Heap                           ( Heap(..) )

main :: IO ()
main = do
  let q0 = empty :: TrinomialHeap Int
  let q1 = insert 2 (insert 1 (insert 0 q0))
  print q1
