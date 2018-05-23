module DeletableHeap
  ( module DeletableHeap
  ) where

import qualified Heap as H

data DeletableHeap h a =
  DQ (h a)
     (h a)
  deriving (Show)

delete :: (Ord a, H.Heap h) => a -> DeletableHeap h a -> DeletableHeap h a
delete i (DQ h1 h2) = check r
  where
    r =
      if H.findMin h1 == i
        then DQ (H.deleteMin h1) h2
        else DQ h1 (H.insert i h2)

check :: (Ord a, H.Heap h) => DeletableHeap h a -> DeletableHeap h a
check o@(DQ hp hn) =
  if not (H.isEmpty hp) && not (H.isEmpty hn) && H.findMin hp == H.findMin hn
    then check (DQ (H.deleteMin hp) (H.deleteMin hn))
    else o

instance H.Heap h => H.Heap (DeletableHeap h) where
  empty = DQ H.empty H.empty
  isEmpty (DQ h _) = H.isEmpty h
  insert v (DQ hp hm) = DQ (H.insert v hp) hm
  merge (DQ h1p h1n) (DQ h2p h2n) = DQ (H.merge h1p h2p) (H.merge h1n h2n)
  findMin (DQ h _) = H.findMin h
  deleteMin (DQ h hn) = DQ (H.deleteMin h) hn
