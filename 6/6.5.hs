-- 正格評価をしていないので、以下は雰囲気のみ伝えるもの

data Tree a = Node Int a [Tree a] deriving Show
newtype BinomialHeap a = BH [Tree a]

isEmpty ts = ts

rank (Node r _ _) = r
root (Node _ x _) = x

link t1@(Node r x1 c1) t2@(Node _ x2 c2) =
  if x1 <= x2 then Node (r+1) x1 (t2:c1) else Node (r+1) x2 (t1:c2)

insTree t [] = [t]
insTree t ts@(t':ts') =
  if rank t < rank t' then t : ts else insTree (link t t') ts'

mrg ts1 [] = ts1
mrg [] ts2 = ts2
mrg ts1@(t1:ts1') ts2@(t2:ts2')
  | rank t1 < rank t2 = t1 : mrg ts1' ts2
  | rank t2 < rank t1 = t2 : mrg ts1 ts2'
  | otherwise = insTree (link t1 t2) (mrg ts1' ts2')

removeMinTree [] = error "empty"
removeMinTree [t] = (t, [])
removeMinTree (t:ts) = if root t < root t' then (t,ts) else (t', t:ts')
  where (t', ts') = removeMinTree ts


-- ここまでBinomialHeapの実装

-- ↓ bh_.*は元のBinomialHeapのメソッド

data SizedHeap a = SH Int (BinomialHeap a)

isEmpty (SH len _) = len = 0
rank (SH _ bh) = bh_rank bh
root (SH _ bh) = bh_root bh

insTree t (SH len bh) = SH (len+1) (bh_insTree t bh)
mrg (SH l1 bh1) (SH l2 bh2) = SH (l1+l2) (bh_mrg bh1 bh2)

findMin (SH _ bh) = bh_findMin bh
deleteMin (SH len bh) = SH (len-1) (bh_deleteMin bh)


main :: IO ()
main = do
  print 0
