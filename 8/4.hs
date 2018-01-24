import           Prelude hiding (cons, head, tail)

class Queue q where
  empty :: q a
  isEmpty :: q a -> Bool
  snoc :: q a -> a -> q a
  head :: q a -> a
  tail :: q a -> q a

data ConsQueue a q = CQ [a] q

ccons :: Queue q => ConsQueue a (q a) -> a -> ConsQueue a (q a)
ccons (CQ xs q) x = CQ (x:xs) q

ctail :: Queue q => ConsQueue a (q a) -> ConsQueue a (q a)
ctail (CQ [] _)     = error "empty"
ctail (CQ (x:xs) q) = CQ xs q

chead :: Queue q => ConsQueue a (q a) -> a
chead (CQ [] q)     = error "empty"
chead (CQ (x:xs) q) = x

main :: IO ()
main = do
  print "hoge"
