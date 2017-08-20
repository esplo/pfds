import Prelude hiding (head, tail, last, init)

data Deq a = Q [a] [a] deriving Show

empty :: Deq a
empty = Q [] []

isEmpty :: Deq a -> Bool
isEmpty (Q [] _) = True
isEmpty _ = False

checkf :: Deq a -> Deq a
checkf (Q [] r) = Q (reverse p2) p1
  where
    (p1, p2) = splitAt (length r `div` 2) r
checkf (Q f []) = Q p1 (reverse p2)
  where
    (p1, p2) = splitAt (length f `div` 2) f
checkf q = q

-- front
cons :: a -> Deq a -> Deq a
cons x (Q f r) = checkf $ Q (x:f) r

head :: Deq t -> t
head (Q [] []) = error "Empty"
head (Q [] (y:_)) = y -- ys = Nil
head (Q (x:_) _) = x

tail :: Deq a -> Deq a
tail (Q [] []) = error "Empty"
tail (Q [] _) = empty -- length r = 1
tail (Q (_:xs) r) = checkf $ Q xs r

-- back
snoc :: Deq a -> a -> Deq a
snoc (Q f r) x = checkf $ Q f (x:r)

last :: Deq t -> t
last (Q [] []) = error "Empty"
last (Q (x:_) []) = x -- xs = Nil
last (Q _ (y:_)) = y

init :: Deq a -> Deq a
init (Q [] []) = error "Empty"
init (Q _ []) = empty -- length f = 1
init (Q f (_:ys)) = checkf $ Q f ys

main :: IO ()
main = do
  let q2 = snoc ( snoc (snoc (snoc (snoc empty 3) 5) 1) 7) 2
  print $ isEmpty q2
  print q2
  print $ head q2
  print $ last q2
  let q3 = cons 6 (cons 9 q2)
  print q3
  let q4 = tail . tail $ tail q3
  print q4
  let q5 = init . init $ init q3
  print q5
  let q6 = init . init $ init q5
  print q6
  print $ tail q6
  print $ last q6
  print $ init q6
  print $ head q6
