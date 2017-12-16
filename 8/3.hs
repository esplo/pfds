import           Prelude hiding (head, tail)

data RotationState a =
  Idle
  | Reversing Int [a] [a] [a] [a]
  | Appending Int [a] [a]
  | Done [a]
  deriving Show

-- diff = lenf - lenr
data HoodMelvilleQueue a = HM Int [a] (RotationState a) [a] deriving Show

exec (Reversing ok (x:f) f' (y:r) r') = Reversing (ok+1) f (x:f') r (y:r')
exec (Reversing ok [] f' [y] r')      = Appending ok f' (y:r')
exec (Appending 0 f' r')              = Done r'
exec (Appending ok (x:f') r')         = Appending (ok-1) f' (x:r')
exec state                            = state

invalidate (Reversing ok f f' r r') = Reversing (ok-1) f f' r r'
invalidate (Appending 0 f' (x:r'))  = Done r'
invalidate (Appending ok f' r')     = Appending (ok-1) f' r'
invalidate state                    = state


execHM1 diff f state r = getNewQueue diff f state r $ exec state
exec2 diff f state r = getNewQueue diff f state r $ exec (exec state)
getNewQueue diff f state r newstate =
  case newstate of
    Done newf -> HM diff newf Idle r
    ns        -> HM diff f ns r

check diff f state r =
  if diff >= 0 then execHM1 diff f state r
  else let newstate = Reversing 0 f [] r []
       in exec2 (size diff r) f newstate []

size diff r = 2 * (length r) + diff

empty = HM 0 [] Idle []
isEmpty (HM diff f state r) = (size diff r == 0)
snoc (HM diff f state r) x = check (diff-1) f state (x:r)
head (HM _ [] _ _)     = error "empty queue"
head (HM _ (x:f') _ _) = x
tail (HM _ [] state r)        = error "empty queue"
tail (HM diff (x:f') state r) = check (diff-1) f' (invalidate state) r


main :: IO ()
main = do
  let befRot = foldl (\acc v -> snoc acc v) empty [1..6]
  print befRot

  let aftRot = snoc befRot 7
  print aftRot

  t1 <- test aftRot
  t2 <- test t1
  t3 <- test t2

  print t3


  where
    test t = do
      let r = tail t
      print r
      return r


{-|
HM 0 [1,2,3] Idle [6,5,4]
HM 7 [1,2,3] (Reversing 2 [3] [2,1] [5,4] [6,7]) []
HM 6 [2,3] (Reversing 2 [] [3,2,1] [4] [5,6,7]) []
HM 5 [3] (Appending 1 [3,2,1] [4,5,6,7]) []
HM 4 [4,5,6,7] Idle []
HM 4 [4,5,6,7] Idle []
-}
