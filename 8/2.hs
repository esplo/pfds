{-|
先頭側リストが枯渇するのは、tailがm回行われた時。

この時、invalidateがm回呼ばれており、併せてexecもm回呼ばれている。
これにより、rが反転されたr'は存在し、かつ残りはr'だけの状態になっている。

この時、f'とr'の結合操作は不要なため、既に回転の準備は出来ている。

（さらにtailが起きても、r'から順番に取れば良い）
-}

import           Prelude hiding (head, tail)

data RotationState a =
  Idle
  | Reversing Int [a] [a] [a] [a]
  | Appending Int [a] [a]
  | Done [a]
  deriving Show

data HoodMelvilleQueue a = HM Int [a] (RotationState a) Int [a] deriving Show

exec (Reversing ok (x:f) f' (y:r) r') = Reversing (ok+1) f (x:f') r (y:r')
exec (Reversing ok [] f' [y] r')      = Appending ok f' (y:r')
exec (Appending 0 f' r')              = Done r'
exec (Appending ok (x:f') r')         = Appending (ok-1) f' (x:r')
exec state                            = state

invalidate (Reversing ok f f' r r') = Reversing (ok-1) f f' r r'
invalidate (Appending 0 f' (x:r'))  = Done r'
invalidate (Appending ok f' r')     = Appending (ok-1) f' r'
invalidate state                    = state


execHM1 lenf f state lenr r = getNewQueue lenf f state lenr r $ exec state
exec2 lenf f state lenr r = getNewQueue lenf f state lenr r $ exec (exec state)
getNewQueue lenf f state lenr r newstate =
  case newstate of
    Done newf -> HM lenf newf Idle lenr r
    ns        -> HM lenf f ns lenr r

-- 8.2
check lenf f state lenr r =
  if lenr <= lenf then execHM1 lenf f state lenr r
  else let newstate = Reversing 0 f [] r []
       in exec2 (lenf + lenr) f newstate 0 []

empty = HM 0 [] Idle 0 []
isEmpty (HM lenf f state lenr r) = (lenf == 0)
snoc (HM lenf f state lenr r) x = check lenf f state (lenr+1) (x:r)
head (HM _ [] _ _ _)     = error "empty queue"
head (HM _ (x:f') _ _ _) = x
tail (HM lenf [] state lenr r) = error "empty queue"
tail (HM lenf (x:f') state lenr r) = check (lenf-1) f' (invalidate state) lenr r


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
HM 3 [1,2,3] Idle 3 [6,5,4]
HM 7 [1,2,3] (Reversing 2 [3] [2,1] [5,4] [6,7]) 0 []
HM 6 [2,3] (Reversing 2 [] [3,2,1] [4] [5,6,7]) 0 []
HM 5 [3] (Appending 1 [3,2,1] [4,5,6,7]) 0 []
HM 4 [4,5,6,7] Idle 0 []
HM 4 [4,5,6,7] Idle 0 []
-}
