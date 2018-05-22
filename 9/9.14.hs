-- SkewBinaryRandomAccessListへ変更するだけ
-- 単純な書き換えが相当大変だった（[x:t]でのパターンマッチ -> head/tailにする、SBLを付けるなど）

import Prelude hiding (head, lookup, tail)
import qualified SkewBinaryRandomAccessList as SBL

data RotationState a
  = Idle
  | Reversing Int
              (SBL.SkewList a)
              (SBL.SkewList a)
              (SBL.SkewList a)
              (SBL.SkewList a)
  | Appending Int
              (SBL.SkewList a)
              (SBL.SkewList a)
  | Done (SBL.SkewList a)
  deriving (Show, Eq)

data HoodMelvilleQueue a =
  HM Int
     (SBL.SkewList a)
     (RotationState a)
     Int
     (SBL.SkewList a)
  deriving (Show, Eq)

exec :: RotationState a -> RotationState a
exec (Reversing ok (SBL.SL []) f' rr@(SBL.SL [(1, _)]) r') =
  Appending ok f' (SBL.cons (SBL.head rr) r')
exec (Reversing ok ff f' rr r') =
  Reversing (ok + 1) f (SBL.cons x f') r (SBL.cons y r')
  where
    x = SBL.head ff
    f = SBL.tail ff
    y = SBL.head rr
    r = SBL.tail rr
exec (Appending 0 f' r') = Done r'
exec (Appending ok rr r') = Appending (ok - 1) f' (SBL.cons x r')
  where
    x = SBL.head rr
    f' = SBL.tail rr
exec state = state

invalidate :: RotationState a -> RotationState a
invalidate (Reversing ok f f' r r') = Reversing (ok - 1) f f' r r'
invalidate (Appending 0 f' rr) = Done (SBL.tail rr)
invalidate (Appending ok f' r') = Appending (ok - 1) f' r'
invalidate state = state

exec2 ::
     Int
  -> SBL.SkewList a
  -> RotationState a
  -> Int
  -> SBL.SkewList a
  -> HoodMelvilleQueue a
exec2 lenf f state lenr r =
  case exec (exec state) of
    Done newf -> HM lenf newf Idle lenr r
    newstate -> HM lenf f newstate lenr r

check ::
     Int
  -> SBL.SkewList a
  -> RotationState a
  -> Int
  -> SBL.SkewList a
  -> HoodMelvilleQueue a
check lenf f state lenr r =
  if lenr <= lenf
    then exec2 lenf f state lenr r
    else let newstate = Reversing 0 f SBL.empty r SBL.empty
          in exec2 (lenf + lenr) f newstate 0 SBL.empty

empty :: HoodMelvilleQueue a
empty = HM 0 SBL.empty Idle 0 SBL.empty

isEmpty :: HoodMelvilleQueue a -> Bool
isEmpty (HM lenf f state lenr r) = lenf == 0

snoc :: HoodMelvilleQueue a -> a -> HoodMelvilleQueue a
snoc (HM lenf f state lenr r) x = check lenf f state (lenr + 1) (SBL.cons x r)

head :: HoodMelvilleQueue a -> a
head (HM _ (SBL.SL []) _ _ _) = error "empty queue"
head (HM _ x _ _ _) = SBL.head x

tail :: HoodMelvilleQueue a -> HoodMelvilleQueue a
tail (HM lenf (SBL.SL []) state lenr r) = error "empty queue"
tail (HM lenf f state lenr r) = check (lenf - 1) f' (invalidate state) lenr r
  where
    f' = SBL.tail f

lookup :: Int -> HoodMelvilleQueue a -> a
lookup i (HM lenf f _ _ r)
  | i < lenf = SBL.lookup i f
  | otherwise = SBL.lookup (i - lenf) r

update :: Int -> a -> HoodMelvilleQueue a -> HoodMelvilleQueue a
update i x (HM lenf f s lenr r)
  | i < lenf = HM lenf (SBL.update i x f) s lenr r
  | otherwise = HM lenf f s lenr (SBL.update (i - lenf) x r)

main :: IO ()
main = do
  let u = foldr (\x y -> snoc y x) empty [0 .. 10]
  print u
  print $ head u
  print $ tail u
  let u' = tail u
  print $ head u'
  print $ tail u'
  print $ tail (tail u')
  print $ tail (tail (tail u'))
  print $ lookup 3 u == 7
  print $ update 3 99 u
