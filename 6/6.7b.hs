data MergeSort a = MS Int [[a]] deriving Show

mrg [] ys = ys
mrg xs [] = xs
mrg xs@(x:xs') ys@(y:ys') =
  if x <= y then x : mrg xs' ys else y : mrg xs ys'

empty = MS 0 []

add x (MS size segs) = MS (size+1) (addSeg [x] segs size)
  where addSeg seg segs size =
          if size `mod` 2 == 0 then seg : segs
          else addSeg (mrg seg (head segs)) (tail segs) (size `div` 2)

sort (MS size segs) = foldl mrg [] segs

pickK :: Ord a => Int -> MergeSort a -> [a]
pickK _ (MS 0 _) = []
pickK k (MS size segs)
  | k > size = error "too large"
  | otherwise = heads
    where
      pickHead :: [a] -> [a] -> [a]
      pickHead r [] = r
      pickHead r (x:_) = x:r
      -- log(n) のリスト
      heads = foldl pickHead [] segs
      pick 0 _ = []
      pick _ [] = []
      pick m (x:xs) = minimum 


main :: IO ()
main = do
  let u = (add 5 . add 1 . add 8 . add 4 . add 3 . add 2 . add 10) empty
  print u
  print $ pickK 3 u
