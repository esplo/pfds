-- from SML

{-# LANGUAGE TypeFamilies #-}

module RandomAccessList where

type family Elem l

class RandomAccessList l where
  empty :: l
  isEmpty :: l -> Bool
  cons :: Elem l -> l -> l
  head :: l -> Elem l -- raise Empty
  tail :: l -> l -- raise Empty
  lookup :: Int -> l -> Elem l -- raise Subscript
  update :: Int -> Elem l -> l -> l -- raise Subscript

-- http://maoe.hatenadiary.jp/entry/20091021/1256101883
-- 今回の場合はMultiParamTypeClassesでもいけると思う。