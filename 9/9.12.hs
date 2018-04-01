data Digits = Zero | Ones Int | Two | Threes Int | Four deriving (Show, Eq)

ones :: Int -> [Digits] -> [Digits]
ones 0 ds = ds
ones i (Ones j : ds) = Ones (i+j) : ds
ones i ds = Ones i : ds

threes :: Int -> [Digits] -> [Digits]
threes 0 ds = ds
threes i (Threes j : ds) = Threes (i+j) : ds
threes i ds = Threes i : ds


simpleInc :: [Digits] -> [Digits]
simpleInc [] = [Ones 1]
simpleInc (Zero : ds) = ones 1 ds
simpleInc (Ones i : ds) = Two : ones (i-1) ds
simpleInc (Two : ds) = threes 1 ds
simpleInc (Threes i : ds) = Four : threes (i-1) ds

fixup :: [Digits] -> [Digits]
fixup (Four : ds) = Two : simpleInc ds
fixup (Ones i : Four : ds) = Ones i : Two : simpleInc ds
fixup (Threes i : Four : ds) = Threes i : Two : simpleInc ds
fixup (Zero : ds) = Two : simpleDec ds
fixup (Ones i : Zero : ds) = Ones i : Two : simpleDec ds
fixup (Threes i : Zero : ds) = Threes i : Two : simpleDec ds
fixup ds = ds

inc :: [Digits] -> [Digits]
inc ds = fixup (simpleInc ds)

simpleDec :: [Digits] -> [Digits]
simpleDec [Ones 1] = []
simpleDec (Ones i : ds) = Zero : ones (i-1) ds
simpleDec (Two : ds) = ones 1 ds
simpleDec (Threes i : ds) = Two : threes (i-1) ds
simpleDec (Four : ds) = threes 1 ds

dec :: [Digits] -> [Digits]
dec ds = fixup (simpleDec ds)


main = do
  test 30
  print "----------------------"
  test2 30

test 0 = do
  return ()
test n = do
  let u = foldl (\x y -> inc x) [] [1..n]
  test (n-1)
  print u
  return ()

test2 0 = do
  return ()
test2 n = do
  let u = foldl (\x y -> dec x) [Two,Four,Threes 1,Ones 1] [1..n]
  test2 (n-1)
  print u
  return ()