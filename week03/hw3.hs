import Data.Char


repl n a = [a | _ <- [1 .. n]]

factors :: Int -> [Int]
factors n = [x | x <- [1 .. n], n `mod` x == 0]

pyths n = [(x,y,z) | x <- [1..n],
                     y <- [1..n],
                     z <- [1..n],
                     x ^ 2 + y ^ 2 == z ^2]

perfects n = [x | x <- [1 .. n], isPerfect x]
  where isPerfect num = sum (init (factors num)) == num

find :: (Eq a) => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']

positions x xs = find x (zip xs [0 .. n])
  where n = length xs - 1

let2int :: Char -> Int
let2int c = ord c - ord 'a'

ulet2int :: Char -> Int
ulet2int c = ord c - ord 'A'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)
uint2let :: Int -> Char
uint2let n = chr (ord 'A' + n)

shift :: Int -> Char -> Char
shift n c
  | isLower c = int2let ((let2int c + n) `mod` 26)
  | isUpper c = uint2let ((ulet2int c + n) `mod` 26)
  | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

xs = 1 : [x + 1 | x <- xs]
