n = a `div` length xs
  where
    a = 10
    xs = [1, 2, 3, 4, 5]

lst1 xs = head (drop (length xs - 1) xs)

lst2 xs = head (reverse xs)

lst3 xs = xs !! (length xs - 1)

ini1 xs = reverse (tail (reverse xs))

ini2 xs = take (length xs - 1) xs

summ [] = 0
summ (x : xs) = x + summ xs

prod [] = 1
prod (x : xs) = x * prod xs

qsort1 [] = []
qsort1 (x : xs) = qsort1 larger ++ [x] ++ qsort1 smaller
  where smaller = [a | a <- xs, a <= x]
        larger  = [b | b <- xs, b > x]

qsort7 [] = []
qsort7 (x : xs)
  = reverse
      (reverse (qsort7 smaller) ++ [x] ++ reverse (qsort7 larger))
  where smaller = [a | a <- xs, a <= x]
        larger  = [b | b <- xs, b > x]

factorial n = product [1..n]

average ns = sum ns `div` length ns

second xs = head (tail xs)

swap (x,y) = (y,x)

pair x y = (x,y)

double x = x * 2

palindrome xs = reverse xs == xs

twice f x = f (f x)

halves2 xs = splitAt (length xs `div` 2) xs

safetail3 (_ : xs)
  | null xs = []
  | otherwise = tail xs

safetail5 xs
  | length xs == 0 = []
  | otherwise = tail xs

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

mySplit :: Int -> [a] -> ([a],[a])
mySplit n xs = (take n xs,drop n xs)

type InfoP a = String -> Int -> Double -> Char -> a

liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP q f k w x y z = f w x y z `q` k

tst :: [[t]] -> [t]
tst xs = head xs

e111 :: [[a]]
e111 = [[1,2],[3,4]]
