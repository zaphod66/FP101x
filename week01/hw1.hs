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
