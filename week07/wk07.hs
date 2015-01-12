primes :: [Int]
primes = sieve [2..]

sieve :: [Int] -> [Int]
sieve (p:xs) = p : sieve [x | x <- xs, x `mod`p /= 0]

fibs :: [Integer]
fibs = 0:1:[ x + y| (x,y) <- zip fibs(tail fibs)]

fib :: Int -> Integer
fib n = fibs !! n
