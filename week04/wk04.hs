p x = x `mod` 2 == 0
f x = x + 3

type Bit = Int

bin2int :: [Bit] -> Int
bin2int bits = sum [w*b | (w,b) <- zip weights (reverse bits)]
               where weights = iterate (*2) 1

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = int2bin (n `div` 2) ++ [n `mod` 2]
