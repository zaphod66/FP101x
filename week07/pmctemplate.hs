module Lab5 where

import Control.Monad
import System.Random

data Concurrent a = Concurrent ((a -> Action) -> Action)

data Action 
    = Atom (IO Action)
    | Fork Action Action
    | Stop

instance Show Action where
    show (Atom x) = "atom"
    show (Fork x y) = "fork " ++ show x ++ " " ++ show y
    show Stop = "stop"

-- ===================================
-- Ex. 0
-- ===================================

fromConcurrent :: Concurrent a -> ((a -> Action) -> Action)
fromConcurrent (Concurrent a) = a

action :: Concurrent a -> Action
action (Concurrent a) = a (const Stop)

-- ===================================
-- Ex. 1
-- ===================================

stop :: Concurrent a
-- stop = Concurrent (\_ -> Stop)
stop = Concurrent(const Stop)

-- ===================================
-- Ex. 2
-- ===================================

atom :: IO a -> Concurrent a
atom x = Concurrent(\c -> (Atom (x >>= (\ a -> return (c a)))))

-- ===================================
-- Ex. 3
-- ===================================

fork :: Concurrent a -> Concurrent ()
fork a = Concurrent $ \c -> Fork (action a) (c ())

par :: Concurrent a -> Concurrent a -> Concurrent a
par (Concurrent a1) (Concurrent a2) = Concurrent $ \c -> Fork (a1 c) (a2 c)


-- ===================================
-- Ex. 4
-- ===================================

instance Monad Concurrent where
    (Concurrent f) >>= g = Concurrent(\c ->
                             f (\x -> fromConcurrent (g x) c))
--     m >>= g  = Concurrent (\c ->
--                  fromConcurrent m (\x -> fromConcurrent (g x) c))
    return x = Concurrent (\c -> c x)


-- ===================================
-- Ex. 5
-- ===================================

roundRobin :: [Action] -> IO ()
roundRobin [] = return ()
roundRobin (a:as) =
    do c <- exec a
       roundRobin(as ++ c)
    where exec :: Action -> IO [Action]
          exec Stop       = return []
          exec (Fork l r) = return [l,r]
          exec (Atom io)  = do c <- io
                               return [c] 
                               

-- ===================================
-- Tests
-- ===================================

ex0 :: Concurrent ()
ex0 = par (loop (genRandom 1337)) (loop (genRandom 2600) >> atom (putStrLn ""))

ex1 :: Concurrent ()
ex1 = do atom (putStr "Haskell")
         fork (loop $ genRandom 7331) 
         loop $ genRandom 42
         atom (putStrLn "")


-- ===================================
-- Helper Functions
-- ===================================

run :: Concurrent a -> IO ()
run x = roundRobin [action x]

-- genRandom :: Int -> [Int]
-- genRandom s = take 10 . randomRs (0, 99) . mkStdGen $ s

genRandom :: Int -> [Int]
genRandom 1337 = [1, 96, 36, 11, 42, 47, 9, 1, 62, 73]
genRandom 7331 = [17, 73, 92, 36, 22, 72, 19, 35, 6, 74]
genRandom 2600 = [83, 98, 35, 84, 44, 61, 54, 35, 83, 9]
genRandom 42   = [71, 71, 17, 14, 16, 91, 18, 71, 58, 75]

loop :: [Int] -> Concurrent ()
loop xs = mapM_ (atom . putStr . show) xs

