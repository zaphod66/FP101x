import Data.List
import Data.Char
import Unsafe.Coerce

-- wk06

-- Natural numbers

data Nat = Zero | Succ Nat deriving Show
-- Zero :: Nat
-- Succ :: Nat -> Net

nat2int         :: Nat -> Int
nat2int Zero     = 0
nat2int (Succ n) = 1 + nat2int n

nat2int2         :: Nat -> Int
nat2int2 = \ n -> length [c | c <- show n, c == 'S']

nat2int3         :: Nat -> Int
nat2int3 = \ n -> genericLength [c | c <- show n, c == 'S']

int2nat  :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n - 1))

add  :: Nat -> Nat -> Nat
add Zero n     = n
add (Succ m) n = Succ(add m n)

mul :: Nat -> Nat -> Nat
mul m Zero     = Zero
mul m (Succ n) = add m (mul m n)

-- Expressions

data Expr = Val Int
          | Add Expr Expr
          | Mul Expr Expr
          deriving Show

-- Val :: Int -> Expr
-- Add :: Expr -> Expr -> Int
-- Mul :: Expr -> Expr -> Int

-- Add (Val 1) (Mul (Val 2) (Val 3))

size          :: Expr -> Int
size (Val n)   = 1
size (Add x y) = size x + size y
size (Mul x y) = size x + size y

eval          :: Expr -> Int
eval (Val n)   = n
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

-- eval' = fold id (+) (*)

-- Trees

data Tree = Leaf Int
          | Node Tree Int Tree
          deriving Show

-- Node (Node (Leaf 1) 3 (Leaf 4))
--      5
--      (Node (Leaf 6) 7 (Leaf 9))

occurs :: Int -> Tree -> Bool

occurs m (Leaf n)     = m == n
occurs m (Node l n r) = m == n
                     || occurs m l
                     || occurs m r

-- for sorted trees
occurs' m (Leaf n)     = m == n
occurs' m (Node l n r) | m == n = True
                       | m < n  = occurs' m l
                       | m > n  = occurs' m r

flatten  :: Tree -> [Int]
flatten (Leaf n)     = [n]
flatten (Node l n r) = flatten l
                    ++ [n]
                    ++ flatten r

data BTree = BLeaf Int
           | BNode BTree BTree deriving Show

balanced :: BTree -> Bool
leaves (BLeaf _) = 1
leaves (BNode l r) = leaves l + leaves r
balanced (BLeaf _) = True
balanced (BNode l r) = abs (leaves l - leaves r) <= 1 && balanced l && balanced r

balance :: [Int] -> BTree
halve xs = splitAt (length xs `div` 2) xs
balance [x] = BLeaf x
balance xs  = BNode (balance ys) (balance zs)
              where (ys,zs) = halve xs
