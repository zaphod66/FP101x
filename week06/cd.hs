-- Countdown problem

data Op = Add | Sub | Mul | Div deriving Show

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

data Expr = Val Int | App Op Expr Expr deriving Show

-- Return the overall value of an expression
-- provided that it is a natural number
-- Either succeeds and returns a singleton list,
-- or fails and returns the empty list
eval :: Expr -> [Int]
eval (Val n)     = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l
                                , y <- eval r
                                , valid o x y]

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choises ns)
                  && eval e == [n]

-- Brute force

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choises ns,
                      e   <- exprs ns',
                      eval e == [n]]

exprs :: [Int] -> [Expr]
exprs []  = []
exprs [n] = [Val n]
exprs ns  = [e | (ls,rs) <- split ns,
                 l <- exprs ls,
                 r <- exprs rs,
                 e <- combine l r]

split :: [a] -> [([a],[a])]
split []     = []
split [_]    = []
split (x:xs) = ([x], xs): [(x:ls,rs) | (ls,rs) <- split xs]

combine :: Expr -> Expr -> [Expr]
combine l r  =  [App o l r | o <- ops]

ops :: [Op]
ops = [Add,Sub,Mul,Div]

-- helper functions

values :: Expr -> [Int]
values (Val n)     = [n]
values (App _ l r) = values l ++ values r

subs :: [a] -> [[a]]
subs []     =  [[]]
subs (x:xs) =  yss ++ map (x:) yss
               where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x : y : ys) : map (y :)(interleave x ys)

perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

choises :: [a] -> [[a]]
choises xs = concat (map perms (subs xs))

-- Combining generation and evaluation
-- Avoid to generate invalid exprs upfront

type Result = (Expr,Int)

results :: [Int] -> [Result]
results []  = []
results [n] = [(Val n,n) | n > 0]
results ns  = [res | (ls,rs) <- split ns,
                     lx <- results ls,
                     ry <- results rs,
                     res <- combine' lx ry]

combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid o x y]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns'   <- choises ns,
                       (e,m) <- results ns',
                       m == n]

-- Exploiting numeric properties

valid' :: Op -> Int -> Int -> Bool
valid' Add x y = x <= y
valid' Sub x y = x > y
valid' Mul x y = x /= 1 && y /= 1 && x <= y
valid' Div x y = y /= 1 && x `mod` y == 0

combine'' :: Result -> Result -> [Result]
combine'' (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid' o x y]

results' :: [Int] -> [Result]
results' []  = []
results' [n] = [(Val n,n) | n > 0]
results' ns  = [res | (ls,rs) <- split ns,
                     lx <- results' ls,
                     ry <- results' rs,
                     res <- combine'' lx ry]

solutions'' :: [Int] -> Int -> [Expr]
solutions'' ns n = [e | ns'   <- choises ns,
                        (e,m) <- results' ns',
                        m == n]
