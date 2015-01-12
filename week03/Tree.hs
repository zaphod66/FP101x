{-# LANGUAGE PackageImports #-}

import "mtl" Control.Monad.State.Lazy

data Tree a = Leaf a | Branch (Tree a) (Tree a)

number :: Int -> Tree a -> (Tree (a,Int),Int)
number seed (Leaf a) = (Leaf (a,seed), seed + 1)
number seed (Branch left right) =
	let (l, ls) = number seed left
	    (r, rs) = number ls right
    in
        (Branch l r, rs)

numbers :: Tree a -> State Int (Tree (a,Int))
numbers (Leaf a) = do n <- get
                      modify (+1)
                      return (Leaf (a,n))
numbers (Branch left right) = do l <- numbers left
                                 r <- numbers right
                                 return (Branch l r)

initState :: State s s
initState = State (\s -> (s,s))
