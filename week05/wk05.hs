import System.IO

a :: IO (Char, Char)
a = do x <- getChar
       getChar
       y <- getChar
       return (x,y)

getLn :: IO String
getLn = do x <- getChar
           if (x == '\n') then
             return []
           else
             do xs <- getLn
             	return (x:xs)

putString        :: String -> IO ()
putString []     = return ()
putString (x:xs) = do putChar x
                      putString xs


putStringLn :: String -> IO ()
putStringLn xs = do putString xs
                    putChar '\n'

stringLen :: IO ()
stringLen = do putString "Enter a string: "
               xs <- getLn
               putString "The String has "
               putString (show (length xs))
               putStringLn " characters"

hangman :: IO ()
hangman = do putString "Think of a word: "
             word <- sgetLn
             putStringLn "Try to guess it:"
             guess word

sgetLn :: IO String
sgetLn = do x <- getCh
            if x == '\n' then
            	do putChar x
            	   return []
            else
            	do putChar '-'
            	   xs <- sgetLn
            	   return (x:xs)

getCh :: IO Char
getCh  = do hSetEcho stdin False
            c <- getChar
            hSetEcho stdin True
            return c

guess :: String -> IO ()
guess word =
	do putString "> "
	   xs <- getLn
	   if xs == word then
	   	  putStringLn "You got it!"
	   else
	   	  do putStringLn (diff word xs)
	   	     guess word

diff :: String -> String -> String
diff xs ys =
	[if elem x ys then x else '-' | x <- xs]


-- exercises

putStrLn' :: String -> IO ()

putStrLn' [] = putChar '\n'
putStrLn' xs = putString xs >> putChar '\n'

getLine' = get []

get :: String -> IO String
get xs
  = do x <- getChar
       case x of
       	'\n' -> return xs
       	_ -> get (xs ++ [x])

interact' f
  = do inp <- getLn
       putStringLn (f inp)

seq' [] = return []
seq' (m:ms)
  = m >>=
  	\ a ->
  	  do as <- seq' ms
  	     return (a:as)

seq2 [] = return []
seq2 ms = foldr func (return []) ms
  where
  	func :: (Monad m) => m a -> m [a] -> m[a]
  	func m acc
  	  = do x <- m
  	       xs <- acc
  	       return (x:xs)

toOpt x = Just x

mapM' f as = seq' (map f as)

mapM2 f [] = return []
mapM2 f (a:as)
  = f a >>= \ b -> mapM2 f as >>= \ bs -> return (b : bs)

mapM3 f [] = return []
mapM3 f (a:as)
  = do
      b <-f a
      bs <- mapM3 f as
      return (b : bs)

mapM4 f [] = return []
mapM4 f (a:as)
  = f a >>=
    \ b ->
      do bs <- mapM4 f as
         return (b : bs)

mapM5 f [] = return []
mapM5 f (a:as)
  = f a >>=
    \ b ->
      do bs <- mapM5 f as
         return (bs ++ [b])
         