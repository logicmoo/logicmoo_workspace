{-
--* Introduces information that cannot be included
in syntactic analysis, but can help the user.
-}

-- Infix operators declarations

infixl 9 ., !!
infixl 7 *, /, `div`, `mod`
infixl 6 +, -
infixr 5 ++
infixc 4 ==, <, >, =<, >=
infixl 3 &&
infixl 2 ||
infixr 1 >>, >>=, =:=
infixr 0 &, &>


-- Constraints

-- Equational constraint
(=:=) :: a -> a -> Constraint

-- Always solvable constraint
success :: Constraint

-- Concurrent conjunction of constraints
(&) :: Constraint -> Constraint -> Constraint

-- Sequential conjunction  of constraints
(&>) :: Constraint -> Constraint -> Constraint
c1 &> c2 | c1 = c2


-- Some standard combinators:

-- Function composition
(.)             :: (b -> c) -> (a -> b) -> (a -> c)
f . g           = \x -> f (g x)

-- Identity
id              :: a -> a
id x            = x

curry           :: ((a,b) -> c) -> a -> b -> c
curry f a b     = f (a,b)

uncurry         :: (a -> b -> c) -> (a,b) -> c
uncurry f (a,b) = f a b

flip            :: (a -> b -> c) -> b -> a -> c
flip f x y      = f y x


-- Boolean values

data Bool  = True | False

-- Sequential conjunction
(&&)       :: Bool -> Bool -> Bool
True  && x = x
False && _ = False

-- Sequential disjunction
(||)       :: Bool -> Bool -> Bool
True  || _ = True
False || x = x

-- Negation
not        :: Bool -> Bool
not True   = False
not False  = True

-- Otherwise function
otherwise  :: Bool
otherwise  = True


-- Pairs

--* data (a,b) = (a,b)

fst        :: (a,b) -> a
fst (x,_)  = x

snd        :: (a,b) -> b
snd (_,y)  = y


-- Unit type

--* data () = ()


-- Lists

--* data [a]          = [] | a : [a]

head              :: [a] -> a
head (x:_)        = x

tail              :: [a] -> [a]
tail (_:xs)       = xs

-- Concatenation
(++)              :: [a] -> [a] -> [a]
[]     ++ ys      = ys
(x:xs) ++ ys      = x : xs++ys

-- List length
length            :: [a] -> Int
length []         = 0
length (_:xs)     = 1 + length xs

-- List index (subscript) operator, head has index 0
(!!)              :: [a] -> Int -> a
(x:xs) !! n       = if n==0 then x else xs !! (n-1)

-- Map a function on a list
map               :: (a->b) -> [a] -> [b]
map _ []          = []
map f (x:xs)      = f x : map f xs

-- Accumulate all list elements
foldr             :: (a->b->b) -> b -> [a] -> b
foldr _ z []      = z
foldr f z (x:xs)  = f x (foldr f z xs)

-- Filter elements in a list
filter            :: (a -> Bool) -> [a] -> [a]
filter _ []       = []
filter p (x:xs)   = if p x then x : filter p xs
                           else filter p xs

-- Join two lists to one list of pairs
zip               :: [a] -> [b] -> [(a,b)]
zip []     []     = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys

-- Concatenate a list of lists into one list
concat            :: [[a]] -> [a]
concat l          = foldr (++) [] l

-- Return prefix of length n
take              :: Int -> [a] -> [a]
take _ []     = []
take n (x:xs) = if n==0 then [] else x : take (n-1) xs

-- Return suffix without first n elements
drop              :: Int -> [a] -> [a]
drop _ []     = []
drop n (x:xs) = if n==0 then (x:xs) else drop (n-1) xs

-- Return longest prefix with elements satisfying a predicate
takeWhile          :: (a -> Bool) -> [a] -> [a]
takeWhile _ []     = []
takeWhile p (x:xs) = if p x then x : takeWhile p xs else []

-- Return suffix without takeWhile prefix
dropWhile          :: (a -> Bool) -> [a] -> [a]
dropWhile _ []     = []
dropWhile p (x:xs) = if p x then dropWhile p xs else x:xs


-- Conversion functions between characters and their ASCII values
ord :: Char -> Int
chr :: Int -> Char


-- Convert a term into a printable representation

show :: a -> String


-- Types of primitive functions and predicates

(+)  :: Int -> Int -> Int          
(-)  :: Int -> Int -> Int          
(*)  :: Int -> Int -> Int          
div  :: Int -> Int -> Int
mod  :: Int -> Int -> Int
(<)  :: Int -> Int -> Bool      
(>)  :: Int -> Int -> Bool      
(<=) :: Int -> Int -> Bool      
(=>) :: Int -> Int -> Bool      


-- Monadic IO

--* data IO a -- conceptually: World -> (a,World)

(>>)          :: IO a -> IO b        -> IO b
(>>=)         :: IO a -> (a -> IO b) -> IO b
putChar       :: Char -> IO ()
getChar       :: IO Char
done          :: IO ()
return        :: a -> IO a
readFile      :: String -> IO String
writeFile     :: String -> String -> IO ()

putStr        :: String -> IO ()
putStr []     = done
putStr (c:cs) = putChar c >> putStr cs

putStrLn      :: String -> IO ()
putStrLn cs   = putStr cs >> putChar '\n'

getLine       :: IO String
getLine       = getChar >>= \c -> 
                if c == '\n' then return []
                             else getLine >>= \cs -> return (c:cs)
