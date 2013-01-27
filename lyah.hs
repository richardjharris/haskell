import Control.Applicative
import Data.Char
import Data.Function
import Data.List
import qualified Data.Foldable as F
import qualified Data.Map as Map
import qualified Data.Monoid as MN
import qualified Data.Set as Set
import System.IO
import System.Random

{- Ready, Set, Go -}
notEqual x y = x /= y
getIndex xs n = xs !! n {- starts at 0 -}
listEmpty = null
{- cycle, repeat, replicate num element -}
comp1 = [x*2 | x <- [1..10], odd x]
comp2 = [x*y | x <- [2,5,10], y <- [8,10,11], x*y > 50]
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z'] ]
rightTriangles' = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]  

{- Types and typeclasses -}

getCompare a b = a `compare` b
show1 = show 5.334
read1 = read "6" :: Int
{- Eq: =, /= -}
{- Ord: >, <, >=, <=; implies Eq -}
{- Show: can be printed via show -}
{- Read: can be read -}
{- Enum: succ/pred functions, can be used in ranges -}
{- Bounded: upper and lower bound via minBound/maxBound -}
{- Num: type used by *, + etc. implies Eq, Show -}
{- Integral: Int/Integer -}
{- Floating: Float/Double -}
blah1 = fromIntegral (length [1,2,3,4]) + 3.2

{- Syntax in functions -}
calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]  

cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea  

head' :: [a] -> a  
head' [] = error "No head for empty lists!"  
head' (x:_) = x  

head'' :: [a] -> a  
head'' xs = case xs of [] -> error "No head for empty lists!"  
                       (x:_) -> x  

{- Inline function def -}
describeList :: [a] -> String  
describeList xs = "The list is " ++ what xs  
    where what [] = "empty."  
          what [x] = "a singleton list."  
          what xs = "a longer list."  

{- Recursion -}

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "No maximum for empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > max = x
    | otherwise = max
    where max = maximum' xs

replicate' :: (Eq b, Num b) => a -> b -> [a]
replicate' _ 0 = []
replicate' a x = a:(replicate' a (x - 1))

take' :: (Num i, Ord i) => i -> [x] -> [x]
take' i _
  | i <= 0 = []
take' _ [] = []
take' i (x:xs) = x : take' (i - 1) xs

repeat' :: a -> [a]
repeat' a = a : repeat' a

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' item [] = False
elem' item (x:xs)
 | x == item = True
 | otherwise = item `elem'` xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    quicksort left ++ [x] ++ quicksort right
    where left = filter (<= x) xs
          right = filter (> x) xs

{- replicate implemented in terms of foldl -}
replicate'' :: (Num b, Enum b) => a -> b -> [a]
replicate'' elem n = foldl (\acc _ -> elem:acc) [] [1..n]

{- Higher order functions -}

{- max 4 5 == (max 4) 5 -}
compareWithHundred = compare 100  
divideByTen = (/10)
isUpperAlphanum = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

regularZip = zipWith' (,)  {- tuple ctor -}

{- Use zipWith' (*) as an argument to itself. -}
{- For each 3-list, multiply pairwise -}
f01 = zipWith' (zipWith' (*)) [[1,2,3],[3,5,6],[2,3,4]] [[3,2,2],[3,4,5],[5,4,3]]
{- Recursive map on sublists -}
f02 = map (map (^2)) [[1,2],[3,4,5,6],[7,8]]

{- Definition of flip. flip' f returns a part-curried function to flip the
   other two arguments. -}
flip' f y x = f x y
{- Defined as set of currying lambdas -}
flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f = \y x -> f x y

{- Also identical: -}
addThree x y z = x + y + z
addThree' = \x -> \y -> \z -> x + y + z  

sum' = foldl (+) 0
{- expanded: sum' xs = foldl (\acc x -> acc + x) 0 xs -}
{- foldl1 assumes first element of list is initial accumulator -}
sum'' = foldl1 (+)

{- e.g. -}
ff1 = foldl (\acc x -> x:acc) "" ['A'..'Z']  {-> "ZYXWVUTSRQPONMLKJIHGFEDCBA" -}
ff2 = foldr (\x acc -> x:acc) "" ['A'..'Z']  {-> "ABCDEFGHIJKLMNOPQRSTUVWXYZ" -}
  {-= foldr (:) "" ['A'..'Z']  or  foldl (flip (:)) "" ['A'..'Z']) -}
  {- If we wanted A..Z then foldl is less efficient:
   - foldl (\a b -> a ++ [b]) "" ['A'..'Z']
   - Also, foldl1 cannot be used: its function type is (a -> a -> a), e.g. (+) -}
withProgressToo = scanl (+) 0 [3,5,2,1]

{- Largest number under 100k divisible by 3829 -}
lrg0 = head $ filter (\x -> x `mod` 3829 == 0) [100000,99999..1]
lrg1 = head . filter ((== 0) . (`mod` 3829)) $ [100000,99999..1]

{- Odd squares smaller than 10k
 - This will loop forever as filter/listcomp doesn't know the list is ascending
 - [sqr | x <- [1..], let sqr = x*x, odd sqr, sqr < 10000]
 - This works:
 - takeWhile (< 10000) [sqr | x <- [1..], let sqr = x*x, odd sqr]
 - Also written as:
 - sum . takeWhile (< 10000) . filter odd . map (^2) $ [1..] -}

chain :: (Integral a) => a -> [a]
chain x
 | x <= 0    = error "Invalid chain"
chain 1 = [1]
chain n = n : chain m
  where m = if even n then n `div` 2 else n * 3 + 1

bigChains = length . filter ((> 15) . length) . map chain $ [1..100]

{- Pattern matching in lambdas -}
frib00 = map (\(a,b) -> a + b) [(1,2), (3,5), (6,3), (2,6), (2,5)]

{- Point free:
 - the guide recommends
 -
 -  sum . takeWhile (< 10000) . filter odd . map (^2) $ [1..]
 -  This composes all these functions together, as they all take
 -  1 argument. Then applies it to [1..]
 -
 - I initially wrote
 -
 - sum $ takeWhile (< 10000) $ filter odd $ map (^2) [1..]
 - ^ this won't work if you want to apply more arguments as we resolve
 - 'map (^2) [1..]' first, then pass as argument to left.
 -}

{- Modules -}

{- import Foo [hiding (blah) | (foo,bar)]; import qualified Foo [as M] -}

search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack = needle `elem` (concatMap inits . tails $ haystack)

search' needle haystack =
    any ((== needle) . take nlen) . tails $ haystack
    where nlen = length needle

search'' x y = x `isInfixOf` y

{- grouping -}
{- group [1,1,1,2,2,2,3] = [[1,1,1],[2,2,2],[3]]
 - cf. partition: partition (< 0) [1,-1,2,-2] = ([-1,-2], [1,2])
 -
 - groupBy (\x y -> (x > 0) == (y > 0)) values
 - Looks at two adjacent items to determine if it can split a group.
 - Decided to implement my own: -}
groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' _ [] = []
groupBy' _ [a] = [[a]]
groupBy' f (x:y:rest)
 | grouping  = (x:(head groupRest)):(tail groupRest)
 | otherwise = [x]:groupRest
 where groupRest = groupBy' f (y:rest)
       grouping = f x y

{- Prelude version is considerably nicer. Logic is a little different:
 - all elements in group are compared to first element of group, rather
 - than prev. element. Only works for transitive operations. -}
groupBy'' _ [] = []
groupBy'' eq (x:xs) = (x:ys) : groupBy eq zs
                      where (ys,zs) = span (eq x) xs
                      
{- on function is used in *By functions, e.g.
   groupBy ((==) `on` (> 0)) values
   this is the same as
   groupBy (\x y -> (x > 0) == (y > 0)) values -}
smallestFirst0 = sortBy (compare `on` length) ["Foo", "b", "cz", "Habala!",""]
stringValid0 = all isAlphaNum "hello"
spaceCat0 = generalCategory ' '

caesarCipher :: Int -> String -> String
caesarCipher shift = map (chr . (+ shift) . ord)
caesarDecode shift = caesarCipher (negate shift)

{- Data.Map -}
employees = [("robin", 16), ("fips", 31), ("jez", 58), ("rjh", 26)]
findKey :: (Eq k) => k -> [(k,v)] -> v
findKey key = snd . head . filter ((== key) . fst)
{- head of empty list if key isn't there! Using Maybe... -}
findKey' :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey' key = foldr (\(k,v) acc -> if k == key then Just v else acc) Nothing
{- or (fmap propagates the Just) -}
findKey'' key = fmap snd . find ((== key) . fst)

{- The Map module (empty, insert k v map, null, size, lookup, member
 - map, filter, toList, fromList, keys, elems) is better! -}
listToMap :: Ord k => [(k,v)] -> Map.Map k v
listToMap [] = Map.empty
listToMap ((k,v):xs) = Map.insert k v (listToMap xs)
{- or use a fold to make it more obvious -}
listToMap' :: Ord k => [(k,v)] -> Map.Map k v
listToMap' = foldr (\(k,v) acc -> Map.insert k v acc) Map.empty
{- as a right fold, if we have dupe keys, we use the leftmost -}
{- map, filter work on values -}

unixGroups =
    [("rjh", "wheel"), ("rjh", "toolbar")
    ,("robin", "toolbar"), ("jli", "wheel")
    ,("jli", "toolbar"),("(jli", "sysadmin")
    ]

unixGroupsToMap :: (Ord k) => [(k,String)] -> Map.Map k String
unixGroupsToMap = Map.fromListWith (\g1 g2 -> g1 ++ ", " ++ g2)
{- other options: max, ++ (on lists), +; see also insertWith -}

{- Data.Set filters duplicates faster than the O(n^2) nub, but does
 - not preserve ordering -}
setNub xs = Set.toList $ Set.fromList xs

{- module Foo (func, func...) where [definitions]
 - submodules (e.g. Geometry.Sphere) go in Geometry/Sphere.hs 
 - modules can import others
 - exporting a type with ctors:  Shape(..) or Shape(Rectangle, Circle)
 - writing Shape on its own means it can only be constructed indirectly,
 - using another exported function. This is more common (e.g. Map.fromList)
 - as it hides the implementation. -}

{- Types and type-classes -}
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)
{- Notice 'Circle' is not a type, Shape is (same as Bool is, but True isn't) -}

{- Record syntax -}
data Person = Person { firstName :: String, lastName :: String, age :: Int }
    deriving (Show)
{- firstName (Person "r" "h" 26) -> "r" -}
{- stang = Car {company="Ford", model="Mang", year=1967} -}
{- Don't add typeclass constraints in Data declarations, e.g.
 -   data (Ord k) => Map k v = ...
 - This is because not all functions on Map care about keys being ordered. Either
 - way they need to be added to relevant function defs anyway... -}

{- Value ctor: 3 args; type ctor: 1 arg -}
data Vector a = Vector a a a deriving (Show)
vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector a b c) `vplus` (Vector x y z) = Vector (a+x) (b+y) (c+z)

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

week0 = [minBound .. maxBound] :: [Day]

{- Type synonym: type String = [Char] -}
type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]
inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name,pnumber) `elem` pbook
type AssocList k v = [(k,v)] {- Parameterised! -}
type IntMap = Map.Map Int    {- Partially-applied; needs value -}

data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup number map =
    case Map.lookup number map of
        Just (Taken, _)   -> Left $ "Locker " ++ show number ++ " is already taken!"
        Just (Free, code) -> Right code
        Nothing           -> Left $ "Locker " ++ show number ++ " does not exist!"

{- Creates constructors 'Empty' and 'Cons a (List a)' for you -}
{- data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord) -}
{- Now with custom infix ctor function -}
infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)
list0 = 3 :-: 5 :-: Empty  {- works as fixity is low -}

infixr 5 .++
(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)

{- Binary tree -}
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)
treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert v EmptyTree = Node v EmptyTree EmptyTree
treeInsert v orig@(Node root left right)
    | v == root = orig
    | v < root  = Node root (treeInsert v left) right
    | v > root  = Node root left (treeInsert v right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem v EmptyTree = False
treeElem v (Node root left right)
    | v == root = True
    | v < root  = treeElem v left
    | v > root  = treeElem v right

myTree = foldr treeInsert EmptyTree [8,6,4,1,7,3,5]

{- Typeclasses 102 -}

{- class Eq a where           {- 'a' is a type -}
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x == y = not (x /= y)   {- mutually-recursive def -}
    x /= y = not (x == y)   {- so we only have to implement
                               one of them -} -}

data Greg a = Sure | Somewhat a
instance (Show a) => Show (Greg a) where   {- Ensure a is a Show type -}
    show Sure = "Sure."
    show (Somewhat a) = show a ++ ", yeah"

instance (Eq a) => Eq (Greg a) where
    Sure == Sure = True
    Somewhat x == Somewhat y = x == y
    _ == _ = False

class (Eq a) => Pants a {- Pants typeclass is a subclass of Eq -}

class YesNo a where yesno :: a -> Bool
instance YesNo Int where
    yesno 0 = False
    yesno _ = True
instance YesNo [a] where
    yesno [] = False
    yesno _ = True
instance YesNo Bool where yesno = id
instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing = False
instance YesNo (Tree a) where
    yesno EmptyTree = False
    yesno _ = True
{- yesno is type (YesNo a) => a -> Bool -}

yesNoIf :: (YesNo y) => y -> a -> a -> a
yesNoIf cond yesResult noResult = (if yesno cond then yesResult else noResult)

{- Functor typeclass defines fmap, which takes a function, a value wrapped with
 - its subtype, and returns a new value also wrapped with its subtype. -}
class Functor' f where
    fmap' :: (a -> b) -> f a -> f b
instance Functor' Maybe where
    fmap' f Nothing = Nothing
    fmap' f (Just x) = Just (f x)  {- map f over the Just -}
instance Functor' [] where
    fmap' = map   {- map already propogates a function over values wrapped in [] -}

{- This might screw up an ordered tree if the wrong function is used -}
instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node value left right) = Node (f value) (fmap f left) (fmap f right)

{- We use (Either a) rather than Either: we need a type ctor that takes a single
 - parameter. The Left type (error) stays the same; we map the Right type. -}
{- Type sig: (b -> c) -> Either a b -> Either a c -}
instance Functor' (Either a) where
    fmap' f (Right x) = Right (f x)
    fmap' f (Left x) = Left x

{- Same principle: k stays the same, value is mapped. -}
instance Functor' (Map.Map k) where
    fmap' f m = Map.map f m

{- Type 'kinds': Int is *, Maybe is * -> * (see :k)
 -   class Functor f where
 -     fmap :: (a -> b) -> f a -> f b
 - As this is a type definition, 'f a' must be a concrete type (in addition
 - to a, b and 'f b'. So the f in 'class Functor f' must be a type of kind '* -> *'
 -   to match the definition. -}

{- Bubble sort -}
bubbleSort :: Ord a => [a] -> [a]
bubbleSort list = iterate bubble list !! (length list)  {- run $length times -}
    where bubble [] = []
          bubble [a] = [a]
          bubble (x:y:xs)
            | x <= y =    x : bubble (y:xs)             {- swap disordered elements -}
            | otherwise = y : bubble (x:xs)

{- Input and output -}

readLineByLine = do
    contents <- getContents          {- buffers reads due to laziness -}
    putStr (map toUpper contents)

-- 'interact' encapsulates the above pattern
reverseWordsIO = interact $ unlines . map reverseWords . lines
  where reverseWords = unwords . map reverse . words

{- See 'todo.hs' for more IO stuff -}

randomFloat seed = random (mkStdGen seed) :: (Float, StdGen)
randomInt seed = random (mkStdGen seed) :: (Integer, StdGen)
randomBool seed = random (mkStdGen seed) :: (Bool, StdGen)

coins :: StdGen -> (Bool, Bool, Bool, StdGen)
coins gen1 =
    let (coin1, gen2) = random gen1
        (coin2, gen3) = random gen2
        (coin3, gen4) = random gen3
    in (coin1, coin2, coin3, gen4)

coins' = take 5 $ randoms (mkStdGen 11) :: [Bool]

-- Version of randoms that returns a new generator (and is not infinite)
randomList :: (RandomGen g, Eq n, Num n, Random r) => n -> g -> ([r], g)
randomList 0 g = ([], g)
randomList n g =
    let (r, g') = random g
        (retR, retG) = randomList (n-1) g'
    in (r:retR, retG)
randomListExample = randomList 10 (mkStdGen 10) :: ([Bool], StdGen)

rollOfDice = randomR (1,6) (mkStdGen 52) :: (Int, StdGen)
password00 = take 10 $ randomRs ('a','z') (mkStdGen 3) :: [Char]

printTwoRandomStrings = do
    gen <- getStdGen  -- calling twice returns the same generator, so... 
    putStrLn $ take 20 (randomRs ('a','z') gen)
    gen' <- newStdGen -- this splits the current gen into two 
    putStrLn $ take 20 (randomRs ('a','z') gen')
    {- Another way -}
    let randomChars = randomRs ('a', 'z') gen
        (first20, rest) = splitAt 20 randomChars
        (second20, _) = splitAt 20 rest
    putStrLn first20
    putStrLn second20

{- of note is 'reads':
 -  reads "1 2 3" :: [(Int, String)] -> [(1, " 2 3")]
 -  reads "haha" :: [(Int, String)] -> [] -}

{- Functors, Applicative Functors and Monads -}

-- fmap can be used to apply a function to an IO monad and return an IO monad
fmapIO = do line <- fmap (intersperse '-' . reverse . map toUpper) getLine
            putStrLn line
-- applicative style can join multiple IO actions
applIO = do a <- (++) <$> getLine <*> getLine
            putStrLn a

-- Functor ((->) r) is a type constructor (like Maybe) that creates functions
-- accepting type r as input. E.g. `((->) r) a` for function type `r -> a`
-- For the `(->) r` functor, fmap = (.)  (function composition)
--
-- fmap :: (a -> b) -> ((->) r a) -> ((->) r b)
--  ^ take a function a->b and a function r->a and return a function r->b

justJT = Just (++) <*> Just "johntra" <*> Just "volta"  -- Just "johntravolta"
justJT' = (++) <$> Just "johntra" <*> Just "volta"
-- First step produces Just ("johntra" ++)

allComps0 = [(+),(**)] <*> [1,10] <*> [3,5]
--        = [4.0,6.0,13.0,15.0,1.0,1.0,1000.0,100000.0]
-- Function application, combinatorially.

allComps1 = (+) <$> ZipList [1,2,3] <*> ZipList [100,100..]
--        = [101,102,103] (via getZipList). An alternate way to apply (+)
-- This allows arbitrary zips (vs. zipWith[,2,3,4,5,6,7] functions)

appComp = (+) <$> (*2) <*> (+1) $ 5  -- add 5*2 and 5+1
-- <$> operates on applicative functors. The functor is (-> r) = (+)
--   '+' is applied to the functor's "value" (like with Just a) yielding
--   a function \x -> + (*2 x)
-- <*> is a function builder f <*> g = \x -> f x (g x)
--   In this context, it builds a function f = (+ (*2 x)) and applies
--   it to (x + 1), yielding (+ (*2 x) (+1 x))

bigComp = (\x y z -> [x, y, z]) <$> (+3) <*> (*2) <*> (/2) $ 5
-- [8.0, 10.0, 2.5]
-- <$> injects (+3) into first arg, yielding \x y z -> [(x + 3), y, z]
--   it's just (.) - function composition - in this context
-- <*> returns f, but with (x * 2) applied as an argument ('uncurried'?)
-- <*>' returns that, but with (x / 2) applied
-- Finally we set x = 5 by passing it.

liftA2' :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2' f a b = f <$> a <*> b
-- liftA2 (:) (Just 3) (Just [4]) = Just [3,4]

-- We can write functions that work on any Applicative
sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA [] = pure []
sequenceA (x:xs) = (:) <$> x <*> sequenceA xs

sequenceA' :: (Applicative f) => [f a] -> f [a]
sequenceA' = foldr (liftA2 (:)) (pure [])

-- seqA [M a, M b] -> M [a,b] for applicatives (list, maybe, functions...)
-- seqA [(+2),(*2),(**2)] = single function \x -> [(x+2),(x*2),(x**2)]

fitsConstraints = and . sequenceA [(>4),(<10),odd]

{-
  Law of applicative functors: pure f <*> x = fmap f x 
 
  For Maybe: pure f = Just f
             (Just f) <*> x = fmap f x by definition

  For []:    pure f = [f]
             fs <*> xs  = [f x | f <- fs, x <- xs]
             [f] <*> xs = [f x | x <- xs]
                        = map f xs = fmap f xs (for Functor [])

  For IO:    pure = return
             a <*> b = do f <- a; x <- b; return (f x)
             (return f) <*> b = do
                f <- (return f)   -- identity
                x <- b            -- unwraps b
                return (f x)      -- applies (f x) and wraps
             
             fmap f x = x >>= (return . f), which is the same
               (>>= just does application for monads)

  For ((->) r):  pure x = (\_ -> x)
                 f <*> g = \x -> f x (g x)
                 (\_ -> f) <*> g = (\_ -> f) x (g x)
                                 = f (g x) = f (.) g = fmap f g
-}

data ZipList' a = ZipList' [a]
data ZipList_ a = ZipList_ { getZipList_ :: [a] } -- generate func for us
newtype Zyplist a = ZypList { getZypList :: [a] } -- runs faster
newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show)

-- If we want a Pair Functor that maps the first value, the type of the
-- first value will change! So we swap the types around.
newtype Pair b a = Pair { getPair :: (a,b) }
instance Functor (Pair c) where
    fmap f (Pair (x,y)) = Pair (f x, y)

-- newtypes does not involve a 'wrapper' over their values in the way
-- data does; they also have only a single-valued ctor. This means
newtype CoolBool = CoolBool { getCoolBool :: Bool }
helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"  -- ...will always return hello,
                                -- even if we pass 'undefined'
-- ...but if CoolBool was 'data', Haskell would need to check if the value
-- was in the single-value ctor form (vs. a hypothetical CoolBool a b)
-- and evaluate the first argument, which is less lazy.

-- IN SUMMARY: `type` is for synoyms (no ctor)
--             `newtype` wraps existing types (with ctor), Haskell
--             considers it a new type - e.g. CharList ++ String won't work
--               we also need to re-implement type classes
--             `data` is for custom data types with multiple fields/ctors

{- Monoids are: associative (a -> a -> a) functions with an identity value
 -  examples are + (0), * (1) and ++ ([]) -}
class Monoid m where
    mempty :: m             -- the identity value
    mappend :: m -> m -> m  -- apply the function
    mconcat :: [m] -> m
    mconcat = foldr mappend mempty
instance Monoid [a] where
    mempty = []
    mappend = (++)

monoidString = "one" `mappend` "two" `mappend` "three"

-- newtype allows us to tell Haskell if we want product or sum semantics.

newtype Product a = Product { getProduct :: a }
  deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Monoid (Product a) where
    mempty = Product 1
    Product x `mappend` Product y = Product (x * y)
    -- Sum is defined similarly

newtype Any = Any { getAny :: Bool }
    deriving (Eq, Ord, Read, Show, Bounded)
instance Monoid Any where
    mempty = Any False
    Any x `mappend` Any y = Any (x || y)
    -- All is defined similarly

instance Monoid Ordering where
    mempty = EQ
    LT `mappend` _ = LT
    EQ `mappend` y = y       -- note, this means it's not associative!
    GT `mappend` _ = GT

lengthCompareCrap :: String -> String -> Ordering
lengthCompareCrap x y = let a = length x `compare` length y
                            b = x `compare` y
                        in if a == EQ then b else a

-- mappend means we'll pick the first non-EQ result
lengthCompare :: String -> String -> Ordering
lengthCompare x y = (length x `compare` length y) `mappend` (x `compare` y)

-- adding another compare level (number of vowels)
lengthCompare' :: String -> String -> Ordering
lengthCompare' x y = mconcat $ map (\f -> f x y) criteria  -- or (flip ($ x) y)
   where criteria = [compare `on` length, compare `on` vowels, compare]
         vowels = length . filter (`elem` "aeiou")

-- The maybe monad has two variants, First and Last. When applied they
-- yield the first or last non-Nothing value.
just9 = MN.getFirst . MN.mconcat . map MN.First $ [Nothing, Just 9, Just 10]
just10 = MN.getLast . MN.mconcat . map MN.Last $ [Nothing, Just 9, Just 10]

-- Make our Tree a Foldable, so we can run F.foldl et al on it
-- Note that f must map each value to a monoid so we know how to combine them
-- e.g. \x -> [x]             (join as list)
--      \x -> Any $ x == 3    (True if any value is 3, False otherwise)
instance F.Foldable Tree where
    foldMap f EmptyTree = MN.mempty
    foldMap f (Node x l r) = F.foldMap f l `MN.mappend`
                             f x           `MN.mappend`
                             F.foldMap f r

exTree = Node 5 (Node 3 (Node 1 EmptyTree EmptyTree) (Node 6 EmptyTree EmptyTree)) (Node 9 (Node 8 EmptyTree EmptyTree) (Node 10 EmptyTree EmptyTree))
exTreeSum = F.foldl (+) 0 exTree --or getSum $ F.foldMap Sum tree
exTreeProduct = F.foldl (*) 1 exTree
exTreeContainsThree = MN.getAny $ F.foldMap (MN.Any . (== 3)) exTree
exTreeAsList = F.foldMap (\x -> [x]) exTree

{- A Fistful of Monads -}
{-
 - In English: we have a fancy value and a function that takens a normal
 - value but returns a fancy value. How do we feed that fancy value into the
 - function? e.g. a 'bind' function that modifies the computation based on the
 -   Monadic input context.
 -
 - (Just "foo") >>= (\x -> Just (x ++ "!"))   = Just "foo!"
 - Nothing >>= (...) = Nothing
 -}

class Monad_ m where
    bind_ :: m a -> (a -> m b) -> m b  -- aka >>=, described above
    return_ :: a -> m a                -- alias for pure

instance Monad_ Maybe where
    return_ = Just
    Nothing `bind_` _ = Nothing
    (Just a) `bind_` f = f a

{- Pierre balancing on a tightrope. Left/right birds must be within three -}
type Birds = Int
type Pole = (Birds,Birds)
landLeft_ :: Birds -> Pole -> Pole
landLeft_ birds (left,right) = (left + birds, right)
landRight_ :: Birds -> Pole -> Pole
landRight_ birds (left,right) = (left, right + birds)

x -: f = f x   -- reverse apply, so we can do (0,0) -: landLeft 1 -: landRight 2

-- If we want to detect imbalance, we need to rewrite these functions. Now they
-- return Maybe Pole.
landLeft n (left, right)
    | abs ((left + n) - right) < 4 = Just (left + n, right)
    | otherwise                    = Nothing
landRight n (left, right)
    | abs (left - (right + n)) < 4 = Just (left, right + n)
    | otherwise                    = Nothing

-- Now we can use >>= to thread Pole through a (Pole -> Maybe Pole) function
pierreOK = landRight 1 (0,0) >>= landLeft 2 >>= landRight 1 -- Just (2,2)
pierreFalls = pierreOK >>= landRight 10 -- Nothing!

-- Slightly more consistent way of writing pierreOK:
pierreOK' = return (0,0) >>= landRight 1 >>= landLeft 2 >>= landRight 1

banana :: Pole -> Maybe Pole
banana _ = Nothing

pierreFalls' = return (0,0) >>= landLeft 1 >>= banana >>= landRight 1
-- (>>= banana) can be replaced with (>> Nothing)
-- where (>> x) returns a function \_ -> x, except Monad-wrapped with >>=

doPierre :: Maybe Pole
doPierre = do
    start <- return (0,0)        -- Just (0,0) >>= (\start -> ...)
    first <- landLeft 2 start    -- >>= (\first -> (landLeft 2 start)) >>= ...
    second <- landRight 2 first 
    landLeft 1 second

doPierreFalls = do
    start <- return (0,0)
    first <- landLeft 2 start
    Nothing                      -- same as (>> Nothing)
    second <- landRight 2 first  --   which ignores the prev. result and just
    landLeft 1 second            --   returns Nothing

-- Pattern matching is also possible
justH = do (x:xs) <- Just "hello"; return x

-- Failed pattern matches invoke fail, which is error by default, but Nothing for
-- Maybe types.
wopwop = do (x:xs) <- Just ""; return x   -- is Nothing

-- List Monad represents non-determinism
posNeg = [3,4,5] >>= \x -> [x,-x]   -- = [3,-3,4,-4,5,-5], i.e. concat map
posNeg' = do x <- [3,4,5]; [x,-x]   -- same thing

-- This is how list comprehensions are implemented under the hood
listMonad = do x <- [1,2,3]; y <- ['a', 'b', 'c']; return (x,y)
-- = [(1,'a'),(1,'b'),(1,'c'),(2,'a'),(2,'b'),(2,'c'),(3,'a'),(3,'b'),(3,'c')]
listMonad' = [ (x,y) | x <- [1,2,3], y <- ['a', 'b', 'c'] ]

-- But what about guards? These are handled by the MonadPlus typeclass, which
-- represents Monads that are also Monoids.
class Monad m => MonadPlus' m where                        --  List:
    mzero :: m a                        -- Monoid 'mempty'       []
    mplus :: m a -> m a -> m a          -- Monoid 'mappend'     (++)

-- guard function takes a bool and returns either 'return ()' or mzero
-- guard (5 > 2) :: Maybe ()   = Just ()  = [()]  -- always '$monad ()'
-- guard (1 > 2) :: Maybe ()   = Nothing  = []

hasSeven = [1..50] >>= (\x -> guard ('7' `elem` show x) >> return x)

-- guard works in conjunction with >>, which uses the context of its first
-- arg to determine its result, remember:
--   Nothing >> Just 3 = Nothing         x >> y = x >>= \_ -> y
--   Just 3 >> Nothing = Nothing                = Nothing (if x is Nothing)
--   Just 3 >> Just 4 = Just 4                  = y (if x is Just a)
-- When guard true, [()] >> l  is l
-- When guard false, [] >> l   is []

sevensOnly :: [Int]
sevensOnly = do
    x <- [1..50]
    guard ('7' `elem` show x)
    return x                      -- important. else, result would be [(),(),(),(),()]
-- And that's how list comprehensions are made!

-- Monad Laws: TLDR
-- Left identity   :   return x >>= f  === f x
--   Due to return's "minimal" context
--   For Maybe: return is Just, and 'f x' is a successful computation
--
-- Right identity  :   m >>= return   === m
--   Return extracts the value from the monad and returns it again
--   For Maybe: we don't introduce any failure at random
--
-- Associativity   :  (m >>= f) >>= g  === m >>= (\x -> f x >>= g)
--   Monads can be composed in any order
--
-- If we define  f <=< g = (\x -> g x >>= f)
--   (composes two monadic functions)
--   f <=< return === f                  just as f . id = f
--   return <=< f === f                          id . f = f
--   f <=< (g <=< h)  ===  (f <=< g) <=< f
--       just as (f . g) . h  ===  f . (g . h)

{- A few Monads more -}

monoidExample = (["milk"], Sum 25) `mappend` (["whiskey", Sum 10)
  -- => (["milk", "whiskey"], Sum {getSum = 35})
