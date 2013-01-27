import Data.List
import System.Random

-- 1. Find the last element of a list.
myLast :: [a] -> a
myLast = foldl1 (flip const) -- \acc x -> x
myLast' = foldr1 (const id) -- \x acc -> acc

-- 2. Find last but one element of a list.
myButLast :: [a] -> a
myButLast (x:xs)
    | length xs == 2 = head xs
    | otherwise = myButLast xs

myButLast' = last . init
myButLast'' [x,_] = x
myButLast'' (_:xs) = myButLast'' xs

-- 3. Find K'th element of list, 1-indexed
elementAt (x:_) 1 = x
elementAt (_:xs) n = elementAt xs (n-1)

elementAt' list i = list !! (i-1)

-- 4. Find number of elements in list
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myLength' = foldl (\n _ -> n + 1) 0
myLength'' = foldr (\_ -> (+1)) 0  -- succinct!, same as \_ n -> 1 + n

-- 5. Reverse a list
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

myReverse' = foldl (flip (:)) []  -- rewritten as a fold, which is clearer

myReverse'' list = myReverse_ list []   -- avoids re-consing
    where myReverse_ [] r = r
          myReverse_ (x:xs) r = myReverse_ xs (x:r)

-- 6. Is the list a palindrome?
isPalindrome x = x == reverse x
--isPalindrome = ap (==) reverse -- pointfree

-- 7. Flatten a nested list structure
data NestedList a = Elem a | List [NestedList a]
flatten (Elem a) = [a]
flatten (List b) = concatMap flatten b

-- 8. Eliminate consecutive duplicates
compress (x:ys@(y:_))
  | x == y    = compress ys
  | otherwise = x : compress ys
compress ys = ys -- empty/singleton

compress' xs = map head $ group xs

-- 9. Pack consecutive duplictes into sublists (group)
pack :: Eq a => [a] -> [[a]]
pack (x:ys@(y:_))
  | x == y    = injectPack x (pack ys)
  | otherwise = [x] : pack ys
   where injectPack x ys = (x:head ys):tail ys
pack x = [x] -- empty/singleton

pack' (x:xs) = let (first,rest) = span (==x) xs
               in (x:first) : pack rest
pack' [] = [] -- how elegant!

-- 10. Run-length encoding of a list
encode :: Eq a => [a] -> [(a, Int)]
encode xs = map (\x -> (head x, length x)) $ pack xs

encode' xs = [(length x, head x) | x <- group xs]

-- 11. As above, but single elements are copied directly
data RLE a = Multiple Int a | Single a deriving Show
encodeModified xs = map encodeRLE . pack $ xs
    where encodeRLE [x] = Single x
          encodeRLE xs = Multiple (length xs) (head xs)

-- 12. Decode RLE
decodeModified :: [RLE a] -> [a]
decodeModified = concatMap decodeRLE
    where decodeRLE (Single x) = [x]
          decodeRLE (Multiple n x) = replicate n x

-- 13. Run-length encoding without creating sublists
encodeDirect :: (Eq a) => [a] -> [RLE a]
encodeDirect = map mapSingle . foldr helper []
    where helper x [] = [Multiple 1 x]
          helper x' (y@(Multiple n x):ys)
            | x == x'   = (Multiple (n+1) x):ys
            | otherwise = (Multiple 1 x'):y:ys

          mapSingle (Multiple 1 x) = Single x
          mapSingle m = m

-- 14. Duplicate the elements of a list, once each
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:dupli xs

dupli' = concatMap (\x -> [x,x])

-- 15. Replicate the elements of a list a given number of times
repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = replicate n x ++ repli xs n

repli' xs n = concatMap (replicate n) xs

repli'' xs n = xs >>= replicate n  -- via list monad === concatMap

-- 16. Drop every Nth element from a list.
dropEvery :: [a] -> Int -> [a]
dropEvery xs 0 = xs
dropEvery xs n = map fst . filter ((n /=) . snd) . zip xs $ cycle [1..n]
dropEvery' xs n = [ i | (i,c) <- (zip xs [1..]), (mod c n) /= 0 ] -- similar

-- 17.  Split a list into two parts, given the length of the first part
split xs n = helper ([], xs) n
    where helper (xs, ys) 0 = (xs, ys)
          helper (xs, y:ys) n = helper (xs ++ [y], ys) (n-1)
   -- ^ but inefficient due to cons. Other way is: split (x:xs) n is the same as
   --   unshifting x onto the split of xs (n-1)
split' xs 0 = ([], xs)
split' (x:xs) n = let (f,l) = split' xs (n-1) in (x:f, l)

-- 18. Slice of a list, 1-indexed
slice xs m n = [ x | (x,i) <- (zip xs [1..n]), i >= m ]

-- 19. Rotate a list N places to the left
-- Doesn't work with |n| > |length xs|
rotate xs n
    | n >= 0 =    (slice xs (n+1) len) ++ (slice xs 1 n)
    | otherwise = (slice xs (len+n) len) ++ (slice xs 1 (len + n - 1))
  where len = length xs

rotate' xs n = drop nn xs ++ take nn xs
  where nn = n `mod` length xs

-- 20. Remove K'th element
-- Really inefficient solution...
removeAt n xs = (e, without)
    where e = xs !! (n-1)
          without = slice xs 1 (n-1) ++ slice xs (n+1) (length xs)

removeAt' n xs =
    let (front, x:rest) = splitAt n xs
    in (x, front ++ rest)

-- 21. Insert an element at the given position
insertAt e xs n =
    let (front, back) = splitAt n xs
    in front ++ e:back

-- 22. Create a list containing all integers within a given range
range m n = drop (m-1) [1..n]  -- or just [m..n], or enumFromTo

-- 23. Extract a given number of randomly selected elements from list
rndSelect :: RandomGen g => [a] -> Int -> g -> ([a], g)
rndSelect _ 0 gen = ([], gen)
rndSelect [] _ gen = ([], gen)
rndSelect xs count gen =
    let (idx, gen') = randomR (1, length xs) gen
        (picked, left) = removeAt idx xs
        (picks, gen'') = rndSelect left (count - 1) gen'
    in (picked:picks, gen'')

-- Instead of extracting and storing 'count' elements, we can remove
-- (length - count) elements. Elements are not returned in a random
-- order.
rndSelect' _ 0 gen = ([], gen)
rndSelect' [] _ gen = ([], gen)
rndSelect' xs count gen
    | count == (length xs) = (xs, gen)  -- removed enough elements
    | otherwise = let (k, gen') = randomR (1, length xs) gen
                  in rndSelect' (snd $ removeAt k xs) count gen'

-- 24. Draw N different random numbers from the set 1..M
lottoSelect n m gen = rndSelect' [1..m] n gen

-- 25. Generate a random permutation of the elements of a list.
rndPermu xs gen = rndSelect xs (length xs) gen

-- 26. Generate the combinations of K distinct objects chosen from
-- the N elements of a list.
comb :: Int -> [a] -> [[a]]

-- 27. Group element into disjoint subsets of e.g. 2,3,4 persons
-- group [2,3,4] ['a'..'z']
--  -> [[['a','b'], ['c','d','e'], ['f','g','h','i']], ... ]
--  -> 1260 solutions in total
--  -> ignoring permutation of members within groups, but we do
--  count [['a','b'], ['c','d'], ...] as different to
--        [['c','d'], ['a','b'], ...]
