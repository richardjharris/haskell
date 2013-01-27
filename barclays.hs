import Control.Monad
import Data.Array
import Data.List
import qualified Data.Map as M
import System.Random

-- Barclays problem: two trucks, a set of packages, get packages into trucks
-- such that trucks are as equal as possible. Returns truck contents + sums

type DiffMap = M.Map Int Int
compareCases :: Int -> IO DiffMap
compareCases n = compareCases_ n M.empty

compareCases_ :: Int -> DiffMap -> IO DiffMap
compareCases_ 0 map = return map
compareCases_ n diffmap = do
    gen <- newStdGen
    let items = randomItems gen
        (_,_,t1,t2) = barclays items
        (_,_,u1,u2) = truckFill items
        t = min t1 t2
        u = min u1 u2
        diff = abs $ u - t
    compareCases_ (n-1) $ M.insertWith' (+) diff 1 diffmap
    
randomItems gen = let (noItems,gen') = randomR (1,100) gen
                      items = take noItems $ randomRs (1,100) gen'
                  in items

-- Optimal solution: treat as knapsack problem with target weight of
-- half the sum of all weights; solve via dynamic programming
barclays :: [Int] -> ([Int], [Int], Int, Int)
barclays xs = 
    let
        targetValue = (sum xs) `div` 2
        items = map (\(i,n) -> Item i n n) $ zip [1..] xs
        Cell (_, packed) = dynapack targetValue items
        inTruck1 = map itemSize packed
        inTruck2 = map itemSize $ (items \\ packed)
    in (inTruck1, inTruck2, sum inTruck1, sum inTruck2)

-- Naive solution: fill one truck until weights exceed the other truck
truckFill :: [Int] -> ([Int], [Int], Int, Int)
truckFill [] = ([], [], 0, 0)
truckFill (x:xs) =
    let (t1,t2,sum1,sum2) = truckFill xs
    in
    if sum1 <= sum2 then (x:t1,t2,sum1+x,sum2)
                    else (t1,x:t2,sum1,sum2+x)

-- A knapsack item
data Item a = Item { item :: a, itemValue :: Int, itemSize :: Int }
    deriving (Eq, Show, Ord)

-- A solution or cell: summed value, items in sack
data Cell a = Cell (Int, [Item a])
    deriving (Eq, Show, Ord)

-- For each element in the list, half possible subsets will include it,
-- half will not.
powerset :: [a] -> [[a]]
powerset = filterM (const [True, False])

-- Brute force implementation
brutepack :: (Ord a) => Int -> [Item a] -> Cell a
brutepack size items = maximum packs
    where packs = [cellOf subset | subset <- powerset items, itemsFit subset]
          itemsFit items = sum (map itemSize items) <= size
          cellOf items = Cell (sum (map itemValue items), items)

dynapack :: Int -> [Item a] -> Cell a
dynapack size items = valOf noItems size
    where
        noItems = length items
        itemsArr = listArray(1,noItems) items
        itemNo n = itemsArr ! n
        
        table = array ((1,1),(noItems,size)) $
            [((m,n), cell m n) | m <- [1..noItems], n <- [1..size]]

        valOf m n
         | m < 1 || n < 1 = Cell (0, [])
         | otherwise      = table ! (m,n)

        cell m n =
            case itemNo m of
                i@(Item _ v s)
                 | s > n || vL >= vU + v   -> Cell (vL, isL)
                 | otherwise               -> Cell (vU + v, i:isU)
                 where
                    Cell (vL, isL) = valOf (m - 1) n
                    Cell (vU, isU) = valOf (m - 1) (n - s)
