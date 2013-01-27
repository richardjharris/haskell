import System.Environment
import System.IO
import Data.List
import Data.Function (on)

data Section = Section { getA :: Int, getB :: Int, getC :: Int } deriving Show
type RoadSystem = [Section]

heathrowToLondon :: RoadSystem
heathrowToLondon = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]

data Label = A | B | C deriving Show
type Path = [(Label, Int)]

pathLen = sum . map snd
minPath = minimumBy (compare `on` pathLen)

-- Given section and best paths to An and Bn, produce
-- best paths to A(n+1), B(n+1)
roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
      let newA = minPath [(A,a):pathA, (C,c):(B,b):pathB]
          newB = minPath [(B,b):pathB, (C,c):(A,a):pathA]
      in (newA, newB)

optimalPath :: RoadSystem -> Path
optimalPath roadSystem =
    let (bestA, bestB) = foldl roadStep ([],[]) roadSystem
    in reverse $ minPath [bestA, bestB]
    where solve = foldl' roadStep ([],[])

-- Input/output
groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf n xs
    | n <= 0 = error "Can't take a negative/zero group"
    | otherwise = let (group,rest) = splitAt n xs
                  in group : groupsOf n rest

main = do
    contents <- getContents
    let threes = groupsOf 3 (map read $ lines contents)
        roadSystem = map (\[a,b,c] -> Section a b c) threes
        path = optimalPath roadSystem
        pathString = concatMap (show . fst) path
        pathPrice = pathLen path
    putStrLn $ "The best path to take is: " ++ pathString
    putStrLn $ "The price is: " ++ show pathPrice
