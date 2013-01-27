-- Generate road system for crossroads.hs
-- Takes system length as optional argument
import System.Environment
import System.Random

main = do
    args <- getArgs
    gen <- getStdGen
    let (roadLength) = parseArgs gen args
    newStdGen
    printRoadSystem gen roadLength

parseArgs :: StdGen -> [String] -> (Int)
parseArgs gen [] = fst . randomR (1,100) $ gen
parseArgs _ [roadLength] = read roadLength

printRoadSystem :: StdGen -> Int -> IO ()
printRoadSystem gen roadLength =
    let numbers = randomRs (1,100) gen :: [Int]
        roads = take (roadLength * 3) numbers
        lines = map show roads
    in putStr (unlines lines)
