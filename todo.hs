{- Todo program. Maintains a file called todo.txt.
 - Usage:
 -  todo [list]
 -  todo [Thing to be done]
 -  todo done #
 -}
import System.Directory
import System.Environment
import System.IO
import Data.List

fileName = "todo.txt"

main = do
    args <- getArgs
    dispatch args

dispatch :: [String] -> IO ()
dispatch [] = cmdList
dispatch ["list"] = cmdList
dispatch ("list":_) = error "list takes no arguments"
dispatch ["bump", num] = cmdBump $ read num
dispatch ("bump":_) = error "usage: bump <number>"
dispatch ["done", num] = cmdDone $ read num
dispatch ("done":_) = error "usage: done <number>"
dispatch todoText = cmdTodo $ unwords todoText

cmdList :: IO ()
cmdList = do
    todo <- readFile fileName
    let todoTasks = lines todo
        numberedTasks = zipWith (\n line -> show n ++ ". " ++ line) [1..] todoTasks
    putStr $ unlines numberedTasks

cmdDone :: Int -> IO ()
cmdDone num = rewriteTodo (removeIndex (num-1))

cmdBump :: Int -> IO ()
cmdBump num = rewriteTodo (bumpToTop (num-1))
    where bumpToTop n list = (list !! n) : (removeIndex n list)

cmdTodo :: String -> IO ()
cmdTodo thing = do
    appendFile fileName (thing ++ "\n")
    cmdList

rewriteTodo :: ([String] -> [String]) -> IO ()
rewriteTodo rewriteFunc = do
    handle <- openFile fileName ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle
    let todoTasks = lines contents
        newTodoItems = rewriteFunc todoTasks
    hPutStr tempHandle $ unlines newTodoItems
    hClose handle
    hClose tempHandle
    removeFile fileName
    renameFile tempName fileName
    cmdList

removeIndex n list = (take n list) ++ (drop (n+1) list)
