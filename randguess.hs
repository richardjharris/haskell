import System.Random
import Control.Monad (when)

main = do
    gen <- getStdGen
    askForNumber gen

askForNumber gen = do
    putStrLn "Can you guess what number I am thinking of?"
    guessStr <- getLine
    when (not $ null guessStr) $ do   {- if user presses 'Enter' without guess -}
        let guess = read guessStr :: Int
            (rand, newGen) = randomR (1,10) gen :: (Int, StdGen)
        if guess == rand
            then putStrLn "You are correct!"
            else putStrLn $ "Sorry, it was " ++ show rand
        askForNumber newGen
