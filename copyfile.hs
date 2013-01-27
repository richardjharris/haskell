{-# LANGUAGE ScopedTypeVariables #-}

import System.Environment
import qualified Control.Exception as E
import qualified Data.ByteString.Lazy as B

main = do
    (filename1:filename2:_) <- getArgs
    E.catch (copyFile filename1 filename2)
            (\(err :: E.IOException) -> putStrLn ("ARGH! " ++ show err))

{- Without ScopedTypeVariants, we need to write this differently.
 -
 - E.catch (copyFile filename1 filename2)
 -         (\e -> do let err = show (e :: E.IOException)
 -                   hPutStr stderr ("Warning: " ++ err)
 -                   return "")
 -}

copyFile :: FilePath -> FilePath -> IO ()
copyFile source dest = do
    contents <- B.readFile source
    B.writeFile dest contents
