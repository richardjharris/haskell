import Data.List

{- Solve string-form RPN and return the result. -}
solveRPN :: String -> Float
solveRPN = head  . foldl rpnStep [] . words

rpnStep :: [Float] -> String -> [Float]
rpnStep (x:y:ys) "*" = (x * y):ys
rpnStep (x:y:ys) "+" = (x + y):ys
rpnStep (x:y:ys) "-" = (y - x):ys
rpnStep (x:y:ys) "/" = (y / x):ys
rpnStep (x:y:ys) "**" = (y ** x):ys
rpnStep (x:xs) "ln" = log x:xs
rpnStep xs "sum" = [sum xs]
rpnStep xs numberString = (read numberString : xs)
