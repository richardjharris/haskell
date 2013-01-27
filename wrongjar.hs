module WrongJar(
    WrongJar,
    empty,
    isEmpty,
    add,
    total,
    wrong,
    runWrong
) where

import Control.Monad.Writer
import Data.Monoid
import Text.Printf

data Wrong = Wrong { getPerson :: String, getWrong :: String, getMoney :: Float }
  deriving (Eq, Show)

newtype WrongJar = WrongJar { getJar :: [Wrong] }
  deriving (Eq, Show)

empty :: WrongJar
empty = WrongJar []

isEmpty = (== empty)

add :: WrongJar -> String -> String -> Float -> WrongJar
add jar person wrong money =
    let w = Wrong person wrong money
    in WrongJar (w:getJar jar)

singleton :: String -> String -> Float -> WrongJar
singleton = add empty

merge :: WrongJar -> WrongJar -> WrongJar
merge a b = WrongJar (getJar a ++ getJar b)

summarise jar =
    "The jar total is " ++ t ++ ". The most recent wrong was '" ++ w ++ "' committed by " ++ person ++ ", costing " ++ money ++ "."
    where t = formatMoney $ total jar
          lastWrong = last $ getJar jar
          w = getWrong lastWrong
          person = getPerson lastWrong
          money = formatMoney $ getMoney lastWrong

formatMoney :: Float -> String
formatMoney m
    | m < 1     = (show $ round (m * 100)) ++ "p"
    | otherwise = "£" ++ printf "%.2f" m

total :: WrongJar -> Float
total (WrongJar []) = 0.0
total jar = sum . map getMoney . getJar $ jar

instance Monoid WrongJar where
    mempty = empty
    mappend = merge

newtype WrongWriter = Writer WrongJar

wrong person thing money = tell $ singleton person thing money

runWrong code =
    let (result, jar) = runWriter code
    in "Result is " ++ show result ++ "\n" ++ case isEmpty jar of
        True  -> "No wrongs committed."
        False -> summarise jar

doStuff = do
    wrong "rjh" "This is a wrong!" 0.3
    wrong "sjw" "Again..." 0.1
    return (3+4)

main = print $ runWrong doStuff
