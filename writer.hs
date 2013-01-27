-- My attempt to implement the Writer Monad.
import Data.Monoid
import Control.Monad

newtype Writer w a = Writer { runWriter :: (a,w) }
    deriving (Eq, Ord, Show)

instance (Monoid w) => Monad (Writer w) where
    return a             = Writer (a, mempty)
    (Writer (a,w)) >>= k = let (Writer (a',w')) = k a in Writer (a', w `mappend` w')

-- Returns (), which is ignored.
tell :: Monoid w => w -> Writer w ()
tell w = Writer ((), w)

-- Testing the implementation
logNumber :: Int -> Writer [String] Int
logNumber x = Writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    tell ["Gonna multiply these two"]
    return (a*b)

main = print $ runWriter multWithLog
-- (15,["Got number: 3","Got number: 5"])
