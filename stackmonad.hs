-- A stack monad. Supports push and pop.
import Data.Monoid
import Control.Monad

newtype Stack a = Stack { runStack :: (a,[a]) }

instance Monad Stack where
    return a = Stack (a, [])
    Stack (a, s) >>= k = k a

pop stack = (head stack, tail stack)

push :: a -> Stack a
push (a,s) = Stack ((), a:s)

example = do
    push 1
    push 2
    a <- pop
    b <- pop
    return (show a) ++ (show b)

main = putStrLn $ runStack example

