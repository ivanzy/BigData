import Control.Monad.Random

main :: IO ()
main = do
    let xs = [1..10]
    ys <- shuffle xs
    print $ doSomething ys

doSomething :: [Integer] -> Integer
doSomething = sum
