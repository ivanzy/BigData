module Main where
import Data.List

fib = 1 : 2 : prox fib
  where
    prox (x : t@(y:_)) = (x+y) : prox t


main :: IO ()
main = do
	let euler2 =   foldr (+) 0 [x | x<- takeWhile (< 4000000) (fib), x `mod` 2 ==0]
	print euler2
