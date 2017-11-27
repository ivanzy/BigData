module Main where
import System.IO

collatz :: (Integral a) => a -> a
collatz x
	| x `mod` 2 == 0 = x `div` 2
	| otherwise = (3*x + 1)
main :: IO ()
main = do	
print $ collatz 10
print $ collatz 5
