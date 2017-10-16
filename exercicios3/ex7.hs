module Main where
import System.IO

collatz :: (Integral a) => a -> a
collatz x
	| x `mod` 2 == 0 = x `div` 2
	| otherwise = (3*x + 1)

collatzLen :: (Integral a) => a -> a
collatzLen x = collatzLen' x 0
		where
			collatzLen' x cont 
				|  x == 1 = cont
				| otherwise  = collatzLen' (collatz x) (cont+1)

main :: IO ()
main = do	
print $ collatzLen 10
print $ collatzLen 5
print $ collatzLen 123
