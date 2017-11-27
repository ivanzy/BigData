module Main where
import System.IO

sumDigits :: Integer -> Integer
sumDigits k = sumDigits' k 0
	where 
		sumDigits' k r
			| k >= 10 = sumDigits' (k `div` 10) (r+(k `mod` 10))
			| otherwise = k+r

persAdd :: Integer -> Integer
persAdd num = persAdd' num 0
	where
	persAdd' num cont
		| num >= 10 = persAdd' (sumDigits num) (cont+1) 
		| otherwise = cont


main :: IO ()
main = do	
	print $ persAdd 1111
	print $ persAdd 7801
	print $ persAdd 8911

	
