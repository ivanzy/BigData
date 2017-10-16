module Main where
import System.IO

sumDigits :: Integer -> Integer
sumDigits k = sumDigits' k 0
	where 
		sumDigits' k r
			| k >= 10 = sumDigits' (k `div` 10) (r+(k `mod` 10))
			| otherwise = k+r

main :: IO ()
main = do	
	print $ sumDigits 1157
	print $ sumDigits 7801
	print $ sumDigits 8911
	print $ sumDigits 19
	print $ sumDigits 10

	
