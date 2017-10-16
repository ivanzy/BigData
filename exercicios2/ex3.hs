module Main where
import System.IO

multEtiope :: Integer -> Integer -> Integer
multEtiope a b = multEtiope' a b 0
	where
		multEtiope' a b r
			| a ==1 = (r+b)
			| (a `mod` 2 == 1) = multEtiope' (a `div` 2) (b*2) (r+b)
			| otherwise = multEtiope' (a `div` 2) (b*2) (r)
			


main :: IO ()
main = do	
	print $ multEtiope 14 12
	print $ multEtiope 25 25
	print $ multEtiope 10 48
