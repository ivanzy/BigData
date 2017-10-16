 
module Main where
import System.IO
import Data.List (length)


takeLast :: [Integer] -> [Integer]
takeLast (_:xs)
	| length xs == 10 = xs
	| otherwise = takeLast xs

main :: IO ()
main = do
	let b = [x | x <- [0..2017],  (x `rem` 400 == 0) || ((x `rem` 4 == 0) && (x `rem` 100 /= 0)) ]
	print $ takeLast b

