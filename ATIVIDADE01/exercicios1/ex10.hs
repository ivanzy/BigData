 
module Main where
import System.IO
import Data.List (length)


takeLast :: [Int] -> Int-> [Int]
takeLast (_:xs) num
	| length xs == num = xs
	| length xs < num = error "deu pau" 
	| otherwise = takeLast xs num

main :: IO ()
main = do
	let b = [x | x <- [0..2017],  (x `rem` 400 == 0) || ((x `rem` 4 == 0) && (x `rem` 100 /= 0)) ]
	let len = (length b) `div` 2
	print len
	print $ ((take len b), takeLast b len)

