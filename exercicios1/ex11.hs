 
module Main where
import System.IO

concatena ::  [Char] -> [Char]
concatena a = (takeWhile (/= ' ') a) ++ (tail (dropWhile (/= ' ') a))

main :: IO ()
main = do
	let a = "Ivan Dimitry"
	print (concatena a)


