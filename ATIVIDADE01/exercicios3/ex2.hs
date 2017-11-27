module Main where
import System.IO
import Data.List


main :: IO ()
main = do	 
	print $ (foldl lcm 2 [3..20])

