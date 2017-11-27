module Main where
import System.IO

prodEsc :: Num a => [a] -> [a] -> a 
prodEsc a b  = sum (zipWith (*) a b)



main :: IO ()
main = do	
	print $ prodEsc [1,1,1] [1,1,1]
