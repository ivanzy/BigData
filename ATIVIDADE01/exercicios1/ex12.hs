 
module Main where
import System.IO
import Data.Char


charToInt :: [Char] -> [Integer]
charToInt a =  map (fromIntegral) $ (map (digitToInt) a)
main :: IO ()
main = do	
	let a = "0123456789"
	print (charToInt a)

