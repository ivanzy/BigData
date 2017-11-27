module Main where
import System.IO

divisivel20 :: (Integral a) => a -> Bool
divisivel20 x = null [n | n <- [1..20], x `mod` n /= 0]

main :: IO ()
main = do	
	print $	divisivel20 232792560 
	print $	divisivel20 2123

