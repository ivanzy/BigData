module Main where
import System.IO

calculaCB :: (Integral a) => a -> a -> a
calculaCB n k = product [k+1..n] `div` product [1..n-k]

main :: IO ()
main = do	
print $ calculaCB 10 5
print $ calculaCB 5 3
