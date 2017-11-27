module Main where
import System.IO

calculaCB :: (Integral a) => a -> a -> a
calculaCB n k = product [k+1..n] `div` product [1..n-k]

tPascal :: (Integral a) => a -> a -> a
tPascal linha coluna = calculaCB (linha-1) (coluna-1)

main :: IO ()
main = do	
print $ tPascal 10 5 --linha coluna
print $ tPascal 5 3
