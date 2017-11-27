module Main where
import System.Environment
import System.IO



ex7 :: Double -> (Double, Double)
ex7 a =  (sqrt((1- cos(a))/2),-sqrt((1- cos(a))/2))

main :: IO ()
main = do
    print	(ex7 pi )



