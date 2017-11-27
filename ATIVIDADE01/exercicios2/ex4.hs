module Main where
import System.IO

isPrime :: Integer -> Bool
isPrime k = null [ x | x <- [2..k - 1], k `mod`x  == 0]
-- a avaliação preguiçosa aqui foi uma indicação de um usuário do StackOverflow

main :: IO ()
main = do	
	print $ isPrime 10
	print $ isPrime 11
	print $ isPrime 5387
	print $ isPrime 123454
	
