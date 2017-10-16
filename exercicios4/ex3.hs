import Data.List   
 
geraLista:: Int -> Int -> [Int]
geraLista a n = geraLista' a n 0 []
	where
		geraLista' a n cont l
			| n==cont = l
			| a==(cont+1) = geraLista' a n (cont+1) (l ++ [1])
			| otherwise = geraLista' a n (cont+1) (l  ++ [0] )

geraIdentidade :: Int -> [[Int]]
geraIdentidade n = [geraLista x n | x <- [1..n]]

somaDiagSec :: [[Int]] -> Int
somaDiagSec m = somaDiag' m 0 ((length m) - 1)
	where
		somaDiag' m cont n 
			| n >= 0 = somaDiag' (m) (cont+( (m!!((length m) - n -1)) !! n)) (n-1)
			| otherwise = cont


main :: IO ()
main = do	
	print $ somaDiagSec [[1,2,30],[1,2,3],[1,2,3]]
	print $ somaDiagSec (geraIdentidade 3)
