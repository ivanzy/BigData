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

somaDiag :: [[Int]] -> Int
somaDiag m = somaDiag' (m) (0) (0)
	where
		somaDiag' m cont n 
			| (length m) /= n = somaDiag' (m) (cont+( (m!!n) !! n)) (n+1)
			| otherwise = cont

main :: IO ()
main = do	
	print $ somaDiag [[1,2,3],[1,2,3],[1,2,3]]
	print $ somaDiag (geraIdentidade 40)
	print $ somaDiag (geraIdentidade 10)
