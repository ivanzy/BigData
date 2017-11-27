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

main :: IO ()
main = do	
	print $ geraIdentidade 4
	print $ geraIdentidade 7
	print $ geraIdentidade 10
