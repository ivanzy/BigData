import Data.List   
 
collatz :: (Integral a) => a -> a
collatz x
	| x `mod` 2 == 0 = x `div` 2
	| otherwise = (3*x + 1)

problem_14 = j 1000000 where   
f :: Int -> Integer -> Int   
f k 1 = k   
f k n = f (k+1) $ collatz n 
g x y = if snd x < snd y then y else x   
h x n = g x (n, f 1 n)   
j n = fst $ foldl' h (1,1) [2..n-1]

main :: IO ()
main = do	
	print $ problem_14
