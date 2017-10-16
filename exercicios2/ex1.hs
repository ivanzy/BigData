
module Main where
import System.IO

test :: (Eq a,Num a, Ord a) => a -> a -> a -> Bool
test a b c 
	| a < b + c = True
	| otherwise = False

positivos :: (Eq a,Num a, Ord a) => a -> a -> a -> Bool
positivos a b c = (a > 0) && (b > 0) && ( c > 0) 

triangleCondition :: (Eq a, Num a, Ord a) => a -> a -> a -> Bool
triangleCondition a b c = (test a b c) && (test b a c) && (test c a b) && (positivos a b c)

main :: IO ()
main = do	
print (triangleCondition 5 8 3)
print (triangleCondition 15 12 17)
print (triangleCondition 15 12 17)
print (triangleCondition 5 5 5)
print (triangleCondition 1 1 0)
print (triangleCondition 3 3 12)
