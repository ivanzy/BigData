
module Main where
import System.IO

test :: (Eq a,Num a, Ord a) => a -> a -> a -> Bool
test a b c 
	| a < b + c = True
	| otherwise = False

positivos :: (Eq a,Num a, Ord a) => a -> a -> a -> Bool
positivos a b c = (a > 0) && (b > 0) && ( c > 0) 

testaIsosceles :: (Eq a,Num a, Ord a) => a -> a -> a -> Bool
testaIsosceles a b c = (a==b) || (a==c) || (b==c)

triangleCondition :: (Eq a, Num a, Ord a) => a -> a -> a -> String
triangleCondition a b c
	| not((test a b c) && (test b a c) && (test c a b) && (positivos a b c)) = "nao eh triangulo"
	| a==b && b==c = "equilatero"
	| testaIsosceles a b c = "isosceles"
	| otherwise = "escaleno"

main :: IO ()
main = do	
print (triangleCondition 5 8 3)
print (triangleCondition 15 12 17)
print (triangleCondition 12 12 17)
print (triangleCondition 5 5 5)
print (triangleCondition 1 1 0)
print (triangleCondition 3 3 12)
