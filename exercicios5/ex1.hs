module Main where
import System.IO
import Data.List
--new data types
data Color = Yellow | Red | Blue | Ivory | Green deriving (Show,  Eq, Enum, Ord)
data Drink = Tea | Milk | Coffee | Water | Orange deriving (Show,  Eq, Enum, Ord)
data Pet = Zebra | Dog | Snails | Fox | Horse deriving (Show,  Eq, Enum, Ord)
data Nationality = Norwegian | Japanese | Ukrainian | Spaniard | Englishman deriving (Show,  Eq, Enum, Ord)
data Smoke = LuckyStrike | OldGold | Parliament | Kools | Chesterfield deriving (Show,  Eq, Enum, Ord)
data House = House Color Nationality Pet Drink Smoke deriving (Show,  Eq, Ord)


cartProd cs ns ps ds ss = [House c n p d s | c <- cs, n <- ns, p <- ps, d <- ds, s <-ss]

getAllPermutations :: Ord a => [a] -> [[a]]
getAllPermutations x = nub $ map sort $ map (take 5) (permutations x)

test3 n p  --The Spaniard owns the dog.
	| (n /= Spaniard) && (p/= Dog) = True
	| (n == Spaniard) && (p== Dog) = True 
	| otherwise = False 

test4 c d--Coffee is drunk in the green house.
	| (d /= Coffee) && (c/= Green) = True
	| (d == Coffee) && (c== Green) = True
	| otherwise = False 

test5 n d--The Ukrainian drinks tea.
	| (d /= Tea) && (n/= Ukrainian) = True
	| (d == Tea) && (n== Ukrainian) = True
	| otherwise = False 

test6 c1 c2--The green house is immediately to the right of the ivory house.
	| (c1 == Green) && (c2 == Ivory) = False --the order can't be in the opposite way
	| (c2 == Green) && (c1 == Ivory) = True
	| (c2 /= Green) && (c1 /= Ivory) = True
	| otherwise = False 

test7 s p--The Old Gold smoker owns snails.
	| (s /= OldGold) && (p/= Snails) = True
	| (s == OldGold) && (p== Snails) = True
	| otherwise = False 

test8 s c--Kools are smoked in the yellow house.
	| (s /= Kools) && (c/= Yellow) = True
	| (s == Kools) && (c== Yellow) = True
	| otherwise = False 

test11 s p--The man who smokes Chesterfields lives in the house next to the man with the fox.
	| (s /= Chesterfield) && (p/= Fox) = True
	| (s == Chesterfield) && (p== Fox) = True
	| otherwise = False 

test12 s p--Kools are smoked in the house next to the house where the horse is kept.
	| (s /= Kools) && (p/= Horse) = True
	| (s == Kools) && (p== Horse) = True
	| otherwise = False 

test13 s d-- The Lucky Strike smoker drinks orange juice.
	| (s /= LuckyStrike) && (d/= Orange) = True
	| (s == LuckyStrike) && (d== Orange) = True
	| otherwise = False 

test14 s n-- --The Japanese smokes Parliaments.
	| (s /= Parliament) && (n/= Japanese) = True
	| (s == Parliament) && (n== Japanese) = True
	| otherwise = False 


testChallenge :: [House] -> Bool
testChallenge [(House c1 n1 p1 d1 s1),(House c2 n2 p2 d2 s2),(House c3 n3 p3 d3 s3),(House c4 n4 p4 d4 s4),(House c5 n5 p5 d5 s5)]
	| n1 /= Norwegian = False --The Norwegian lives in the first house.
    | c2 /= Blue = False --The Norwegian lives next to the blue house.
	| d3 /= Milk = False -- Milk is drunk in the middle house.
	| (((test3 n1 p1) && (test3 n2 p2) && (test3 n3 p3) && (test3 n4 p4) && (test3 n5 p5)) == False) = False
	| (((test4 c1 d1) && (test4 c2 d2) && (test4 c3 d3) && (test4 c4 d4) && (test4 c5 d5)) == False) = False
	| (((test5 n1 d1) && (test5 n2 d2) && (test5 n3 d3) && (test5 n4 d4) && (test5 n5 d5)) == False) = False
	| (((test6 c1 c2) && (test6 c2 c3) && (test6 c3 c4) && (test6 c4 c5)) == False) = False
	| (((test7 s1 p1) && (test7 s2 p2) && (test7 s3 p3) && (test7 s4 p4) && (test7 s5 p5)) == False) = False
	| (((test8 s1 c1) && (test8 s2 c2) && (test8 s3 c3) && (test8 s4 c4) && (test8 s5 c5)) == False) = False
	| (((test11 s1 p2) || (test11 s2 p1) || (test11 s3 p2) || (test11 s4 p3) || (test11 s5 p4) || (test11 s2 p3) || (test11 s3 p4) || (test11 s4 p5)) == False) = False
	| (((test12 s1 p2) || (test12 s2 p1) || (test12 s3 p2) || (test12 s4 p3) || (test12 s5 p4) || (test12 s2 p3) || (test12 s3 p4) || (test12 s4 p5)) == False) = False
	| (((test13 s1 d1) && (test13 s2 d2) && (test13 s3 d3) && (test13 s4 d4) && (test13 s5 d5)) == False) = False
	| (((test14 s1 n1) && (test14 s2 n2) && (test14 s3 n3) && (test14 s4 n4) && (test14 s5 n5)) == False) = False
	| otherwise = True
	   
checkValid houses 
	| length houses == 5 = testChallenge houses
	| otherwise = error "lista invalida"

resolveChallenge :: [[House]] -> [House]
resolveChallenge houses = resolveChallenge' houses 0
	where
		 resolveChallenge' houses n
			| (length houses == 0) = error "nao achou combinacao correta"
			| (checkValid (houses !! n) == True) = (houses !! n)
			| otherwise = resolveChallenge (tail houses) 

main :: IO ()
main = do
	let h1 = House Yellow	Norwegian	Fox		Water	Kools
	let h2 = House Blue		Ukrainian	Horse	Tea 	Chesterfield 
	let h3 = House Red		Englishman 	Snails	Milk	OldGold
	let h4 = House Ivory 	Spaniard 	Dog		Orange	LuckyStrike
	let h5 = House Green 	Japanese 	Zebra	Coffee	Parliament
	let allCombinations = cartProd [Yellow .. Green] [Norwegian .. Englishman] [Zebra .. Horse] [Tea .. Orange] [LuckyStrike .. Chesterfield]
	print $ resolveChallenge (getAllPermutations allCombinations)
	print $ testChallenge [h1,h2,h3,h4,h5]
	print $ testChallenge [h1,h2,h3,h5,h4]



	
