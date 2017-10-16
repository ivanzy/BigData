ex5 :: Integer -> Bool
ex5 n = (n < -1) || (n>1 && n `mod` 2 == 0)

main = do
    print $ ( ex5 5 )
    print $ ( ex5 12 )
    print $ ( ex5 (-30) )
    print $ ( ex5 10123 )

