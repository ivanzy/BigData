mult35 :: Integer -> Bool
mult35 n = ((n `mod` 5 == 0 ) && (n `mod` 3 == 0 ))

main = do
    print $ ( mult35 10 )
    print $ ( mult35 12 )
    print $ ( mult35 30 )
    print $ ( mult35 10123 )

