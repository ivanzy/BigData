mult5 :: Int -> Bool
mult5 n = (n `mod` 5 == 0 )

main = do
    print $ ( mult5 10 )
    print $ ( mult5 12 )
    print $ ( mult5 30 )
    print $ ( mult5 10123 )

