mult3 :: Int -> Bool
mult3 n = (n `mod` 3 == 0 )

main = do
    print $ ( mult3 10 )
    print $ ( mult3 12 )
    print $ ( mult3 3 )
    print $ ( mult3 10123 )

