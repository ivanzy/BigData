import Control.Monad.Random

main :: IO ()
main = do
    gen <- getStdGen -- We could easily use a different RNG here if we wanted
    let values = evalRand dieRolls gen -- Evaluate using our RNG
    let a =  (take 5 $ values)
    print a

-- Single die roll function.
dieRolls :: RandomGen g => Rand g [Int]
dieRolls =  getRandomRs (1, 5) 
