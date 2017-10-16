

main = do
	let b = [x | x <- [0..2017],  (x `rem` 400 == 0) || ((x `rem` 4 == 0) && (x `rem` 100 /= 0)) ]
	print b 


