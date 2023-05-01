sum' [] z = z
sum' (x:xs) z = sum' xs (x+z)


sum'' xs = foldr (+) 0 xs