collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz n
    | even n =  n:collatz (n `div` 2)
    | odd n  =  n:collatz (n*3 + 1)

all_collatz = (map (collatz) [1..100] )

collatz_count = length (filter (\x -> length x > 15) all_collatz)