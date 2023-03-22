odd_squares = sum (filter odd (takeWhile (< 10000) (map (^2) [1..])))
