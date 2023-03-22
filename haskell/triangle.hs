pythagoras = [(x,y,z) | x <- [1..10], y <- [1..10], z <- [1..10], x+y+z == 24, x<=y, x<=z, y<=z, x ^ 2 + y ^ 2 == z ^ 2]
