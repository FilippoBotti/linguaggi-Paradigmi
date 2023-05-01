
data Ball = Ball Int Int Int Int deriving (Show)


move (Ball x y dx dy) = Ball (x+dx) (y+dy) dx dy

contains_W (Ball x y 5 5)= if (x + 5 <=0 || x+5 >= 360-20) then Ball x y (-5) 5 else Ball x y 5 5 
contains_H (Ball x y 5 5)= if (y+5 <= 0 || y+5 >= 360 - 20) then Ball x y 5 (-5) else Ball x y 5 5



