calculate :: (Num x) , (String y) => y -> x -> x -> y 
calculate "/" a 0 = "Error, division by zero"
calculate "+" a b = show(a + b)
calculate "-" a b = show(a - b)
calculate "*" a b = show(a * b)
calculate "/" a b = show(a / b)
calculate x a b = "Error in parse argument"




