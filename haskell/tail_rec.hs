rev' :: [a] -> [a] -> [a]
rev'  [] acc = acc
rev'  (x:xs) acc  = rev' xs (x: acc )

reverse' x = rev' x []