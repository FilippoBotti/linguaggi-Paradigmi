


skyscraper' xs = foldl (\(acc,res) x -> if x > acc then (x,res + 1) else (acc,res)) (0,0) xs