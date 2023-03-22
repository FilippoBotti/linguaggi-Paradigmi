skyscraper' [] (acc,res) = res
skyscraper' (x:xs) (acc, res)
    | x > acc = skyscraper' xs (x, (res+1))
    | otherwise = skyscraper' xs (acc, (res))

skyscraper list = skyscraper' list (0, 0)


-- maxim [] acc = acc
-- maxim (x:xs) acc = maxim xs (max x acc)

-- maxim' list = maxim list 0