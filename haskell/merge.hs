merge' [] ys = ys
merge' xs [] = xs

merge' (x:xs) (y:ys) 
    | x < y = x : merge' xs (y:ys)
    | otherwise = y : merge' (x:xs) ys

merge_sort :: (Ord a) => [a] -> [a]

merge_sort [x] = [x]

merge_sort [] = []

merge_sort v  =  merge'   (merge_sort(take (len v)  v)) (merge_sort(drop (len v)  v))

len v  = (length v) `div` 2

       
