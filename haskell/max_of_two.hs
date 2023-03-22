max_of_two :: (Ord a) => a -> a -> a
max_of_two first second
    | first <= second = second
    | otherwise = first