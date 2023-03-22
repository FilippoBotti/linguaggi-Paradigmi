mean list = fromIntegral (fives list) / fromIntegral (len list)
fives list = sum (take (len list) list) 
len list = minimum (5:[length list])