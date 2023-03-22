# odd_squares = sum (filter odd (takeWhile (< 10000) (map (^2) [1..])))
from functools import partial
from itertools import takewhile, count

pow2 = partial(pow, exp=2)


lista = (map((pow2), count()))
result = list(takewhile(lambda x: x < 10000, lista))
somma = filter(lambda x: x%2==1, result)
print(sum(somma))



result2 = takewhile(lambda x: x<10000, (pow2(x) for x in count() if pow2(x)%2==1 ))

print((sum(result2)))



