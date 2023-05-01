from functools import reduce

def my_lambda(acc,val):
    res, max = acc
    if(val>max):
        max = val
        res+=1
    return res,max

myList = [1,2,3,4,5,1]
skyscraper = reduce(my_lambda, myList, (0,0))

print(myList, "Il max cambia " ,skyscraper[0], " volte")