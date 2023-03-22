def merge(v1,v2):
    a = 0
    countV1 = 0
    countV2 = 0
    while countV1 < len(v1):
        if lista[count] > a:
            a = lista[count]
            yield(a)  # ~ append in fib_list, but lazy
        count += 1