class Skyscraper:
    '''iterator that yields Fibonacci numbers'''
    def __init__(self, list):
        self._list = list

    def __iter__(self):
        self._current = 0
        self._a = 0
        return self

    def __next__(self):
        while self._list[self._current] <= self._a:
            self._current += 1
            if self._current == len(self._list):
                raise StopIteration
        if self._list[self._current] > self._a:
            self._a = self._list[self._current]
        list = self._a

        return list
    
def skys(lista):
    a = 0
    count = 0
    while count < len(lista):
        if lista[count] > a:
            a = lista[count]
            yield(a)  # ~ append in fib_list, but lazy
        count += 1

def main():
    print("Iterator:")
    s = Skyscraper([1,2,5,3,4])
    for v in s: print(v, end=' ')
    print("\nGenerator:")
    s = skys([1,2,5,3,4])
    for v in s: print(v, end=' ')

main()