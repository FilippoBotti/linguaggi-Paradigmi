from random import shuffle

def count_tops(values: list[int]) -> int:
    max = 0
    count = 0
    for num in values:
        if num > max:
            max = num
            count+=1
    return count

def main():
    values = list(range(1, 6))
    shuffle(values)  # e.g., [3, 1, 4, 2, 5]
    print("La tua lista: ", values)
    print("Il numero di volte che cambia il massimo: ", count_tops(values))

main()