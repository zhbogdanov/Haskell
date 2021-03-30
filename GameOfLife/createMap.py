import sys
from random import randint

def transform(num):
    if num == 1:
        return '*'
    return '.'

args = list(map(int, sys.argv[1:]))
with open("map.txt", 'w') as fout:
    for i in range(args[0]):
        for j in range(args[1]):
            fout.write(transform(randint(0, 1)))
        fout.write('\n')

