#!/usr/bin/env python3
'''
ADDING THE SQUARE OF TWO NUMBERS                                       SB_ini.py

AUTHOR          Sibbe Bakker
STRUCTURE       Using sys.argv to import a file name and read te file.
'''
import sys
F = sys.argv[1]
with open(F) as f:
    data = f.readline()
    numbers = data.split()
first = int(numbers[0])
second = int(numbers[1])
print(first**2 + second**2)
