#!/usr/bin/python

import sys
import math
import pandas
import re
import numpy as np
import linecache

#assign files
xyzcoordinatesa = sys.argv[1]
xyzcoordinatesb = sys.argv[2]
opticoordinates = sys.argv[3]

#split into columns for matching
columnnames = ['ATOM', 'C1', 'C2', 'C3']
data = pandas.read_csv(xyzcoordinatesa,names=columnnames, header=None, delimiter=r"\s+")
data1 = pandas.read_csv(xyzcoordinatesb,names=columnnames, header=None, delimiter=r"\s+")
data2 = pandas.read_csv(opticoordinates,names=columnnames, header=None, delimiter=r"\s+")

#make column elements into list:
AtomlistxyzA = map(str, list(data.ATOM))
AtomlistxyzB = map(str, list(data1.ATOM))
Atomlistxyzdimer = map(str, list(data2.ATOM))

#counter of lines from xyzfile
num_linesa = sum(1 for line in AtomlistxyzA) + 1
num_linesb = sum(1 for line in AtomlistxyzB) + 1
num_linesc = sum(1 for line in Atomlistxyzdimer) + 1

#for every num_lines print out that amount of lines from optimizated coordinate file 
for line in xrange(0, num_linesa):
    print linecache.getline(opticoordinates, line)



#else:
#    print 'No match:', line
