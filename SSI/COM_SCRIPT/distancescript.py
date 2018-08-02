#!/usr/bin/python
import pandas
import numpy as np
import re
import math
import sys

# Take names for files from command line
inputnamea = sys.argv[1]
inputnameb = sys.argv[2]

dataa=open(inputnamea, 'r+')
datab=open(inputnameb, 'r+')

for line in dataa:
    linesa = line.split()
    linesa = map(float,linesa)

for line in datab:
    linesb = line.split()
    linesb = map(float,linesb)
 
xcoordinates = (linesa[0]-linesb[0])**2
ycoordinates = (linesa[1]-linesb[1])**2
zcoordinates = (linesa[2]-linesb[2])**2

add = xcoordinates + ycoordinates + zcoordinates

distance = np.sqrt(add)

print 'distance: ', distance

  

