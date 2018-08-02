#!/usr/bin/python
import pandas
import numpy as np
import re
import math
import sys

input = sys.argv[1]
csv = np.genfromtxt(input)
numericals=csv[:,1:5]
coordinates=np.transpose(csv[:,2:5])
weights = numericals[:,0]
Netmass = np.sum(weights)
multipledcsv = weights*coordinates

weightedcsv = np.transpose(multipledcsv[:])

averagedx=np.sum(weightedcsv[:,0])/Netmass
averagedy=np.sum(weightedcsv[:,1])/Netmass
averagedz=np.sum(weightedcsv[:,2])/Netmass

print averagedx , ' ', averagedy, ' ', averagedz, ' '
                                                                                                                                 
