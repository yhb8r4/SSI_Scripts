#!/bin/bash

# Extract cartesian coordinates
for i in *.out; 
do sed -n '/FINAL STATE/,/RESTART DATA/p' $i | sed 1,4d | sed 's/RESTART DATA//g'  > $i.xyz; done

#splice files I need to find the total atoms for amino acids and splice there to get fragments a and b 
for file in *a.xyz; do
python splicescripta.py ${file:0:10}a.xyz ${file:0:10}b.xyz ${file:0:10}f.inp.out.xyz | sed '/^$/d' >> "${file:0:10}a.txt1"; done
for file in *b.xyz; do
python splicescriptb.py ${file:0:10}a.xyz ${file:0:10}b.xyz ${file:0:10}f.inp.out.xyz | sed '/^$/d' >> "${file:0:10}b.txt1"; done

#for i in *.txt1; do
sed -i -e 's/C        /C       12.0    /g' $i; 
sed -i -e 's/H        /H       1.00    /g' $i; 
sed -i -e 's/O        /O       16.0    /g' $i;
sed -i -e 's/N        /N       14.0    /g' $i;
sed -i -e 's/S        /S       32.0    /g' $i; done

#average coordinates of monomers to get centroids of A and B
for file in *.txt1; do
python centerofmass.py $file >> $file.txt2;  done

#transform name so script doesn't fuck up
for i in *.txt1.txt2; do mv "$i" $(echo "$i" | sed 's/.txt1//'); done

#call distance calculating script between monomers:
for file in *a.txt2; do
python distancescript.py ${file:0:10}a.txt2 ${file:0:10}b.txt2 >> "${file}.distance"; done

#remove temp files
rm *.txt1;
rm *.txt2; 
rm *.txt1-e

#produce logfile of all distances
grep 'distance' *.distance > optimizeddistances.txt; 

