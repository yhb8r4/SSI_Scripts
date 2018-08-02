#!/bin/bash
for i in *.txt; do sed  -i -e 's/20/aryl/g'  $i; done
for i in *.txt; do sed  -i -e 's/19/aryl/g'  $i; done
for i in *.txt; do sed  -i -e 's/18/aryl/g'  $i; done
for i in *.txt; do sed  -i -e 's/17/aliphatic/g'  $i; done
for i in *.txt; do sed  -i -e 's/16/aliphatic/g'  $i; done
for i in *.txt; do sed  -i -e 's/15/aliphatic/g'  $i; done
for i in *.txt; do sed  -i -e 's/14/aliphatic/g'  $i; done
for i in *.txt; do sed  -i -e 's/13/aliphatic/g'  $i; done
for i in *.txt; do sed  -i -e 's/12/aliphatic/g'  $i; done
for i in *.txt; do sed  -i -e 's/11/thiol/g' $i;  done
for i in *.txt; do sed  -i -e 's/10/thiol/g'  $i; done
for i in *.txt; do sed  -i -e 's/9 /polar/g'   $i; done
for i in *.txt; do sed  -i -e 's/8 /polar/g'   $i; done
for i in *.txt; do sed  -i -e 's/7 /polar/g'   $i; done
for i in *.txt; do sed  -i -e 's/6 /polar/g'   $i; done
for i in *.txt; do sed  -i -e 's/5 /anionic/g'   $i; done
for i in *.txt; do sed  -i -e 's/4 /anionic/g'   $i; done
for i in *.txt; do sed  -i -e 's/3 /cationic/g'   $i; done
for i in *.txt; do sed  -i -e 's/2 /cationic/g'   $i; done
for i in *.txt; do sed  -i -e 's/1 /cationic/g'   $i; done
