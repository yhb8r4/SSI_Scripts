#!/bin/bash

for i in *.xyz.efp; do mv "$i" $(echo "$i" | sed 's/.xyz\.efp/.efp/'); done

for i in *.efp; do sed -i -e 's/$FRAGNAME/$'${i%.*}'/g' $i; done

for i in *.efp; do sed -i -e '/>/{ N; s/>\n LMO CENTROIDS/\n LMO CENTROIDS/ }' $i; done

for i in *.efp; do sed -i -e '/CANONVEC/,/STOP/d' $i; done

for i in *.efp; do sed -i -e '/CANONFOK/,/STOP/d' $i; done

for i in *.efp; do sed -i -e '/SCREEN3/,/STOP/d' $i; done

#for i in *.efp; do sed -i -e '/SCREEN2/,/STOP/d' $i; done

for i in *.efp; do sed -i -e '/SCREEN1/,/STOP/d' $i; done

for i in *.xyz; do sed -n '1,3p' $i >> $i.efp; done

for i in *.xyz.efp; do sed -i -e 's/^.....//' $i >> $i; done

for i in *.xyz.efp; do mv "$i" $(echo "$i" | sed 's/.xyz\.efp/lefp/'); done

for i in *.efp; do mv -f "$i" `echo "$i" | tr A-Z a-z`; done

for i in *lefp; do mv -f "$i" `echo "$i"|tr A-Z a-z`; done

for i in *.xyz; do mv -f "$i" `echo "$i" | tr A-Z a-z`; done

for i in *lefp; do sed -i "1ifragment ${i%.*}" "$i"; done

for i in *lefp; do sed -i -e 's/lefp//g' $i; done

for i in *alefp; do fa="*alefp"; fb="*blefp"; fn=`echo $i | sed -E 's/alefp//'`; suf="m.inp"; cat $fn$fa $fn$fb > "$fn$suf"; done

rm *lefp;

for i in *m.inp; do cat TITLE $i >> $i.inp; done

rm *m.inp;

for i in *m.inp.inp; do mv "$i" $(echo "$i" | sed 's/m\.inp\.inp/f\.inp/'); done
