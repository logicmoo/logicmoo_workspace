export MAL_DIR=/home/urban/mptp/MaLARea
echo "downcasing"
echo "##############################"
for i in `ls`; do j=`echo $i | tr [A-Z] [a-z]`; echo $i; mv $i $j; done
mkdir axioms conjectures0 conjectures fofified renamed0 renamed
echo "fofifying axioms"
echo "##############################"
for i in `ls *.p`; do echo $i; grep "cnf([^,]*,axiom," $i | tptp4X -t fofify -u machine -- >axioms/$i; done
for i in `ls *.p`; do echo $i; grep -v "cnf([^,]*,axiom," $i >conjectures0/$i; done
cd conjectures0
mkdir res 
echo "fofifying conjectures"
echo "##############################"
for i in `ls`; do /home/urban/TPTPWorld/TPTP/TPTP2X/tptp2X  -d `pwd`/res  -t fofify:obvious  $i; done
cd res
for i in `ls`; do j=`echo -n $i | sed -e 's/.fof_obvious.tptp//'`; tptp4X -u machine -c $i | sed -e "s/^fof([^,]*,/fof($j,/" > ../../conjectures/$j.p; done
cd ../..
echo "merging axioms and conjectures"
echo "##############################"
for i in `ls *.p`; do echo $i; cat axioms/$i conjectures/$i | $MAL_DIR/bin/NumberVars > fofified/$i; done
cd fofified
echo "uniquifying Isabelle v_ and t_ local constants"
for i in `ls *.p| sed -e 's/[.]p$//g'`; do sed  -e "s/\\b\\([vt]\\)_/\\1_$i\_/g" $i.p; done
echo "sorting formulas into 00"
echo "##############################"
cat *| sort -u >00
echo "merging same formulas"
echo "##############################"
$MAL_DIR/script/rename_from_sorted.pl
cd ../renamed0
sort -u 00 >01
echo "renaming different formulas with the same name"
echo "##############################"
$MAL_DIR/script/mk_diff_names.pl
cd ../renamed
echo "finding problems with not their own conjecture (see file 02)"
echo "##############################"
for i in `ls *.p`; do j=`echo -n $i| sed -e 's/.p$//'`; grep -L "^fof($j,conjecture" $i; done |tee 02
echo "finding ju_hack-renamable problems with not their own conjecture (see file 03)"
echo "##############################"
grep -l "ju_hack.*conjecture" `cat 02` | tee 03
grep -L "ju_hack.*conjecture" `cat 02` | tee 04
echo "renaming ju_hack conjectures (the rest of 02 problems will not be used)"
echo "##############################"
for i in `cat 03`; do j=`grep 'ju_hack.*conjecture' $i|sed -e 's/^fof(\([^,]*\),conjecture.*/\1/'` ; echo $j; cp $i $j.p.new; done
echo "copying all to .new"
for i in `ls *.p`; do cp $i $i.new; done
for i in `cat 02`; do rm $i.new; done
# for i in `ls *.p`; do j=`echo -n $i| sed -e 's/.p$//'`; echo $j; sed -e "s/^fof([^,]*,conjecture/fof($j,conjecture/" $i >$i.new; done
