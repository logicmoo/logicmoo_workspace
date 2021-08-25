# make the dcl2,sch2 and the2 files from xml2
LANG=C;
for i in tarski `cat mml.lar`; do 
    echo $i;
    grep "fof([dt][0-9]" pl/$i.xml2 > pl/$i.the2; 
    grep "fof([fcr]c[0-9]" pl/$i.xml2 > pl/$i.dcl2;
    grep "fof([s][0-9]" pl/$i.xml2 > pl/$i.sch2;
    grep "fof([l][0-9]" pl/$i.xml2 > pl/$i.lem2;
done
