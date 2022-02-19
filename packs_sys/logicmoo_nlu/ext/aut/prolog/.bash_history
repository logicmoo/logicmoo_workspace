vi MA_uc.ARI
vi Gen_mf.ari
touch intoswi.pl
swipl -l intoswi.pl 
swipl intoswi.pl 
sed 's/[$]/`/g' -i *.BAK
sed 's/[$]/`/g' -i *.ARI
sed 's/[$]/`/g' -i *.ari
sed "s/[$]/`/g" -i *.ari
sed -e "s/\[\!/aritySlice((/g" -e  "s/\!\]/))/g" nl_util.ari 
sed -e "s/\[\!/aritySlice((/g" -e  "s/\!\]/))/g" -i nl_util.ari 
sed -e 's/\[\!/aritySlice((/g' -e  's/\!\]/))/g' -i *.BAK
sed -e 's/\[\!/aritySlice((/g' -e  's/\!\]/))/g' -i *.ARI
sed -e 's/\[\!/aritySlice((/g' -e  's/\!\]/))/g' -i *.ari
sed -e "s/\[\!/aritySlice((/g" -e  "s/\!\]/))/g" -i *.*
sed -e 's/\[\!/aritySlice((/g' -e  's/\!\]/))/g'
sed -e "s/\[\!/aritySlice((/g" -e  "s/\!\]/))/g"
sed -e "/s/\[\!/aritySlice((/g" -e  "s/\!\]/))/g" 
sed -e "/s/\[\!/aritySlice((/g" -e  "/s/\!\]/))/g" 
sed -e "/s/\[\!/aritySlice((/g" -e  "/s/\!\]/))/g"
sed -e "/s/[\[\!]/aritySlice((/g" -e  "/s/\!\]]/))/g"
sed -e "/s/[\[\!]/aritySlice((/g" -e  "/s/[\!\]]/))/g"
sed -e "/s/[\[!]/aritySlice((/g" -e  "/s/[!\]]/))/g"
sed -e "/s/[[!]/aritySlice((/g" -e  "/s/[!\]]/))/g"
sed -e "s/[$]/`/g" -i *.ari
rm -rf foo/
mv SALE_MA.ARI SALE_MA_uc.ARI
mv *.pl is_pl/
mv MA.ARI MA_uc.ARI
mv is_pl/intoswi.pl .
mv gen.ari gen_lc.ari
mv foo/ ..
mkdir is_pl
mkdir foo
ls *.pl
ls *ARI 
ls *ari
git status ./*
git status .
git status
git commit -am "computational autism"
git add .
find / -xdev -size +10M
find -xdev /.. -size +10M
find -xdev / -size +10M
find / -xdev -maxdepth 2 -size +10M -exec ls -l {} \; | grep Dec
find / -xdev -maxdepth 2 -size +10M -exec ls -l {} \;
find / -xdev -maxdepth=2 -size +10M
find / -xdev -maxdepth 2 -size +10M
find ../.. -size +10M
find .. -size 10M
find .. -size +10M
find .. -size +10m
find .. -name "*z"
find .. --name "*z"
find .. -name "*ip"
find .. -iname "*ip"
find ../../auttttt/ -name au_dom.ari
du . -h
du .
\cp ../../auttttt/Appendix/training_images/au/Demo/au/au2003/*.ari .
\cp ../../auttttt/Appendix/training_images/au/Demo/au/au2003/*.* .
cp ../../auttttt/Appendix/training_images/au/Demo/au/*.ari .
\cp ../../auttttt/Appendix/training_images/au/Demo/au/*.* .
cd is_pl/
cd foo/
cd ..
cd ..
ls
git add .
git status
git status
git add .
git status
