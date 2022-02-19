touch intoswi.pl
swipl -l intoswi.pl 
sed -e "s/\[\!/aritySlice((/g" -e  "s/\!\]/))/g" nl_util.ari 
sed -e "s/\[\!/aritySlice((/g" -e  "s/\!\]/))/g" -i nl_util.ari 
sed -e "s/\[\!/aritySlice((/g" -e  "s/\!\]/))/g" -i *.*
sed -e "s/\[\!/aritySlice((/g" -e  "s/\!\]/))/g"
sed -e "/s/\[\!/aritySlice((/g" -e  "/s/\!\]/))/g"
sed -e "/s/[\[\!]/aritySlice((/g" -e  "/s/\!\]]/))/g"
sed -e "/s/[\[\!]/aritySlice((/g" -e  "/s/[\!\]]/))/g"
sed -e "/s/[\[!]/aritySlice((/g" -e  "/s/[!\]]/))/g"
sed -e "/s/[[!]/aritySlice((/g" -e  "/s/[!\]]/))/g"
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
\cp ../../auttttt/Appendix/training_images/au/Demo/au/au2003/*.ari .
\cp ../../auttttt/Appendix/training_images/au/Demo/au/au2003/*.* .
cp ../../auttttt/Appendix/training_images/au/Demo/au/*.ari .
\cp ../../auttttt/Appendix/training_images/au/Demo/au/*.* .
cd foo
cd ..
mv foo/ ..
git status
git status .
git status .
git status ./*
git add .
ls
