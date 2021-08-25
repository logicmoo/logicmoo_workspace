for i in `cat Versions-no-i386.lst`;do echo $i; time ./install_miz_loc.sh-no-i386 $i; done | tee 00log1
time for i in `cat Versions.lst`;do echo $i; time ./install_miz_loc.sh $i; done | tee 00log2
time git push -u origin master
git push --tags
