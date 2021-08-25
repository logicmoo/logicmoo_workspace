#!/bin/bash
# $1= 7.8.10_4.99.1005
# script fetching and installing new mizar version into the MML directory
# version using the win32 release
#wget ftp://mizar.uwb.edu.pl/pub/system/i386-win32/mizar-$1-i386-win32.exe
#$mkdir $1
cd MML
rm -rf abstr doc mml prel
rm *
unzip ../mizar-$1-i386-win32.exe
unzip -a -o mmlfull -d mml
unzip -a -o mizdoc -d doc
unzip -a -o mizbib -d doc
unzip -a -o mizxml -d doc/xml
unzip -a -o prel -d prel
unzip -a -o abstr -d abstr
unzip -a -o mizdb1
unzip -a -o mizxml miz.xsl
mv miz.xsl miz.xml

rm abstr.zip install.bat license mizdb1.zip mizsys.zip mizxml.zip prel.zip unzip.exe mizbib.zip mizdoc.zip mizutil.zip  mmlfull.zip 

git add .
git commit -a --author="Library Committee <mml@mizar.uwb.edu.pl>" -m $1
git tag $1
#cp -a mml miztmp
#cp Makefile  miztmp
#export MIZFILES=`pwd`
#cd miztmp
