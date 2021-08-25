#!/bin/bash
# $1= 7.8.10_4.99.1005
# script fetching and installing new mizar version into the current directory
#wget ftp://mizar.uwb.edu.pl/pub/system/i386-linux/mizar-$1-i386-linux.tar
#$mkdir $1
cd MML
rm -rf abstr doc mml prel
rm *
tar xf ~/mizar.uwb.edu.pl/pub/system/i386-linux/mizar-$1-i386-linux.tar -C. 
#cd $1
tar xzf mizshare.tar.gz 
mkdir doc
tar xzf mizdoc.tar.gz -Cdoc
rm mizshare.tar.gz
rm mizdoc.tar.gz mizbin.tar.gz
git add .
git commit -a --author="Library Committee <mml@mizar.uwb.edu.pl>" -m $1
git tag $1
#cp -a mml miztmp
#cp Makefile  miztmp
#export MIZFILES=`pwd`
#cd miztmp
