#!/bin/sh
echo $0
BASEDIR=`dirname $0`
echo $BASEDIR
cd $BASEDIR
exec java -Xdock:name=PrologPlusCG -Xdock:icon=../Resources/PPCG.icns -classpath ../Resources/Java/classes PrologPlusCG.PrologPlusCG
