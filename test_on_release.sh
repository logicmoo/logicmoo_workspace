#!/bin/bash

DIR0="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
(
cd $DIR0
source ./logicmoo_env.sh -v

WAS_PWD=$PWD
FILTER=""

export keep_going="-k"

git switch master
git pull gitlab master
git pull github master
git pull origin master
#git push gitlab master
git push github master


#unset TESTING_TEMP

[ -z "$TESTING_TEMP" ] && [ -d "$(pwd)/test_results" ] && export TESTING_TEMP=$(pwd)/test_results/$(whoami)
[ -z "$TESTING_TEMP" ] && [ -d "${LOGICMOO_WS}/test_results" ] && export TESTING_TEMP=${LOGICMOO_WS}/test_results/$(whoami)
[ -z "$TESTING_TEMP" ] && export TESTING_TEMP=$(mktemp -d -t logicmoo_testing-$(date +%Y-%m-%d-%H-%M-%S)-XXXXXXXXXX)
export TESTING_TEMP
mkdir -p $TESTING_TEMP/   

JENKINS_BUILD_RESULT=/var/lib/jenkins/jobs/logicmoo_workspace/builds/${BUILD_NUMBER}

# source bin/build_xml.text > $JENKINS_BUILD_RESULT/build.xml

rm -f $TESTING_TEMP/???*.*???

export VERBOSITY="2>&1 | grep -2 -i 'WARN\|ERROR\|_file\|00\|fail\|pass'"
if [ "$1" == "-v" ]; then
  export VERBOSITY=""
  shift 1
fi

if [ "$1" == "new" ]; then
  FILTER="-mtime 0"
  shift 1
fi

if [ "$1" == "failed" ]; then
  #FILTER="-mtime 0"
  shift 1
fi

TEST_PARAMS="$*"
if [ -z "$TEST_PARAMS" ]; then 
  TEST_PARAMS="*0*.*"
  #TEST_PARAMS="*f*_01.p*"
fi



echo -e "Running release (all) tests\nTESTING_TEMP=$TESTING_TEMP\n( cd $PWD ; $BASH_SOURCE $TEST_PARAMS )"

eval "lmoo-make $VERBOSITY"

TEST_DIRS=`find -mindepth 2 $FILTER -type f -name "test_on_*.sh" -exec dirname {} \;`
echo TEST_DIRS=$TEST_DIRS
DIRS_SORTED=`find $TEST_DIRS -maxdepth 0 -type d -printf "%T+ %p\n" | sort -r -u | cut -d " " -f 2`
echo DIRS_SORTED=$DIRS_SORTED



for dirname in "${DIRS_SORTED[@]}"; do
    echo -e "$dirname\n"
    find $dirname -maxdepth 1 $FILTER -name "test_on_*.sh" -execdir {} "$TEST_PARAMS" $VERBOSITY \;
done

# Generate JUnit Results
( 
echo "<testsuites>" 
sed -r "s/\x1B\[(([0-9]{1,2})?(;)?([0-9]{1,2})?)?[m,K,H,f,J]//g" ${TESTING_TEMP}/?*-junit.xml | sed -e "s|<testsuites>||g" -e 's|<?xml version="1.0" encoding="utf-8"?>||g' -e "s|</testsuites>||g"
echo -e "\n</testsuites>\n\n\n" 
) | iconv -c -t utf-8  > ${TESTING_TEMP}/junit-all.xml

# Generate Html Reports
( echo junit-viewer --results=${TESTING_TEMP}/junit-all.xml --save=${TESTING_TEMP}/junit-all.html )  ; /bin/true
( junit2html ${TESTING_TEMP}/junit-all.xml ${TESTING_TEMP}/junit2html-all.html )  ; /bin/true


cd $TESTING_TEMP/ && find . -print | zip jenkin_results_${BUILD_NUMBER}.zip -@ 2>&1 > /dev/null

)
