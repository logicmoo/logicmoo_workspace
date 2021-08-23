#!/bin/bash

[ -z "$TESTING_TEMP" ] && [ -d "$(pwd)/test_results" ] && export TESTING_TEMP=$(pwd)/test_results/$(whomai)
#[ -z "$TESTING_TEMP" ] && export TESTING_TEMP=$(mktemp -d -t logicmoo_testing-$(date +%Y-%m-%d-%H-%M-%S)-XXXXXXXXXX)

export TESTING_TEMP

export GLOB="$*"
[ -z "$GLOB" ] && GLOB="*_01.*"
export TEST_STEM=Report-$(echo "${GLOB}-Units" | sed -e "s/[*]/vSTARv/g" -e "s/[?]/vQUESTv/g" -e "s/[.]/vDOTv/g" -e "s/[^a-Z0-9_]/-/g" -e "s/--/-/g" -e "s/-/-/g"  -e "s/--/-/g" )
echo "<!-- TEST_STEM=${TEST_STEM} -->"
export TEST_STEM_PATH=$TESTING_TEMP/$TEST_STEM
echo "<!-- TEST_STEM_PATH=${TEST_STEM_PATH} -->"


export CAPTURE_TEST_ANSI=$TEST_STEM_PATH.ansi
echo -e "<!-- Running release (all) tests\n ( cd $PWD ; $BASH_SOURCE $GLOB) > $CAPTURE_TEST_ANSI -->"

mkdir -p $TESTING_TEMP/

cat /dev/null > $CAPTURE_TEST_ANSI

# Run tests for JUnit Results
( test_prolog.sh -k "$GLOB" ) 2>&1 | tee -a $CAPTURE_TEST_ANSI

junitCombined=$TEST_STEM_PATH-junitCombined

# Generate JUnit Results
echo "<testsuites>" > $junitCombined
sed -r "s/\x1B\[(([0-9]{1,2})?(;)?([0-9]{1,2})?)?[m,K,H,f,J]//g" $TESTING_TEMP/?*-junit.xml | sed -e "s|<testsuites>||g" -e "s|</testsuites>||g" >> $junitCombined
echo "</testsuites>" >> $junitCombined

PATH=~/.npm-packages/bin:$PATH

echo "<!-- "
# Generate Html Reports
(cat $CAPTURE_TEST_ANSI | ansi2html.sh > $TEST_STEM_PATH-README.html)  ; /bin/true
#npm install -g junit-viewer
(junit-viewer --results=$junitCombined --save=$TEST_STEM_PATH-junit-viewer.html &> $TEST_STEM_PATH-junit-viewer.debug.html )  ; /bin/true
#npm install -g xunit-viewer
(xunit-viewer --results $junitCombined -o $TEST_STEM_PATH-xunit-viewer.html )  ; /bin/true
#pip3 install junit2html
(junit2html $junitCombined $TEST_STEM_PATH-junitCombined.html) ; /bin/true
echo "TEST_STEM_PATH=${TEST_STEM_PATH}"
ls -1 $TESTING_TEMP*
echo "-->"
