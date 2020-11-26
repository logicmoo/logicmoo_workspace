

include Makefile.in

default:
	${MAKE} nuance_grammar
	${MAKE} n85_recogniser
	${MAKE} n9_recogniser

nuance_grammar:
	sicstus -l build_nuance_files.pl

n85_recogniser:
	nuance-compile ../Generated/recogniser English.America

n9_recogniser:
	uconv -f ISO-8859-1 -t UTF-8 ../Generated/recogniser.grammar > ../Generated/recogniser_n9.grammar
	convert_gsl -file ../Generated/recogniser_n9.grammar -lang en-US 

clean:
	rm ../Generated/*


