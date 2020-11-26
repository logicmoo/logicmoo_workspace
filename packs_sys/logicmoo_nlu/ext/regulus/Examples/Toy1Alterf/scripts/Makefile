

include Makefile.in

default:
	${MAKE} specialised_nuance
	${MAKE} recogniser

specialised_nuance:
	sicstus -l build_nuance_files.pl

recogniser:
	nuance-compile ../Generated/toy1_specialised_recogniser English.America -auto_pron -dont_flatten 	

# ALTERF stuff

train_alterf:
	(unset SHELL ; sicstus -l train_alterf.pl)	

test_alterf:
	${MAKE} test_alterf_patterns
	(unset SHELL ; sicstus -l test_alterf.pl)

test_alterf_patterns:
	(unset SHELL ; sicstus -l test_alterf_patterns.pl)
	(unset SHELL ; sicstus -l test_alterf_surface_patterns.pl)

clean:
	rm -R ../Generated/*



