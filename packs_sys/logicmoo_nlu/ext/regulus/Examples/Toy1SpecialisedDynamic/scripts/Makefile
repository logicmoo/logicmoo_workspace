
default:
	${MAKE} jit_package
	${MAKE} specialised_nuance

jit_package:
	nuance-compile ../Nuance/dummy English.America -o ../Generated/placeholder_jit_recogniser -enable_jit

specialised_nuance:
	sicstus -l build_nuance_files.pl

clean:
	rm -R ../Generated/*



