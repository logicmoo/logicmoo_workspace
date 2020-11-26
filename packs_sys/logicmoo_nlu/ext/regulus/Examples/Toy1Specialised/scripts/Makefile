

include Makefile.in

default:
	${MAKE} specialised_nuance
	${MAKE} recogniser
	${MAKE} french_generation

multi_ling:
	${MAKE} specialised_nuance_english_multi_ling
	${MAKE} specialised_nuance_french_multi_ling
	${MAKE} recogniser_english_multi_ling
	${MAKE} recogniser_french_multi_ling

specialised_nuance:
	sicstus -l build_nuance_files.pl

specialised_nuance_english_multi_ling:
	sicstus -l build_nuance_files_english_multi_ling.pl

specialised_nuance_french_multi_ling:
	sicstus -l build_nuance_files_french_multi_ling.pl

recogniser:
	nuance-compile ../Generated/toy1_specialised_recogniser English.America -auto_pron -dont_flatten 	

recogniser_english_multi_ling:
	nuance-compile ../Generated/toy1_specialised_eng_recogniser English.America -auto_pron -dont_flatten 	

recogniser_french_multi_ling:
	nuance-compile ../Generated/french_recogniser French -auto_pron -dont_flatten 	

bnf:
	sicstus -l convert_gsl_to_bnf.pl

recogniser_corrections:
	sicstus -l build_nuance_files_corrections.pl
	nuance-compile ../Generated/recogniser_corrections\
           English.America -auto_pron -dont_flatten 

french_generation:
	sicstus -l build_french_generator.pl

ellipsis_classes:
	sicstus -l compile_ellipsis_classes.pl

test:
	sicstus -l test_dialogue_and_translation.pl 

toy1:
	$(JAVAC) -classpath $(CLASSPATH) ../java/Toy1Specialisedapp.java

toy1_Xlint:
	$(JAVAC) -classpath $(CLASSPATH) -Xlint ../java/Toy1Specialisedapp.java

run_toy1:
	cd ../java && $(JAVA) -Dregulus=${REGULUS} -classpath $(CLASSPATH)  Toy1Specialisedapp

run_toy1_debug:
	cd ../java && $(JAVA) -Dregulus=${REGULUS} -classpath $(CLASSPATH)  Toy1Specialisedapp -d

run_toy1_verbose:
	cd ../java && $(JAVA) -verbose -Dregulus=${REGULUS} -classpath $(CLASSPATH)  Toy1app
clean:
	rm -R ../Generated/*



