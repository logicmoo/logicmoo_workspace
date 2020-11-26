

include Makefile.in

default:
	${MAKE} specialised_nuance
	${MAKE} recogniser
	${MAKE} french_generation

text_only:
	${MAKE} specialised_nuance
	${MAKE} french_generation

specialised_nuance:
	sicstus -l build_nuance_files.pl

recogniser:
	nuance-compile ../Generated/toy1_specialised_recogniser English.America -auto_pron -dont_flatten 	

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



