

include Makefile.in

default:
	${MAKE} recogniser
	${MAKE} interlingua_checking
	${MAKE} french_generation
	${MAKE} swedish_generation

recogniser:
	sicstus -l build_nuance_files.pl
	nuance-compile ../Generated/recogniser\
           English.America -auto_pron -dont_flatten 

french_recogniser:
	sicstus -l build_french_nuance_files.pl
	nuance-compile ../Generated/french_recogniser\
           French -auto_pron -dont_flatten 

recogniser_corrections:
	sicstus -l build_nuance_files_corrections.pl
	nuance-compile ../Generated/recogniser_corrections\
           English.America -auto_pron -dont_flatten 

interlingua_checking:
	sicstus -l build_interlingua_generator.pl

french_generation:
	sicstus -l build_french_generator.pl

swedish_generation:
	sicstus -l build_swedish_generator.pl

ellipsis_classes:
	sicstus -l compile_ellipsis_classes.pl

test:
	sicstus -l test_dialogue_and_translation.pl 

test_speech:
	sicstus -l test_dialogue_speech.pl 

toy1:
	$(JAVAC) -classpath $(CLASSPATH) ../java/Toy1app.java

toy1_Xlint:
	$(JAVAC) -classpath $(CLASSPATH) -Xlint ../java/Toy1app.java

run_toy1:
	cd ../java && $(JAVA) -Dregulus=${REGULUS} -classpath $(CLASSPATH)  Toy1app

run_toy1_debug:
	cd ../java && $(JAVA) -Dregulus=${REGULUS} -classpath $(CLASSPATH)  Toy1app -d

run_toy1_verbose:
	cd ../java && $(JAVA) -verbose -Dregulus=${REGULUS} -classpath $(CLASSPATH)  Toy1app
clean:
	rm -R ../Generated/*



