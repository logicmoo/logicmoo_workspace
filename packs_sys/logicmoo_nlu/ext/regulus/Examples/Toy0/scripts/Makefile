
include Makefile.in

default:
	sicstus -l build_nuance_files.pl
	nuance-compile ../Generated/recogniser English.America

hand_coded:
	nuance-compile ../Nuance/hand_coded_toy0 English.America

jdemo:
	$(JAVAC) -classpath $(CLASSPATH) ../java/JDemo.java

run_jdemo:
	cd ../java && $(JAVA) -Dregulus=${REGULUS} -classpath $(CLASSPATH) JDemo

clean:
	rm *~


