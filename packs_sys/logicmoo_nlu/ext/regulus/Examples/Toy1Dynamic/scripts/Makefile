
include Makefile.in

default:
	nuance-compile ../Nuance/dummy English.America -o ../Generated/recogniser_dynamic -enable_jit

jdemo:
	$(JAVAC) -classpath $(CLASSPATH) ../java/JDemo.java

run_jdemo:
	cd ../java && $(JAVA) -Dregulus=${REGULUS} -classpath $(CLASSPATH) JDemo 0

run_jdemo_nbest:
	cd ../java && $(JAVA) -Dregulus=${REGULUS} -classpath $(CLASSPATH) JDemo 1



