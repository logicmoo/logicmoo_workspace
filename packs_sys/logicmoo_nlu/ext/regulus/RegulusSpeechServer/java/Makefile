
FIND = find

ifeq ($(OSTYPE), cygwin)
#
# Add windows specific stuff
#
CLASSPATH := .
JAVAC := javac
else
#
# Add unix specific stuff here
#
JAVAC := javac
CLASSPATH := .
endif

PACKAGE := net/sf/regulus
CLASSES  := $(patsubst %.java,%.class,$(wildcard ${PACKAGE}/*.java)) $(patsubst %.java,%.class,$(wildcard ${PACKAGE}/interpretation/*.java))


OUTPUT_JAR := regclient.jar


%.class: %.java
	echo ${CLASSES}
	${JAVAC} -Xlint -classpath "${CLASSPATH}" $<

default: jcc $(CLASSES) jar

what:
	@echo "Usage: make [jar|clean]"

jcc: interpretationGenerator.jj
	javacc -DEBUG_PARSER -OUTPUT_DIRECTORY=net/sf/regulus/interpretation $<

jar:
	jar cf $(OUTPUT_JAR) net
	cp $(OUTPUT_JAR) ../runtime

clean:
	$(FIND) . -name '*.class' -exec rm -f {} \;
	$(FIND) . \( -name '*~' -o -name '#*' \) -exec rm -f {} \;
	rm -rf $(OUTPUT_JAR)

