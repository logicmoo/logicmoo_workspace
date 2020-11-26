#!/bin/bash

JAVA_HOME=/unige/j2sdk1.4.2_02/j2se
JAVA="${JAVA_HOME}/bin/java"

if [ "${MACHINE_TYPE}" = "win32" ]; then
	export PATH_SEP=";"
else
	export PATH_SEP=:
fi

CLASSPATH="${REGULUS}/RegulusSpeechServer/runtime/regclient.jar${PATH_SEP}."

cd ../java

COMMAND="${JAVA} -Dregulus=${REGULUS} -classpath $CLASSPATH JDemo"

echo ${COMMAND}

${COMMAND}

