
SET JARPATH=%OAA_HOME%/lib/jar/
SET CP=.;%JARPATH%oaa2.jar;%JARPATH%antlr-oaa.jar;%JARPATH%concurrent-1.3.1.jar;%JARPATH%log4j-1.2.7.jar

java -classpath %CP% InOutTextScore

pause
