SET NP=%NUANCE%\java\
SET NCP=%NP%/nsc.jar;%NP%\vcomsc.jar
SET JARPATH=%OAA_HOME%\lib\jar\
SET CP=%JARPATH%oaa2.jar;%JARPATH%antlr-oaa.jar;%JARPATH%concurrent-1.3.1.jar;%JARPATH%log4j-1.2.7.jar

javac -classpath %CP%;%NCP% InOutTextScore.java
javac -classpath %CP%;%NCP% InOutNuanceScore2.java
javac -classpath %CP%;%NCP% TTSAgent.java
pause

