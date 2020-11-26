SET REGULUS=d:/Regulus

cd d:\Regulus\Examples\Calendar\java\

rem set CLASSPATH=%NUANCE%\java\nsc.jar;%NUANCE%\java\vcomsc.jar

java -classpath %CLASSPATH%;%REGULUS%/RegulusSpeechServer/runtime/regclient.jar;%REGULUS%/Examples/Calendar/java/calendar.jar;. issco.calendar.gui.Calendar 
