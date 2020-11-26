@echo off

rem Run the Java side of the GUI using a specific version of Java
rem Edit the following line to say where to get Java from on your local system

set path=C:\Program Files\Java\jdk1.6.0_05\bin;%PATH%

java -classpath "prologbeans.jar;RegulusGUI.jar;." -DREGULUS="%REGULUS%" RegulusGUI.RegulusGUI

pause
