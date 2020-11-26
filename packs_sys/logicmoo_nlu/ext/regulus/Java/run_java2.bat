@echo off

rem Run the Java side of the GUI

echo Starting Java Regulus GUI
echo You need to start Prolog server separately 
echo by invoking the script "run_prolog.bat" in this directory

rem java -classpath "%SPINSTALLDIR%\bin\prologbeans.jar;." RegulusGUI.RegulusGUI
rem java -classpath "%SPINSTALLDIR%\bin\prologbeans.jar;RegulusGUI\RegulusGUI.jar;." RegulusGUI.RegulusGUI
rem java -classpath "%SPINSTALLDIR%\bin\prologbeans.jar;RegulusGUI\RegulusGUI.jar;." -DREGULUS="%REGULUS%" RegulusGUI.RegulusGUI
rem java -classpath "%SPINSTALLDIR%\bin\prologbeans.jar;RegulusGUI.jar;." -DREGULUS="%REGULUS%" RegulusGUI.RegulusGUI
java -classpath "prologbeans.jar;RegulusGUI.jar;." -DREGULUS="%REGULUS%" RegulusGUI.RegulusGUI 8067

pause
