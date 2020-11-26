

rem Run the SICStus process, then start the Java side of the GUI using a specific version of Java
rem Edit the following line to say where to get Java from on your local system

rem set path=C:\Program Files\Java\jdk1.6.0_05\bin;%PATH%

start sicstus -l %REGULUS%\Prolog\regulus_server_for_java_gui.pl --goal "start_regulus_server_for_java_gui." -a 8066
sleep 5

start java -classpath "prologbeans.jar;RegulusGUI.jar;." -DREGULUS="%REGULUS%" RegulusGUI.RegulusGUI


