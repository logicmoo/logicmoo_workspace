REM set classpath and java compiler
call java_settings.bat

%JAVAC% -classpath %CLASSPATH% ../java/JDemo.java

pause