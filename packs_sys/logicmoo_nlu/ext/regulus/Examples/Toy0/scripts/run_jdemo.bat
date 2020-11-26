REM set classpath and java compiler
call java_settings.bat

cd ../java

%JAVA% -Dregulus=%REGULUS% -classpath %CLASSPATH% JDemo

pause
