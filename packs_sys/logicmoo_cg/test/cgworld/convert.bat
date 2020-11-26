setlocal
set classpath=.\WEB-INF\classes;%classpath%
java larflast.tools.Convert %1 %2 %3 %4 %5
endlocal