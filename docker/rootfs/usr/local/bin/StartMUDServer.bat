@rem start mud script 



cd pack\prologmud\runtime


:startMUD



@rem start /wait swipl-win -L32G -G32G -T32G -f run_mud_server.pl
@rem swipl -L32G -G32G -T32G -f run_mud_server.pl
"C:\Program Files\swipl\bin\swipl" -L32G -G32G -T32G -f init_mud_server.pl

CHOICE  /T 2 /C YN /CS /D Y /M  "RESTART MUD"



IF ERRORLEVEL 1 goto :startMUD



