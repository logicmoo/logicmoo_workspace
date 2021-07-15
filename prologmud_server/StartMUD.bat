@REM#!/bin/bash

@REM. setup_env.sh
SET LOGICMOO_WS="W:\opt\logicmoo_workspace"

@REM cls
REM  other options can be --www --irc --nonet --all 
c:\pf\swipl\bin\swipl --world --repl --telnet --no-hmud --no-cliop --no-www --no-irc --no-swish --no-elfinder -l run_mud_server.pl %*
@REM tput setaf 2 && echo -en 'Done ' && tput sgr0 && echo ""
