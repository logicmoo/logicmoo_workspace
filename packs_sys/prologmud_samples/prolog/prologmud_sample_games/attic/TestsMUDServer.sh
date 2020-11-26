@ECHO ON

SETLOCAL ENABLEDELAYEDEXPANSION
PUSHD

SET SWIPLWIN=false
SET KILLATEND=false
SET EXITATEND=false
SET USECHOICE=true


IF "%VSLICKBIN%"=="" (
  cd .
) ELSE (
  cd .
)



if "%1"=="build" (

	SET KILLATEND=true

) ELSE (
    echo The variable contains %*
)



IF "%SWIPLWIN%"=="true" (
    cd .
) else (
    cd runtime
)

@rem start mud script 
SET MUDSCRIPT=src_incoming/run_tests.pl

:startMUD
rem call 	pskill -t swipl-win
rem call 	pskill -t swipl

if "%SWIPLWIN%"=="true" (

	exit
	start /WAIT %MUDSCRIPT%
    if "%USECHOICE%"=="true" (
    	CHOICE /T 10 /C YN /CS /D Y /M  "RESTART MUD"
    	IF ERRORLEVEL 1 goto startMUD
    ) else (
        goto :eof
    )

) else (

	call swipl -f run_debug.pl

	IF "%EXITATEND%"=="true" (
        goto :eof
	)

	IF "%KILLATEND%"=="true" (
        pskill-t ntpinit
     	exit
        goto :eof
	)

    if "%USECHOICE%"=="true" (
    	CHOICE /T 10 /C YN /CS /D Y /M  "RESTART MUD"
    	IF ERRORLEVEL 1 goto startMUD
    )

	IF "%EXITATEND%"=="true" (
        goto :eof
	)

    pskill-t ntpinit
    goto :eof

)


POPD


