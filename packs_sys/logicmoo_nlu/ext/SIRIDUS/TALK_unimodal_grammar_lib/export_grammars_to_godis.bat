@setlocal

@REM delete old gfc and gfr files 
del /s *.gfc
del /s *.gfr

@REM compile deLux grammars:
set GRPATH=GF_GoDiS\Domain\deLux
set APPPATH=delux

pushd %GRPATH%

gf < compile_sys_domain_lamps.gfscript
gf < compile_usr_domain_lamps.gfscript

popd

copy %GRPATH%\*.cfgm %APPPATH%\Grammars
copy %GRPATH%\*.gfcm %APPPATH%\Grammars
copy %GRPATH%\*.grammar %APPPATH%\Grammars
copy %GRPATH%\*.properties %APPPATH%\Grammars

@REM compile djgodis grammars:
set GRPATH=GF_GoDiS\Domain\DJGoDiS
set APPPATH=djgodis

pushd %GRPATH%

gf < compile_sys_domain_player.gfscript
gf < compile_usr_domain_player.gfscript

popd

copy %GRPATH%\*.cfgm %APPPATH%\Grammars
copy %GRPATH%\*.gfcm %APPPATH%\Grammars
copy %GRPATH%\*.grammar %APPPATH%\Grammars
copy %GRPATH%\*.properties %APPPATH%\Grammars


@endlocal
pause
