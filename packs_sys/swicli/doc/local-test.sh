#!/bin/bash
cls
# killall -9 swipl
# fg
# fg

. ./mono_sysvars.sh
./make-linux.sh
% ./install-linux.sh

echo PATH=$PATH
echo MONO_PATH=$MONO_PATH
echo LD_LIBRARY_PATH=$LD_LIBRARY_PATH

# swipl -g "use_module(library(swicli))."
swipl --traditional -g "use_module(library(swicffi)),set_prolog_flag(double_quotes, string),cffi_tests."

