#!/bin/bash
# cls ; kill -9 %1 %2 %3 %4 ; kill -9 %1 %2 %3 %4 ; swipl -f .swiplrc -g 'set_prolog_flag(runtime_testing,4)' -g "['attvar_existentials.pl']" -g "'\$set_source_module'(kbt), afr"
cls ; kill -9 %1 %2 %3 %4 ; kill -9 %1 %2 %3 %4 ; swipl -f .swiplrc -g 'set_prolog_flag(runtime_testing,4)' -g "['attvar_existentials.pl']" -g "set_fileAssertMt(kbt)" -g "afr"

