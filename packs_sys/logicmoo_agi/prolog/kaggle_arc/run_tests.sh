#!/bin/bash

swipl -l kaggle_arc -t $* | tee -i out.ansi
#echo cat output.ansi | ansi2html -l --style 'pre {font-family: Consolas}' > all_tasks.html

cat out.ansi |  ansi2html -a -W -u  > out.html

#cat output.ansi | ansi2html -l --style 'pre {font-family: Consolas}' > all_tasks.html
