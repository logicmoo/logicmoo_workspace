#!/bin/bash

swipl -l kaggle_arc -t $* | tee -i output.ansi
#echo cat output.ansi | ansi2html -l --style 'pre {font-family: Consolas}' > all_tasks.html

cat output.ansi |  ansi2html -a -W -u  > all_tasks.html

#cat output.ansi | ansi2html -l --style 'pre {font-family: Consolas}' > all_tasks.html
