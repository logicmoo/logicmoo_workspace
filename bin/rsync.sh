#!/bin/bash

rsync -avht --update  --exclude "~*" --exclude "*~" --exclude ".git" --files-from=sortme.txt /home/prologmud_server/lib/swipl/pack/ /mnt/aindilis/opt/logicmoo_workspace/pack/ 



