#!/bin/bash

LIB=flair

pip3 list | grep $LIB
grep_return_code=$?
echo grep_return_code=$grep_return_code

[ ! -f /opt/logicmoo_workspace/nofederation ] && grep_return_code="1"

while [ 1==1 ]
   do
      if [ grep_return_code == "0" ]; then
      python3 ./parser_$LIB.py -port 4095
      else 
         sleep 10000
      fi
   done

