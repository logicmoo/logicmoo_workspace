#!/bin/bash


pip3 list | grep allennlp
grep_return_code=$?

[ ! -f /opt/logicmoo_workspace/nofederation ] && grep_return_code="1"

echo skip_parser_allen_srl=$grep_return_code

while [ 1==1 ]
   do
      if [ grep_return_code == "0" ]; then
         python3 ./parser_allen_srl.py -port 4097
      else 
         sleep 10000
      fi
   done

