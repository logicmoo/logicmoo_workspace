#!/bin/bash -x

grep_return_code=0
[ ! -f /opt/logicmoo_workspace/nofederation ] && grep_return_code=1


if (( $grep_return_code == 0 )); then
   apt install -y *py*3.10*
   apt install -y python3-dev
   python -m venv . 
   . bin/activate
   #pip install -r ../../requirements.txt
   pip install spacy==3.1.4 nltk wasabi regex murmurhash cymem certifi urllib3 typing-extensions tqdm spacy-legacy smart-open pyparsing preshed numpy MarkupSafe joblib idna click charset-normalizer catalogue typer srsly requests pydantic packaging jinja2 blis thinc pathy
   python -m spacy download en_core_web_sm
   #pip install https://github.com/explosion/spacy-models/releases/download/en_core_web_lg-3.1.0/en_core_web_lg-3.1.0-py3-none-any.whl   
   #python -m spacy download en_core_web_lg
   
else 
   echo parser_spacy in federated mode
fi

while [ 1==1 ]
   do
      
   [ ! -f /opt/logicmoo_workspace/nofederation ] && grep_return_code=1

   if (( $grep_return_code == 0 )); then
      python3 ./parser_spacy.py -port 4096
      else 
         echo parser_spacy in federated mode
         sleep 1000
      fi
   done

