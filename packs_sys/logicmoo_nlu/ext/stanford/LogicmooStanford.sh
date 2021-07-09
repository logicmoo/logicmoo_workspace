#!/bin/bash

echo "${BASH_SOURCE[0]} Starting..."

if [[ ! -f deps/stanford-corenlp-4.2.0-models.jar ]]; then
(
 echo "${BASH_SOURCE[0]} Downloading..."
 cd deps
 set +x +e
 wget -N http://nlp.stanford.edu/software/stanford-corenlp-4.2.0-models.jar
 wget -N http://nlp.stanford.edu/software/stanford-corenlp-4.2.0-models-english.jar
 wget -N http://nlp.stanford.edu/software/stanford-corenlp-4.2.0-models-english-kbp.jar
)
fi

#apt remove adoptopenjdk-8-hotspot


#export OPTS="edu.stanford.nlp.pipeline.StanfordCoreNLPServer -port 4090 -preload -status_port 4091 -timeout 15000 $@"

#export OPTS="edu.stanford.nlp.pipeline.StanfordCoreNLPServer -port 4090 -timeout 15000 -uriContext /stanford"

export OPTS="-Xmx9G -Xms6G -Djava.util.logging.config.file=./logging.properties edu.stanford.nlp.pipeline.StanfordCoreNLPServer -port 4090 -timeout 15000"

cd deps
cd $(readlink $PWD)
git lfs checkout .

# -annotators tokenize,quote,ssplit,pos,lemma,ner,truecase,parse,hcoref,relation
echo cd `pwd`
echo java -cp "*" $OPTS
java -cp "*" $OPTS

sleep 2
# curl --data "How are you today?" 'http://localhost:4090/stanford/?properties={%22annotators%22%3A%22tokenize%2Cssplit%2Cpos%2Cdepparse%22%2C%22outputFormat%22%3A%22conllu%22}'
# curl --data "How are you today?" 'http://localhost:4090/?properties={%22annotators%22%3A%22quote%2Ctokenize%2Cssplit%2Cpos%2Clemma%2Cdepparse%2Cnatlog%2Ccoref%2Cdcoref%2Cner%2Crelation%2Cudfeats%22%2C%22outputFormat%22%3A%22json%22}'
# curl --data "The Norwegian man lives in the first house." 'http://localhost:4090/?properties={%22annotators%22%3A%22quote%2Ctokenize%2Cssplit%2Cpos%2Clemma%2Cdepparse%2Cnatlog%2Ccoref%2Cdcoref%2Cner%2Crelation%2Cudfeats%22%2C%22outputFormat%22%3A%22json%22}'
echo "${BASH_SOURCE[0]} Exiting..."


