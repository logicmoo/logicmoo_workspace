import sys
import select
from allennlp.predictors.predictor import Predictor
import allennlp_models.tagging
from overrides import overrides

from allennlp.common.util import JsonDict
from allennlp.data import DatasetReader, Instance
#from allennlp.data.tokenizers.word_splitter import SpacyWordSplitter
from allennlp.data.tokenizers.spacy_tokenizer import SpacyTokenizer
from allennlp.models import Model
from allennlp.predictors.predictor import Predictor



sysargv = sys.argv
sysargv.pop(0)

predictor = Predictor.from_path("https://storage.googleapis.com/allennlp-public-models/structured-prediction-srl-bert.2020.12.15.tar.gz")

def do_allen(s):
 printit = predictor.predict(sentence=s)
 print(printit)

verbose = 0
if len(sysargv) > 0 and sysargv[0]=='-v':
  sysargv.pop(0)
  verbose = 1

show_comment = 0
if len(sysargv) > 0 and sysargv[0]=='-sc':
  sysargv.pop(0)
  show_comment = 1
if len(sysargv) > 0 and sysargv[0]=='-nc':
   sysargv.pop(0)
   show_comment = 0

cmdloop = 0
if len(sysargv) > 0 and sysargv[0]=='-cmdloop':
   sysargv.pop(0)
   cmdloop = 1

if verbose==1: print("% " + sysargv)

if cmdloop==1 or select.select([sys.stdin,],[],[],0.0)[0]:
 for line in sys.stdin: 
  do_allen(line)
else:
 do_allen(' '.join(sysargv))

