import sys
import select
from allennlp.predictors.predictor import Predictor
from allennlp.predictors.sentence_tagger import SentenceTaggerPredictor
import allennlp_models.tagging

sysargv = sys.argv
sysargv.pop(0)

predictor = Predictor.from_path("https://storage.googleapis.com/allennlp-public-models/structured-prediction-srl-bert.2020.12.15.tar.gz")
#sentence_tagger = SentenceTaggerPredictor.from_path("https://storage.googleapis.com/allennlp-public-models/structured-prediction-srl-bert.2020.12.15.tar.gz")

def dqt(str):
 return '"'+str.replace("\\","\\\\").replace('"','\\"')+'"'

def do_allen_srl(s):
 printit = predictor.predict(sentence=s)
 print("w2allen_srl(", end='',  flush=False)
 print(dqt(str(printit)), end='',  flush=False)
 print(").", flush=True)

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

print("", end='',  flush=True)

if cmdloop==1 or select.select([sys.stdin,],[],[],0.0)[0]:
 for line in sys.stdin: 
  do_allen_srl(line)
else:
 do_allen_srl(' '.join(sysargv))

