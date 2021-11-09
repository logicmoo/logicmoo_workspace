import sys
import select

from flair.data import Sentence
from flair.models import SequenceTagger
from flair.models import MultiTagger
import os, time
moddate = os.stat(os.path.realpath(__file__))[8] # there are 10 attributes this call returns and you want the next to last
originalsysargv = sys.argv

sysargv = sys.argv
sysargv.pop(0)

#predictor = Predictor.from_path("https://storage.googleapis.com/allennlp-public-models/structured-prediction-srl-bert.2020.12.15.tar.gz")
#sentence_tagger = SentenceTaggerPredictor.from_path("https://storage.googleapis.com/allennlp-public-models/structured-prediction-srl-bert.2020.12.15.tar.gz")
# single wuote prolog atoms
def qt(s):
 str = s
 if ("'" in str or "\\" in str or " " in str or '"' in str or '.' in str or '?' in str or ',' in str): 
  return "'"+str.replace("\\","\\\\").replace("'","\\'")+"'"
 #if (str.lower()==str and str.upper()!=str): return str
 return "'"+str+"'"

def dqt(s):
 return '"'+str(s).replace("\\","\\\\").replace('"','\\"')+'"'

def refresh_on_file_mod():
   if moddate == os.stat(os.path.realpath(__file__))[8]:
        return
   with open(os.path.dirname(__file__) + os.sep + 'refresh.py', 'r') as f:    \
    exec(compile(f.read().replace('__BASE__',                              \
        os.path.basename(__file__).replace('.py', '')).replace('__FILE__', \
            __file__), __file__, 'exec'))

def do_flair(s):
 if s=="+EXIT PYTHON":
   sys.exit(0)
 refresh_on_file_mod()
 # make a sentence
 sentence = Sentence(s)

 # run NER over sentence
 tagger.predict(sentence)
 print("w2flair([", end='',  flush=False)
 sep="";
 for i in sentence:
    print(sep, end='',  flush=False)
    print(f"w({qt(i.text.lower())},[upos({qt(i.labels[0].value.lower())}),fner({qt(i.labels[1].value.lower())}),fn({qt(i.labels[2].value.lower())}),neg({qt(i.labels[3].value.lower())}),text({dqt(i.text)})])", end='', flush=False)
    sep=","
 print("]).\n", end='', flush=False)
 print("", end='',  flush=True)
 print("", end='',  flush=True)

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
else:
 if select.select([sys.stdin,],[],[],0.0)[0]:
  cmdloop = 1


# load tagger for POS and NER 
tagger = MultiTagger.load(['upos', 'ner-large', 'frame', 'negation-speculation'])

if verbose==1: print("% " + sysargv)

print("", end='',  flush=True)

if cmdloop==1: 
 print("\n cmdloop_Ready. \n", end='',  flush=True)

if cmdloop==1:
 for line in sys.stdin: 
  do_flair(line)
  if cmdloop==0: 
   sys.exit(0)
  refresh_on_file_mod()
else:
 sentence=' '.join(sysargv)
 if sentence=="": sentence="George Washington went to Washington."
 do_flair(sentence)

