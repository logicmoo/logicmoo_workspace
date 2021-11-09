import spacy
import sys
import select
from nltk import Tree
# from pybart.api import *

def to_nltk_tree(node):
    if node.n_lefts + node.n_rights > 0:
        return Tree(node.orth_, [to_nltk_tree(child) for child in node.children])
    else:
        return node.orth_


# single wuote prolog atoms
def qt(str):
 if ("'" in str or "\\" in str or " " in str or '"' in str or '.' in str or '?' in str or ',' in str): 
  return "'"+str.replace("\\","\\\\").replace("'","\\'")+"'"
 #if (str.lower()==str and str.upper()!=str): return str
 return "'"+str+"'"

def dqt(str):
 return '"'+str.replace("\\","\\\\").replace('"','\\"')+'"'

def tense(str):
 if (str=="VBZ"): return "tense(present),"
 if (str=="VBP"): return "tense(present),"
 if (str=="VBD"): return "tense(past),"
 if (str=="VBG"): return "tense(active)," 
 if (str=="VBN"): return "tense(past),"
 if (str=="PRP "): return "form(personal),"
 if (str=="NNS"): return "form(pl),"
 if (str=="NNPS"): return "form(pl),"
 if (str=="NN"): return "form(sg),"
 if (str=="NNP"): return "form(sg),"
 return "" # "unused('"+str+"'),"

def nodestr(tokenhead):
  return "n("+qt(str(tokenhead.lower_))+ ","+ str(tokenhead.i+1)+")"

def nodestr2(tokenhead):
  return qt(str(tokenhead.lower_) +'_' + str(tokenhead.i+1))

def maybe_prob(prob):
  if prob== -20.0: return "" # the default
  return "prob(" + str(- (prob/20))+")"

def do_spacy(text0):
 text0 = text0.strip(' \t\n\r')
 doc = nlp(text0)    
 print("w2spacy([",  end='',  flush=False)
 for token in doc:
  print(f"w({qt(token.lower_)},[spos({qt(token.tag_.lower())}),loc({token.i+1}),root({qt(token.lemma_)}),{tense(token.tag_)}txt({dqt(token.text)}),{maybe_prob(token.prob)}", end='',  flush=False)
  for child in doc:
   if child.head==token:
    print(f"dep_child({(child.dep_.lower())},{nodestr(child)}), " , end='',  flush=False) # ,{tense(token.head.tag_)},{qt(token.head.lemma_)},{qt(token.lemma_)}
  print(f"dep_parent({(token.dep_.lower())},{nodestr(token.head)}),node({nodestr(token)})]),", end='',  flush=False)
 print("span([", end='',  flush=False)
 for chunk in doc.noun_chunks: 
  print(f"span([seg({chunk.start+1},{chunk.end}),phrase('NP'),size({chunk.end-chunk.start}),childs(0),type({qt(chunk.root.dep_)}),head({nodestr(chunk.root.head)}),target({nodestr(chunk.root)}),text({dqt(chunk.text)})]),")

 print("[]])", end='',  flush=False)
 if show_comment==1:
  print(',comment("')
  try: 
   [to_nltk_tree(sent.root).pretty_print() for sent in doc.sents]
  except Exception as e:
   print('An exception occurred: {}'.format(e))
  print('")')
 print(']).',  flush=True)
 return



print("")
sysargv = sys.argv
sysargv.pop(0)


model = "en_core_web_lg"
#model = "en_ud_model_sm"
#model = "en_ud_model_trf"
if len(sysargv) > 0 and sysargv[0]=='-m':
  sysargv.pop(0)
  model = sysargv.pop(0);
nlp = spacy.load(model)
# Add BART converter to spaCy's pipeline
#nlp.add_pipe("pybart_spacy_pipe", last="True", config={'remove_extra_info':True}) # you can pass an empty config for default behavior, this is just an example

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
  do_spacy(line)
else:
 do_spacy(' '.join(sysargv))

