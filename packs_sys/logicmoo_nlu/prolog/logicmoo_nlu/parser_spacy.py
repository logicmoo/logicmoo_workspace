import spacy
import sys
import select
from nltk import Tree

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
  return qt(""+str(tokenhead.lower_) +'_' + str(tokenhead.i+1))

def do_spacy(text0):
 text0 = text0.strip(' \t\n\r')
 doc = nlp(text0)    
 print("w2spacey([",  end='',  flush=False)
 for token in doc:
  print(f"w({qt(token.lower_)},[pos({qt(token.tag_)}),loc({token.i+1}),sem({qt(token.lemma_)}),{tense(token.tag_)}txt({dqt(token.text)}),prob({((- token.prob)/20)}),node({nodestr(token)})]),", end='',  flush=False)
 print("seg([", end='',  flush=False)
 for token in doc:
  print(f"dep_tree({(token.dep_.lower())},{nodestr(token.head)},{nodestr(token)}), " , end='',  flush=False) # ,{tense(token.head.tag_)},{qt(token.head.lemma_)},{qt(token.lemma_)}
 #if 1==0: for chunk in doc.noun_chunks: print(f"vp_{chunk.root.dep_}({qt(chunk.root.head.text)},{qt(chunk.root.text)},\"{chunk}\"),")
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
if len(sysargv) > 0 and sysargv[0]=='-m':
  sysargv.pop(0)
  model = sysargv.pop(0);
nlp = spacy.load(model)

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

