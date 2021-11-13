#!/usr/bin/env python3
import socket
import selectors
import types
#-*- coding:utf-8 -*-  
import sys, select, socket
import os, time

moddate = os.stat(os.path.realpath(__file__))[8] # there are 10 attributes this call returns and you want the next to last
originalsysargv = sys.argv

sysargv = sys.argv
sysargv.pop(0)

# single quote prolog atoms
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
  
import spacy
from nltk import Tree
model = "en_core_web_lg" #model = "en_ud_model_sm" #model = "en_ud_model_trf"
if len(sysargv) > 0 and sysargv[0]=='-m':
  sysargv.pop(0)
  model = sysargv.pop(0);
nlp = spacy.load(model)
# Add BART converter to spaCy's pipeline #nlp.add_pipe("pybart_spacy_pipe", last="True", config={'remove_extra_info':True}) # you can pass an empty config for default behavior, this is just an example
def do_nlp_proc(text0):
 text0 = text0.strip(' \t\n\r')
 doc = nlp(text0)    
 output = "w2spacy(["
 for token in doc:
  output = output + f"w({qt(token.lower_)},[spos({qt(token.tag_.lower())}),loc({token.i+1}),root({qt(token.lemma_)}),{tense(token.tag_)}txt({dqt(token.text)}),{maybe_prob(token.prob)}"
  for child in doc:
   if child.head==token:
    output = output + f"dep_child({(child.dep_.lower())},{nodestr(child)}), "  # ,{tense(token.head.tag_)},{qt(token.head.lemma_)},{qt(token.lemma_)}
 output = output + f"dep_parent({(token.dep_.lower())},{nodestr(token.head)}),node({nodestr(token)})]),"
 output = output + "span(["
 for chunk in doc.noun_chunks: 
  output = output + f"span([seg({chunk.start+1},{chunk.end}),phrase('NP'),size({chunk.end-chunk.start}),childs(0),type({qt(chunk.root.dep_)}),head({nodestr(chunk.root.head)}),target({nodestr(chunk.root)}),text({dqt(chunk.text)})]),"

 output = output + "[]])"
 if show_comment==1:
  output = output + ',comment("'
  try: 
   [to_nltk_tree(sent.root).pretty_print() for sent in doc.sents]
  except Exception as e:
   output = output + 'An exception occurred: {}'.format(e)
  output = output + '")'
 output = output + ']).'
 return output

def to_nltk_tree(node):
    if node.n_lefts + node.n_rights > 0:
        return Tree(node.orth_, [to_nltk_tree(child) for child in node.children])
    else:
        return node.orth_
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



port=0
if len(sysargv) > 0 and sysargv[0]=='-port':
   sysargv.pop(0)
   port = sysargv.pop(0)

if verbose==1: print("% " + sysargv)

print("", end='',  flush=True)

def service_connection(key, mask):
    sock = key.fileobj
    data = key.data
    if mask & selectors.EVENT_READ:
        recv_data = sock.recv(1024)  # Should be ready to read
        if recv_data:
            data.outb += recv_data            
        else:
            print('closing connection to', data.addr)
            sel.unregister(sock)
            sock.close()
    if mask & selectors.EVENT_WRITE:
        if data.outb:
            print('replying to', repr(data.outb), 'to', data.addr)
            recv = str(data.outb.decode())
            size = len(recv)
            print("got: ",recv,size);
            data.outb = data.outb[size:]
            # Should be ready to write
            sock.send((do_nlp_proc(recv)+"\n").encode())
            

def accept_wrapper(sock):
    conn, addr = sock.accept()  # Should be ready to read
    print('accepted connection from', addr)
    conn.setblocking(False)
    data = types.SimpleNamespace(addr=addr, inb=b'', outb=b'')
    events = selectors.EVENT_READ | selectors.EVENT_WRITE
    sel.register(conn, events, data=data)

if port!=0:
    import selectors
    sel = selectors.DefaultSelector()
    # ...
    lsock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    lsock.bind(('0.0.0.0', int(port)))
    lsock.listen()
    print('listening on',('0.0.0.0', int(port)))
    lsock.setblocking(False)
    sel.register(lsock, selectors.EVENT_READ, data=None)
    while True:
        events = sel.select(timeout=None)
        for key, mask in events:
            if key.data is None:
                accept_wrapper(key.fileobj)
            else:
                service_connection(key, mask)
            

if cmdloop==1: 
 print("\n cmdloop_Ready. \n", end='',  flush=True)

if cmdloop==1:
 for line in sys.stdin: 
  print(do_nlp_proc(line), flush=True)
  if cmdloop==0: 
   sys.exit(0)
  refresh_on_file_mod()
else:
 sentence=' '.join(sysargv)
 if sentence=="": sentence="George Washington went to Washington."
 print(do_nlp_proc(sentence), flush=True)
 

