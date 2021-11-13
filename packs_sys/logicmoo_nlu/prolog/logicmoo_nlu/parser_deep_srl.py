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
  
from neural_srl.shared import *
from neural_srl.shared.constants import *
from neural_srl.shared.dictionary import Dictionary
from neural_srl.shared.inference import *
from neural_srl.shared.tagger_data import TaggerData
from neural_srl.shared.measurements import Timer
from neural_srl.shared.evaluation import SRLEvaluator
from neural_srl.shared.io_utils import bio_to_spans
from neural_srl.shared.reader import string_sequence_to_ids
from neural_srl.shared.scores_pb2 import *
from neural_srl.shared.tensor_pb2 import *
from neural_srl.theano.tagger import BiLSTMTaggerModel
from neural_srl.theano.util import floatX


import argparse
from itertools import izip
from nltk.tokenize import word_tokenize
import numpy
import os
import sys
import select
import theano
def load_model(model_path, model_type):
  config = configuration.get_config(os.path.join(model_path, 'config'))
  # Load word and tag dictionary
  word_dict = Dictionary(unknown_token=UNKNOWN_TOKEN)
  label_dict = Dictionary()
  word_dict.load(os.path.join(model_path, 'word_dict'))
  label_dict.load(os.path.join(model_path, 'label_dict'))
  data = TaggerData(config, [], [], word_dict, label_dict, None, None)

  if model_type == 'srl':
    test_sentences, emb_inits, emb_shapes = reader.get_srl_test_data(
        None, config, data.word_dict, data.label_dict, False)
  else:
    test_sentences, emb_inits, emb_shapes = reader.get_postag_test_data(
        None, config, data.word_dict, data.label_dict, False)
  
  data.embedding_shapes = emb_shapes
  data.embeddings = emb_inits
  model = BiLSTMTaggerModel(data, config=config, fast_predict=True)
  model.load(os.path.join(model_path, 'model.npz'))
  return model, data

def do_nlp_proc(s):
  parser = argparse.ArgumentParser(description=__doc__)
  parser.add_argument('--model',
                      type=str,
                      default='',
                      required=True,
                      help='SRL Model path.')

  parser.add_argument('--pidmodel',
                      type=str,
                      default='',
                      help='Predicate identfication model path.')

  args = parser.parse_args()

  pid_model, pid_data = load_model(args.pidmodel, 'propid')
  srl_model, srl_data = load_model(args.model, 'srl')
  transition_params = get_transition_params(srl_data.label_dict.idx2str)

  pid_pred_function = pid_model.get_distribution_function()
  srl_pred_function = srl_model.get_distribution_function()

  while (True):
    # read sentence
    sentence = raw_input('Please input a sentence:\n')
    tokenized_sent = word_tokenize(sentence)
    #parse_sentence(tokenized_sent)
    print(tokenized_sent)

    # Predicate identification.
    num_tokens = len(tokenized_sent)
    s0 = string_sequence_to_ids(tokenized_sent, pid_data.word_dict, True)
    l0 = [0 for _ in s0]
    x, _, _, weights = pid_data.get_test_data([(s0, l0)], batch_size=None)
    pid_pred, scores0 = pid_pred_function(x, weights)

    s1_sent = string_sequence_to_ids(tokenized_sent, srl_data.word_dict, True)
    s1 = []
    predicates = []
    for i,p in enumerate(pid_pred[0]):
      if pid_data.label_dict.idx2str[p] == 'V':
        predicates.append(i)
        feats = [1 if j == i else 0 for j in range(num_tokens)]
        s1.append((s1_sent, feats, l0))

    if len(s1) == 0:
      # If no identified predicate.
      continue

    # Semantic role labeling.
    x, _, _, weights = srl_data.get_test_data(s1, batch_size=None)
    srl_pred, scores = srl_pred_function(x, weights)

    arguments = []
    for i, sc in enumerate(scores):
      viterbi_pred, _ = viterbi_decode(sc, transition_params)
      arg_spans = bio_to_spans(viterbi_pred, srl_data.label_dict)
      arguments.append(arg_spans)
    
    # Print human-readable results.
    for (pred, args) in izip(predicates, arguments):
      print ("\nPredicate: {}".format(tokenized_sent[pred]))
      for arg in args:
        print ("\t{}: {}".format(arg[0], " ".join(tokenized_sent[arg[1]:arg[2]+1])))
      print ("")

  # while loop. 


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
 

