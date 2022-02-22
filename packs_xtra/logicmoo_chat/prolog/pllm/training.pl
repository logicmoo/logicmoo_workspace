
:- module(pllm,[]).

% :- include(weightless_pllm).

:- X= (is_word/1,is_word/2,ngram/5,ngram/6,trigram/3,trigram/4,tok_split/3),
  dynamic(X),multifile(X).

:- ensure_loaded(utils_pllm).


compile_corpus:- 
  compile_corpus_in_mem.

compile_corpus_in_mem:- 
 train_from_corpus,
 compute_corpus_extents,!.

corpus_stat(corpus_training). corpus_stat(corpus_nodes). corpus_stat(corpus_node_overlap).
corpus_stat(corpus_unique_toks). corpus_stat(corpus_total_toks). 
corpus_stat(sent_num). corpus_stat(corpus_convos).

train_from_corpus:- 
 debugln("reading corpus..."),
 %absolute_file_name(library('../ext/self_dialogue_corpus/train_from.txt'),File,[access(read)]),
 absolute_file_name(pldata('corpus/self_dialogue_corpus/train_from.txt'),File,[access(read)]),
 time((open(File,read,In),
 forall(corpus_stat(Stat),set_flag(Stat,0)),
 set_flag(sent_num,0),
 repeat,
 (at_end_of_stream(In) -> ! ; 
 flag(sent_num,X,X+1), read_line_to_string(In,Str), add_training(X,Str), fail),
 forall(corpus_stat(Stat),(get_flag(Stat,Value),debugln(Stat=Value))))).

save_stat(G):- assert(G),nop((writeq(G),writeln('.'))).

use_extent(is_word,1). use_extent(tok_split,3). use_extent(ngram,5).
compute_corpus_extents:-
 debugln("compute corpus extents..."),
 time((forall(use_extent(F,A),compute_extent(F,A)))).


min_of(X,Y,X):-X<Y,!. min_of(_,Y,Y).
max_of(X,Y,X):-X>Y,!. max_of(_,Y,Y).

compute_extent(F,A):-
  functor(NGram,F,A),
  set_flag(total_fa,0),
  set_flag(min_fa,999999999),
  set_flag(max_fa,0),
  forall(NGram,(ngram_key(NGram,X), 
     get_flag(X,NN),flag(total_fa,Total,Total+NN),
     get_flag(min_fa,Min),min_of(Min,NN,NewMin),set_flag(min_fa,NewMin),
     get_flag(max_fa,Max),max_of(Max,NN,NewMax),set_flag(max_fa,NewMax),
     append_term(NGram,NN,NGramStat),save_stat(NGramStat))),  
  get_flag(total_fa,Total),
  get_flag(min_fa,Min),
  get_flag(max_fa,Max),
  predicate_property(NGram,number_of_clauses(Insts)),
  Avrg is Total/(Insts+1),
  Props = [insts=Insts,total=Total,min=Min,max=Max,avrg=Avrg],
  assert(extent_props(F,A,Props)),
  debugln([extent_props(F/A),Props]),!.


ngram_key(tok_split(O,_,_),O).
ngram_key(is_word(O),O).
ngram_key(P,K):- ngram_key(P,_,K).
ngram_key(ngram(Loc,A,B,C,D,_),T,Key):- !, ngram_key(ngram(Loc,A,B,C,D),T,Key).
ngram_key(ngram(_Loc,oc(_),A,B,C),T,Key):- T= sgram(A,B,C), !, term_to_atom(T,Key).
ngram_key(ngram(_Loc,A,B,C,oc(_)),T,Key):- T= sgram(A,B,C), !, term_to_atom(T,Key).
ngram_key(ngram(_Loc,A,B,C,D),[trigram(A,B,C,D)],Key):- !, term_to_atom([A,B,C],Key).
ngram_key(trigram(A,B,C),[],Key):- term_to_atom([A,B,C],Key).


save_corpus_stats:-
 time((tell('plm.pl'),
 write('
 :- style_check(- discontiguous).
 :- X= (is_word/2,ngram/6),
    dynamic(X),multifile(X). \n'),
  listing([is_word/2,ngram/6]), told)).

qcompile_corpus:- 
  save_corpus_stats,
  debugln("Compiling now..."),
  time(pllm:qcompile(plm)),
  debugln("Loading now..."),
  time(pllm:ensure_loaded(plm)),
  debugln("Corpus Ready").

add_training(_,"XXXXXXXXXXX"):- flag(corpus_convos,Z,Z+1),
   % every 10 conversations will be considered "close"
   Z10 is Z div 10,
   Y is (Z10+1)*100,flag(sent_num,_,Y),!.
add_training(X,Str):- flag(sent_num,Y,Y), tokenize_atom(Str,Toks),
 maplist(downcase_atom,Toks,TokList), 
 pretok(TokList,PreToks),!, 
 maplist(add_occurs(is_word),PreToks),
 dbltok(oc,PreToks,ReToks),!, 
 append([oc(X)|ReToks],[oc(Y)],Grams),!,
 flag(corpus_training,T,T+1),
 add_ngrams(4,X,Grams).

add_ngrams(N,Loc,Grams):- length(NGram,N),
 append(NGram,_,Mid),
 forall(append(_,Mid,Grams),assert_ngram(ngram,Loc,NGram)).

assert_ngram(P,Loc,List):- W=..[P,Loc|List],ngram_key(W,A),flag(A,X,X+1),
  assert_if_new(W),
 (X=0->(flag(corpus_nodes,N,N+1));flag(corpus_node_overlap,O,O+1)).

add_occurs(F,Tok):- P=..[F,Tok],
  ignore(( \+ P, assert(P), flag(corpus_unique_toks,O,O+1) )),
  flag(Tok,X,X+1),flag(corpus_total_toks,T,T+1).

pretok([],[]).
pretok(['.'],[]):-!.
pretok(['!'],[]):-!.
pretok([X,X,X|Nxt],O):-!,atomic_list_concat([X,X,X],' ',Y),pretok([Y|Nxt],O).
pretok([A,'\'',S|Grams],[F|ReTok]):- atom_concat(A,S,F),!, pretok(Grams,ReTok).
pretok([','|Grams],ReTok):- pretok(Grams,ReTok).
pretok([S|Grams],[S|ReTok]):- pretok(Grams,ReTok).

dbltok(oc,X,X):-!.
dbltok(oc,[],[]):-!.
dbltok(Pre,[],[PS]):-!,atoms_join(Pre,oc,PS).
dbltok(Pre,[S|Grams],[PS|ReTok]):- atoms_join(Pre,S,PS),dbltok(S,Grams,ReTok).

atoms_join(A,B,O):- tok_split(O,A,B),!,flag(O,X,X+1).
atoms_join(A,B,O):- atomic_list_concat([A,B],':',O),!,assert(tok_split(O,A,B)),flag(O,X,X+1).

% @TODO use average 
%as_good(T,X):- is_word(T,X),(Nxt>500->X=0;X is 500-Nxt).
%ngram_rate(A,B,C,D,N,NN):- ngram(Loc,A,B,C,D,N), maplist(as_good,[A,B,C,D],Num), sumlist(Num,NN).

autoc(Sent):- autoc(1,Sent).
autoc(N,Sent):- add_blanks(N,Sent,Slotted),no_repeats( map_sent(_Loc,Slotted)),debugln(Slotted).

add_blanks(_,[],[]):-!.
add_blanks(N,[len(S)|Sent],Slotted):- integer(S),length(L,S),!,add_blanks(N,Sent,Mid),append(L,Mid,Slotted).
add_blanks(N,[S|Sent],[S|Slotted]):- add_blanks(N,Sent,Slotted).
add_blanks(N,[S|Sent],Slotted):- var(S),between(2,N,L),length(S,L),add_blanks(N,Sent,Mid),append(S,Mid,Slotted).

loc_dists(Loc1,Loc2, NN):- NN is abs(Loc1-Loc2).
loc_dists(Loc1,Loc2,Loc3, NN):- NN is (abs(Loc1-Loc2) + abs(Loc3-Loc2) + abs(Loc1-Loc3))/3.

%:- pllm:ensure_loaded(plm).
% added for conversations
ngram(Loc,A,oc(X),B,C,NN):- nonvar(X), ngram(Loc,_,_,A,oc(X),_),ngram(_ULoc,oc(X),B,C,_,NN).
ngram(Loc,A,B,oc(X),C,NN):- nonvar(X), ngram(Loc,_,A,B,oc(X),_),ngram(_ULoc,oc(X),C,_,_,NN).

:- add_history(compile_corpus).

good_toks(Key,E):- functor(P,ngram,6),arg(6,P,E),no_repeats(Key,(P,ngram_key(P,Key))).

:- if(\+ prolog_load_context(reloading, true)).
:- compile_corpus.
:- endif.

:- fixup_exports.
