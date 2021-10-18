
:- module(pllm,[]).

% :- include(weightless_pllm).

:- X= (is_tok/1,is_tok/2,ngram/5,ngram/6),
  dynamic(X),multifile(X).

:- use_module(library(logicmoo_utils)).

% debug printing
debugln(X):- \+ is_list(X) -> dmsg(X) ; maplist(debuglnw,X,Y),atomics_to_string(Y,' ',Z),debugln(Z).
debuglnw(V,S):- var(V), !, sformat(S,"~p",[V]).
debuglnw([N|A],S):- is_list(A),!,maplist(debuglnw,[N|A],Y),atomics_to_string(Y,'_',S).
debuglnw('$'(E),S):- get_flag(E,V),!,sformat(S,"~w=~:d~n",[E,V]).
debuglnw([N],S):- !, debuglnw(N,S).
debuglnw(C,S):- compound(C),!,sformat(S,"~p",[C]).
%debuglnw(C,S):- compound(C),compound_name_arguments(C,N,A),debuglnw([N|A],S).
debuglnw(nl,'\n'):-!.
debuglnw([],''):-!.
debuglnw(E,E).


compile_corpus:- 
  compile_corpus_in_mem.

compile_corpus_in_mem:- 
 train_from_corpus,
 compute_corpus_stats,!.

train_from_corpus:- 
 debugln("reading corpus..."),
 absolute_file_name(library('../self_dialogue_corpus/train_from.txt'),File,[access(read)]),
 time((open(File,read,In),
 set_flag(sent_num,0),
 repeat,
 (at_end_of_stream(In) -> ! ; 
 flag(sent_num,X,X+1), read_line_to_string(In,Str), add_training(X,Str), fail),
 debugln(["Trained results: ",nl, 
   $corpus_training,$corpus_nodes,$corpus_node_overlap,$corpus_unique_toks,$corpus_total_toks]))).

save_stat(G):- assert(G),nop((writeq(G),writeln('.'))).

compute_corpus_stats:-
 debugln("compute corpus stats..."),
 time((forall(is_tok(X),(get_flag(X,NN),save_stat(is_tok(X,NN)))),
 functor(NGrams,ngram,5),
 forall(NGrams,(term_to_atom(NGrams,X),
   get_flag(X,NN),append_term(NGrams,NN,NGramsStat),save_stat(NGramsStat))))),!.

save_corpus_stats:-
 time((tell('plm.pl'),
 write('
 :- style_check(- discontiguous).
 :- X= (is_tok/2,ngram/6),
    dynamic(X),multifile(X). \n'),
  listing([is_tok/2,ngram/6]), told)).

qcompile_corpus:- 
  save_corpus_stats,
  debugln("Compiling now..."),
  time(pllm:qcompile(plm)),
  debugln("Loading now..."),
  time(pllm:ensure_loaded(plm)),
  debugln("Corpus Ready").

add_training(_,"XXXXXXXXXXX"):- flag(corpus_blocks,Z,Z+1),
   % every 10 conversations will be considered "close"
   Z10 is Z div 10,
   Y is (Z10+1)*100,flag(sent_num,_,Y),!.
add_training(X,Str):- flag(sent_num,Y,Y), tokenize_atom(Str,Toks),
 maplist(downcase_atom,Toks,TokList), 
 retok(TokList,ReToks), 
 maplist(add_token_occurs,ReToks),
 append([other_conversant(X)|ReToks],[other_conversant(Y)],Grams),
 flag(corpus_training,T,T+1),
 add_ngrams(5,X,Grams).

add_token_occurs(Tok):- 
  ignore(( \+  is_tok(Tok), assert(is_tok(Tok)), flag(corpus_unique_toks,O,O+1) )),
  flag(Tok,X,X+1),flag(corpus_total_toks,T,T+1).

add_ngrams(N,Loc,Grams):- length(NGram,N),
 append(NGram,_,Mid),
 forall(append(_,Mid,Grams),assert_ngraM(Loc,NGram)).

assert_ngraM(Loc,List):- W=..[ngram,Loc|List],term_to_atom(W,A),flag(A,X,X+1),
 (X=0->(assert(W),flag(corpus_nodes,N,N+1));flag(corpus_node_overlap,O,O+1)).

retok([],[]).
retok(['.'],[]):-!.
retok(['!'],[]):-!.
retok([X,X,X|Nxt],O):-!,atomic_list_concat([X,X,X],' ',Y),retok([Y|Nxt],O).
retok([A,'\'',S|Grams],[F|ReTok]):- atom_concat(A,S,F),!, retok(Grams,ReTok).
retok([S|Grams],[S|ReTok]):- retok(Grams,ReTok).

% @TODO use average 
%as_good(T,X):- is_tok(T,X),(Nxt>500->X=0;X is 500-Nxt).
%ngram_rate(A,B,C,D,N,NN):- ngram(Loc,A,B,C,D,N), maplist(as_good,[A,B,C,D],Num), sumlist(Num,NN).

add_blanks(_,[],[]):-!.
add_blanks(N,[len(S)|Sent],Slotted):- integer(S),length(L,S),!,add_blanks(N,Sent,Mid),append(L,Mid,Slotted).
add_blanks(N,[S|Sent],[S|Slotted]):- add_blanks(N,Sent,Slotted).
add_blanks(N,[S|Sent],Slotted):- var(S),between(2,N,L),length(S,L),add_blanks(N,Sent,Mid),append(S,Mid,Slotted).

autoc(Sent):- autoc(1,Sent).
autoc(N,Sent):- add_blanks(N,Sent,Slotted),no_repeats( map_sent(Slotted)),debugln(Slotted).

map_sent(Sent):- ground(Sent),!.
map_sent(Sent):- var(Sent), length(Sent,9),map_sent(Sent).
%map_sent(Sent):- reverse(Sent,Reverse), map_sent_rev(Reverse).
map_sent([A,B,C,D|More]):-
  once_ngram(A,B,C,D,_Fire),
  map_sent([C,D|More]).
map_sent([A,B,C,D|More]):-
  once_ngram(_,A,B,C,_Fire),
  map_sent([B,C,D|More]).
map_sent(List):- five(Five,_Occurs),append(List,_,Five),
  apply(once_ngram,Five).


:-dynamic(used_cl/1).

:- style_check(- singleton).

may_use(Loc,_,B,C,D,_):- \+ used_cl(ngram(A,B,C,D)), assert(used_cl(ngram(A,B,C,D)),Cl2), undo(erase(Cl2)), !.

once_ngram(A,B,C,D,N):- ngram(Loc,A,B,C,D,N), may_use(Loc,A,B,C,D,N).
five([_,_,_,_,N],N).

gen6([A,B,C,D,E,F,G,H]=N):-
  ngram(Loc1,E,F,G,H,Z), ngram(Loc2,C,D,E,F,Y), ngram(Loc3,A,B,C,D,X), N is X+Y+Z.

%:- pllm:ensure_loaded(plm).
% added for conversations
ngram(Loc,A,other_conversant(X),B,C,NN):- nonvar(X), ngram(Loc,_,_,A,other_conversant(X),_),ngram(_ULoc,other_conversant(X),B,C,_,NN).
ngram(Loc,A,B,other_conversant(X),C,NN):- nonvar(X), ngram(Loc,_,A,B,other_conversant(X),_),ngram(_ULoc,other_conversant(X),C,_,_,NN).

:- add_history(compile_corpus).

:- fixup_exports.
