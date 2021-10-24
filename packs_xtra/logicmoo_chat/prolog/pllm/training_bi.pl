:- encoding(iso_latin_1).
:- module(pllm,[]).

% :- include(weightless_pllm).

:- X= (is_word/1,is_word/2,ngram/5,ngram/6,trigram/3,trigram/4,tok_split/3,tok_split/4),
  dynamic(X),multifile(X).

:- ensure_loaded(trains_trigrams).
:- ensure_loaded(utils_pllm).

compile_corpus:- 
  compile_corpus_in_mem.

compile_corpus_in_mem:- 
 train_from_corpus,
 compute_corpus_extents,
 nop(retrain_from_trigrams),!.

corpus_stat(corpus_training). corpus_stat(corpus_nodes). corpus_stat(corpus_node_overlap).
corpus_stat(corpus_unique_toks). corpus_stat(corpus_total_toks). 
corpus_stat(sent_num). corpus_stat(corpus_convos).

set_last_oc(OC):- nb_setval(last_oc,OC).
get_last_oc(OC):- nb_current(last_oc,OC).

train_from_corpus:- 
 debugln("reading corpus..."),
 set_last_oc(0:0:0),
 absolute_file_name(pldata('corpus/self_dialogue_corpus/train_from_topic_star_wars.txt'),File,[access(read)]),
 time((open(File,read,In), 
 forall(corpus_stat(Stat),set_flag(Stat,0)),
 set_flag(sent_num,0),
 repeat,
 (at_end_of_stream(In) -> ! ; 
 flag(sent_num,X,X+1), read_line_to_string(In,Str), add_training(X,Str), fail),
 forall(corpus_stat(Stat),(get_flag(Stat,Value),debugln(Stat=Value))))).


retrain_from_trigrams:- 
 debugln("retrain from trigrams..."),
 absolute_file_name(library('../self_dialogue_corpus/train_from.txt'),File,[access(read)]),
 time((open(File,read,In), 
 forall(corpus_stat(Stat),set_flag(Stat,0)),
 set_flag(sent_num,0),
 repeat,
 (at_end_of_stream(In) -> ! ; 
 flag(sent_num,X,X+1), read_line_to_string(In,Str), add_training(X,Str), fail),
 forall(corpus_stat(Stat),(get_flag(Stat,Value),debugln(Stat=Value))))).


save_stat(G):- 
  ( \+ G -> assert(G) ; true),
  nop((writeq(G),writeln('.'))).

use_extent(is_word,1). use_extent(tok_split,3). use_extent(trigram,3). use_extent(ngram,5).
compute_corpus_extents:-
 debugln("compute corpus extents..."),
 time((forall(use_extent(F,A),compute_extent(F,A)))).


min_of(X,Y,X):-X<Y,!. min_of(_,Y,Y).
max_of(X,Y,X):-X>Y,!. max_of(_,Y,Y).
inc_flag(F):- flag(F,X,X+1).
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
  max_of(Insts,1,Insts1), % avoid division by zero
  Mean is round(Total/Insts1),
  High is ((Max-Mean)/2 + Mean),
  Low is (Mean-Min)/2 + Min,
  set_flag(med_high_fa, High), set_flag(med_low_fa, Low),
 nop((
  % adds 20 seconds and is not yet used
  set_flag(above_mean_fa, 0), set_flag(above_med_high_fa, 0), set_flag(num_min_fa, 0),
  set_flag(below_mean_fa, 0), set_flag(below_med_low_fa, 0),
  append_term(NGram,NN,NGramStatN),
  forall(NGramStatN,
    (ignore((NN=Min,inc_flag(num_min_fa))),
     ignore((NN>High,inc_flag(above_med_high_fa))),
     ignore((NN<Low,inc_flag(below_med_low_fa))),
     (NN =< Mean ->inc_flag(below_mean_fa);inc_flag(above_mean_fa)))),
  get_flag(num_min_fa, NEMin), get_flag(above_med_high_fa, NAMedHi),
  get_flag(below_mean_fa, NBMean), get_flag(above_mean_fa, NAMean),  
  get_flag(below_med_low_fa, NBMedLo),
  NAMeanNAMedHi is NAMean-NAMedHi,
  NBMeanNBMedLo is NBMean-NBMedLo,
  NBMedLoNEMin is NBMedLo-NEMin,
 !)),
  Props = [
      (min->min)=NEMin,
      (min->low)=NBMedLoNEMin,
      (low->mean)=NBMeanNBMedLo,
      (mean->high)=NAMeanNAMedHi,
      (high->max)=NAMedHi,
      '---------'='------------',
      (min->max)=Insts, 
      nl,
      min=Min,
      low=Low,
      mean=Mean,
      high=High,
      max=Max,
      total=Total],
  maplist(xthe_unbounds,Props),
  assert(extent_props(F,A,Props)),
  debugln([extent_props(F/A),Props]),!.

xthe_unbounds(X):- ignore(X = (_=x)).

ngram_key(tok_split(O,_,_),O):-!.
ngram_key(is_word(O),O):-!.
ngram_key(trigram(A,B,C),Key):- !, atomic_list_concat([A,B,C],',',Key).
ngram_key(ngram(Loc,A,B,C,D,_),Key):- !, ngram_key(ngram(Loc,A,B,C,D),Key).
ngram_key(ngram(_Loc,oc(_),B,C,oc(_)),Key):- !, atomic_list_concat([oc,B,C,oc],',',Key).
ngram_key(ngram(_Loc,oc(_),A,B,C),Key):- !, atomic_list_concat([oc,A,B,C],',',Key).
ngram_key(ngram(_Loc,A,B,C,oc(_)),Key):- !, atomic_list_concat([A,B,C,oc],',',Key).
ngram_key(ngram(_Loc,A,B,C,D),Key):- atomic_list_concat([A,B,C,D],',',Key).

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

add_training(_,"XXXXXXXXXXX"):-  flag(corpus_convos,Z,Z+1),
   % every 10 conversations will be considered "close"
   Z10 is Z div 10,
   set_last_oc(Z),!,
   Y is (Z10+1)*100,flag(sent_num,_,Y),!.
add_training(X,Str):- tokenize_atom(Str,Toks),
 maplist(downcase_atom,Toks,TokList), 
 pretok(TokList,PreToks),!,
 maplist(add_occurs(is_word),PreToks),
 add_training_toks(X,PreToks).

add_training_toks(_,[]):- !.
add_training_toks(X,[A]):- !, add_training_toks(X,[A,'.']).
add_training_toks(X,PreToks):-
 add_ngrams(except_symbols,trigram,3,skip,PreToks),
 dbltok(oc,PreToks,ReToks),!,
 get_last_oc(LastOC),
 %get_flag(corpus_convos,B),%source_location(_,L),
 NewOC is X+1,
 flag(corpus_training,T,T+1),
 set_last_oc(NewOC),
 append([oc(LastOC)|ReToks],[oc(NewOC)],Grams),!,
 add_ngrams(except_none,ngram,4,X,Grams).

add_ngrams(Except,F,N,Loc,Grams):- length(NGram,N),
 append(NGram,_,Mid),
 forall(append(_,Mid,Grams),assert_ngram(Except,F,Loc,NGram)).

except_none(_).
assert_ngram(Except,F,Loc,List):- 
 (Except == except_none ; maplist(Except,List)),!,
 (Loc==skip->W=..[F|List];W=..[F,Loc|List]),
 ngram_key(W,A),flag(A,X,X+1),
 (Loc==skip-> (( \+ W -> assert(W) ; true)) ; assert(W)),
 (X=0->(flag(corpus_nodes,N,N+1));flag(corpus_node_overlap,O,O+1)),!.

add_occurs(F,Tok):- P=..[F,Tok],
  ignore(( \+ P, assert(P), flag(corpus_unique_toks,O,O+1) )),
  flag(Tok,X,X+1),flag(corpus_total_toks,T,T+1).

except_symbols(X):- \+ (upcase_atom(X,U),downcase_atom(X,U)).

pretok([],[]).
pretok(['.'],[]):-!.
pretok([X,X,X|Nxt],O):-!,atomic_list_concat([X,X,X],',',Y),pretok([Y|Nxt],O).
pretok([A,'\'',S|Grams],[F|ReTok]):- atom_concat(A,S,F),!, pretok(Grams,ReTok).
pretok([A,'´',S|Grams],[F|ReTok]):- atom_concat(A,S,F),!, pretok(Grams,ReTok).
pretok([A,'`',S|Grams],[F|ReTok]):- atom_concat(A,S,F),!, pretok(Grams,ReTok).
pretok([','|Grams],ReTok):- pretok(Grams,ReTok).
pretok(['-'|Grams],ReTok):- pretok(Grams,ReTok).
pretok([A,B,C|Grams],ReTok):- trigram(A,B,C,N), N>40, !, ngram_key(trigram(A,B,C),Key),pretok([Key|Grams],ReTok).
pretok(['!'|Grams],ReTok):- pretok(['.'|Grams],ReTok).
pretok([S|Grams],[S|ReTok]):- pretok(Grams,ReTok).

dbltok(oc,[],[]):-!.
dbltok(Pre,[],[PS]):-!,atoms_join(Pre,oc,PS).
dbltok(Pre,[S|Grams],[PS|ReTok]):- atoms_join(Pre,S,PS),dbltok(S,Grams,ReTok).

atoms_join(A,B,O):- tok_split(O,A,B),!,flag(O,X,X+1).
atoms_join(A,B,O):- atomic_list_concat([A,B],':',O),!,assert(tok_split(O,A,B)),flag(O,X,X+1).

% @TODO use average 
%as_good(T,X):- is_word(T,X),(Nxt>500->X=0;X is 500-Nxt).
%ngram_rate(A,B,C,D,N,NN):- ngram(Loc,A,B,C,D,N), maplist(as_good,[A,B,C,D],Num), sumlist(Num,NN).

autoc(Sent):- autoc(1,Sent).
autoc(N,Sent):- 
  retractall(used_cl(ngram(_,_,_,_))),
  add_blanks(N,Sent,Slotted),no_repeats( map_sent(_Loc,Slotted)),debugln(Slotted).

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
