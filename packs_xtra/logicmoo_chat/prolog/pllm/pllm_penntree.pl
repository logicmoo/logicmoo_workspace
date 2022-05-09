:- encoding(iso_latin_1).
:- module(pllm,[]).

% :- include(weightless_pllm).

pllm_preds([training/3,is_word/1,is_word/2,ngram/5,ngram/6,trigram/3,trigram/4,tok_split/3,tok_split/4,tmp:buffer_training/2]).

declare_preds(X):- dynamic(X),multifile(X).

:- pllm_preds(L), maplist(declare_preds,L).

% :- ensure_loaded(trains_trigrams).
:- ensure_loaded(utils_pllm).
:- ensure_loaded(library(logicmoo_nlu)).
:- ensure_loaded(library(logicmoo_nlu/parser_link_grammar)).

%compile_corpus:- functor(P,ngram,6), predicate_property(P,number_of_clauses(N)),N>2.
compile_corpus:- 
  compile_corpus_in_mem.

recompile_corpus:- 
  pllm_preds(L),
  maplist(abolish,L),
  maplist(declare_preds,L),
  compile_corpus_in_mem.

compile_corpus_in_mem:- 
 train_from_corpus,
 compute_corpus_extents,
 nop(retrain_from_trigrams),!.

corpus_stat(corpus_training). corpus_stat(corpus_nodes). corpus_stat(corpus_node_overlap).
corpus_stat(corpus_unique_toks). corpus_stat(corpus_total_toks). 
corpus_stat(corpus_convos).

set_last_oc(OC):- nb_setval(last_oc,OC).
get_last_oc(OC):- nb_current(last_oc,OC).

%train_from_corpus:- training(_,string,_),!,forall(training(XX,string,Val),add_training_str(XX,Val)).
train_from_corpus:- train_from_corpus(library('../self_dialogue_corpus/train_from.txt')).

train_from_corpus(Path):-
 debugln(["reading corpus...",Path]),
setup_call_cleanup(
 must(absolute_file_name(Path,File,[access(read)])),

 time((open(File,read,In), 
 forall(corpus_stat(Stat),set_flag(Stat,0)),
 set_flag(file_line,0),
 repeat,
 (at_end_of_stream(In) -> ! ; 
 inc_flag(file_line), read_line_to_string(In,Str),get_flag(file_line,X),add_training(X,Str), fail),
 forall(corpus_stat(Stat),(get_flag(Stat,Value),debugln(Stat=Value))))),
 save_training).

:- add_history(load_training).
load_training:-
  pllm_preds(L),maplist(load_training,L).
load_training(F/A):-
 atomic_list_concat(['done_',F,'_',A,'.pl'],File),
 (exists_file(File)->ensure_loaded(File) ; true).

save_training:-
  pllm_preds(L),maplist(save_training,L).
save_training(F/A):-
 atomic_list_concat(['done_',F,'_',A,'.pl'],File),
 tell(File),
 writeln(:- encoding(iso_latin_1)),
 listing(F/A),
 % functor(P,F,A),forall(P,(writeq(P),writeln('.'))),
 told.


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
  A2 is A + 1, functor(NGram2,F,A2), dynamic(NGram2),
  set_flag(total_fa,0),
  set_flag(min_fa,999999999),
  set_flag(max_fa,0),
  forall(NGram,(ngram_val(NGram,NN),
     flag(total_fa,Total,Total+NN),
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
  maplist(save_extents(F,A),Props),
  debugln([extent_props(F/A),Props]),!.

save_extents(_,_,(_=x)):-!.
save_extents(F,A,X=Y):- !, assert(extent_props(F,A,X,Y)). 
save_extents(_,_,_):-!.

ngram_val(NGram,NN):- ngram_key(NGram,Key),get_flag(Key,NN).

ngram_inc(NGram):- ngram_inc(NGram,_NN).
ngram_inc(NGram,NN):- ngram_key(NGram,Key),flag(Key,NN,NN+1).

ngram_key(tok_split(O,_,_),O):-!.
ngram_key(is_word(O),O):-!.
ngram_key(trigram(A,B,C),Key):- !, join_text([A,B,C],Key).
ngram_key(ngram(Loc,A,B,C,D,_),Key):- !, ngram_key(ngram(Loc,A,B,C,D),Key).
ngram_key(ngram(_Loc,oc(_),B,C,oc(_)),Key):- !, join_text([oc,B,C,oc],Key).
ngram_key(ngram(_Loc,oc(_),A,B,C),Key):- !, join_text([oc,A,B,C],Key).
ngram_key(ngram(_Loc,A,B,C,oc(_)),Key):- !, join_text([A,B,C,oc],Key).
ngram_key(ngram(_Loc,A,B,C,D),Key):- join_text([A,B,C,D],Key).

join_text(List,Key):- atomic_list_concat(List,',',Key).

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


add_training(X,Str):- 
 flag(speech_act,A,A+1),
 get_flag(corpus_convos,Z),
 XX is ((Z+1)*100_000_000_000)+(A*10_000_000)+X, 
 add_training_str(XX,Str).

add_training_str(XX,"XXXXXXXXXXX"):- C = 100_000_000_000, Buffer is floor(XX/C)*C + 09911111111111,  
  ignore(add_conversation_training(Buffer)), inc_flag(corpus_convos),!,set_flag(speech_act,1).
add_training_str(XX,Str):- 1 is XX mod 2, !, add_training_said(said,"Al",XX,Str),!. 
add_training_str(XX,Str):- add_training_said(said,"Jo",XX,Str),!. 

add_training_said(_,_,_,[]):-!.
add_training_said(Says,PERSON,XX,Str):- string(Str),tokenize_atom(Str,Toks),!,pretok(Toks,PreToks),
   add_training_said(Says,PERSON,XX,PreToks).
add_training_said(Says,PERSON,XX,Toks):- append(Left,['.'],Toks),!,add_training_said(Says,PERSON,XX,Left).
add_training_said(Says,PERSON,XX,Toks):- append(Left,[LE|Right],Toks), Right\==[],
  member(LE,['.','?']),append(Left,[LE],Said),!,
  add_training_said(Says,PERSON,XX,Said), add_training_said(Says,PERSON,XX,Right).
add_training_said(said,PERSON,XX,Toks):- append(Left,['?'],Toks),!,add_training_said(asks,PERSON,XX,Left).
add_training_said(Says,PERSON,XX,Toks):-
  (Says==asks-> J='?' ; J ='.'),
  atomics_to_string(Toks,' ',Str), atomics_to_string([Str,J],'',StrP), sformat(S," ~w ~w, ~q ",[PERSON,Says,StrP]),
  BB = tmp:buffer_training(XX,S), assert(BB),wdmsg(BB).

 
assert_training(XX,P,Parse):- assert_if_new(training(XX,P,Parse)),dmsg(training(XX,P,Parse)),save_training(training/3).

do_training(XX,_Str,F2):- training(XX,F2,_),!.
do_training(XX,Str,F2):-
  catch(call(F2,Str,Result),E,(dumpSt,format('%~~~~~ ERROR: ~p~n',[E --> call(F2,Str,Result)])),fail),!,
  assert_training(XX,F2,Result),!.


add_conversation_training(XX):- 
 wots(Str,
  forall(retract(tmp:buffer_training(_,S)),(write(' : '),writeln(S)))),
  assert_training(XX,convo,Str),
  do_training(XX,Str,text_to_best_tree).


all_letters(X):- \+ (upcase_atom(X,U),downcase_atom(X,U)).
 %tokenize_atom(Str,Toks),
 %maplist(downcase_atom,Toks,TokList),pretok(TokList,PreToks),!,
 %assert_training(XX,tokenize_atom,PreToks),


add_training_toks(_,[]):- !.
add_training_toks(X,[A]):- !, add_training_toks(X,[A,'.']).
add_training_toks(XX,PreToks):-
 maplist(add_occurs(is_word),PreToks),
 inc_flag(corpus_training),
 add_ngrams(except_symbols,trigram,3,skip,PreToks),
 dbltok(oc,PreToks,ReToks),!,
 XX1 is XX+1,
 append([oc(XX)|ReToks],[oc(XX1)],Grams),!,
 add_ngrams(except_none,ngram,4,XX,Grams).

add_ngrams(Except,F,N,Loc,Grams):- length(NGram,N),
 append(NGram,_,Mid),
 forall(append(_,Mid,Grams),assert_ngram(Except,F,Loc,NGram)).

except_none(_).
assert_ngram(Except,F,Loc,List):- 
 (Except == except_none ; maplist(Except,List)),!,
 (Loc==skip->W=..[F|List];W=..[F,Loc|List]),
 ngram_inc(W,X),
 (Loc==skip-> (( \+ W -> assert(W) ; true)) ; assert(W)),
 (X=0->(inc_flag(corpus_nodes));inc_flag(corpus_node_overlap)),!.

add_occurs(F,Tok):- P=..[F,Tok],
  ignore(( \+ P, assert(P), inc_flag(corpus_unique_toks) )),
  ngram_inc(P),inc_flag(corpus_total_toks).

except_symbols(X):- \+ (upcase_atom(X,U),downcase_atom(X,U)).

pretok([],[]).
pretok(['.'],[]):-!.
pretok([X,X,X|Nxt],O):-!,atomic_list_concat([X,X,X],',',Y),pretok([Y|Nxt],O).
pretok([A,'-',S|Grams],[F|ReTok]):- atomic_list_concat([A,S],'-',F),!, pretok(Grams,ReTok).
pretok([A,'\'',S|Grams],[F|ReTok]):- all_letters(A),all_letters(S), atomic_list_concat([A,S],'\'',F),!, pretok(Grams,ReTok).
pretok([A,'´',S|Grams],[F|ReTok]):- all_letters(A),all_letters(S), atomic_list_concat([A,S],'\'',F),!, pretok(Grams,ReTok).
pretok([A,'`',S|Grams],[F|ReTok]):- all_letters(A),all_letters(S), atomic_list_concat([A,S],'\'',F),!, pretok(Grams,ReTok).
pretok(['!'|Grams],ReTok):- pretok(['.'|Grams],ReTok).
pretok([S|Grams],[S|ReTok]):- pretok(Grams,ReTok).

% dbltok(_,X,X):-!.
dbltok(oc,[],[]):-!.
dbltok(Pre,[],[PS]):-!, atoms_join(Pre,oc,PS).
dbltok(Pre,[S|Grams],[PS|ReTok]):- atoms_join(Pre,S,PS), dbltok(S,Grams,ReTok).

atoms_join(A,B,O):- tok_split(O,A,B),!,ngram_inc(tok_split(O,A,B)).
atoms_join(A,B,O):- atomic_list_concat([A,B],':',O),!,assert(tok_split(O,A,B)),ngram_inc(tok_split(O,A,B)).

% @TODO use average 
%as_good(T,X):- is_word(T,X),(Nxt>500->X=0;X is 500-Nxt).
%ngram_rate(A,B,C,D,N,NN):- ngram(Loc,A,B,C,D,N), maplist(as_good,[A,B,C,D],Num), sumlist(Num,NN).

add_blanks(N,S,Slotted):- \+ is_list(S),!,add_blanks(N,[S],Slotted).
add_blanks(_,[],[]):-!.

add_blanks(N,[A,B|Sent],[O|Slotted]):- tok_split(O,A,B),!,add_blanks(N,Sent,Slotted).
add_blanks(N,[S|Sent],[O|Slotted]):- \+ \+ tok_split(_,S,_),!, tok_split(O,S,_),add_blanks(N,Sent,Slotted).
add_blanks(N,[O|Sent],[O|Slotted]):- atom(O), tok_split(O,_,_),!,add_blanks(N,Sent,Slotted).

add_blanks(N,[len(S)|Sent],Slotted):- integer(S),length(L,S),!,add_blanks(N,Sent,Mid),append(L,Mid,Slotted).
add_blanks(N,[S|Sent],[A|Slotted]):- string(S),atom_string(A,S),!,add_blanks(N,Sent,Slotted).
add_blanks(N,[S|Sent],Slotted):- var(S),!,between(1,N,L),add_blanks(N,[1-L|Sent],Slotted).
add_blanks(N,[Lo-Hi|Sent],Slotted):- (integer(Lo);integer(Hi)),!,between(Lo,Hi,L),length(S,L),add_blanks(N,Sent,Mid),append(S,Mid,Slotted).
add_blanks(N,[S|Sent],Slotted):- is_list(S),!,flatten(S,SL),append(SL,Sent,SLSent),!,add_blanks(N,SLSent,Slotted).
add_blanks(N,[S|Sent],Slotted):- atom(S),into_mw(S,SL),!,append(SL,Sent,SLSent),!,add_blanks(N,SLSent,Slotted).
add_blanks(N,[S|Sent],[S|Slotted]):- add_blanks(N,Sent,Slotted).

into_mw(S,SL):- into_mw0(S,SL),SL\==[S],!.
into_mw0(S,SL):- atomic_list_concat([M,_|_],':',S),!,into_mw0(M,SL).
into_mw0(S,SL):- atomic_list_concat(SL,',',S).
into_mw0(S,SL):- atomic_list_concat(SL,' ',S).
into_mw0(S,SL):- atomic_list_concat(SL,'_',S).

loc_dists(Loc1,Loc2, NN):- NN is abs(Loc1-Loc2).
loc_dists(Loc1,Loc2,Loc3, NN):- NN is (abs(Loc1-Loc2) + abs(Loc3-Loc2) + abs(Loc1-Loc3))/3.

%:- pllm:ensure_loaded(plm).
% added for conversations
ngram(Loc,A,oc(X),B,C,NN):- nonvar(X), ngram(Loc,_,_,A,oc(X),_),ngram(_ULoc,oc(X),B,C,_,NN).
ngram(Loc,A,B,oc(X),C,NN):- nonvar(X), ngram(Loc,_,A,B,oc(X),_),ngram(_ULoc,oc(X),C,_,_,NN).

autoc(Sent):- autoc(1,Sent).
autoc(N,Sent):- 
  retractall(used_cl(ngram(_,_,_,_))),
  add_blanks(N,Sent,Slotted),no_repeats( map_sent(_,_Loc,Slotted)),fmt_pllm(Slotted).

good_toks(Key,E):- functor(P,ngram,6),arg(6,P,E),no_repeats(Key,(P,ngram_key(P,Key))).


:- add_history(recompile_corpus).

:- fixup_exports.

:-dynamic(used_cl/1).

map_sent(_,_,Sent):- ground(Sent),!.
map_sent(LR,Loc,Sent):- var(Sent), length(Sent,9),map_sent(LR,Loc,Sent).
map_sent(LR,Loc,List):- LR=lr,append(Left,[X|More],List),nonvar(X),Left\==[],!,map_sent(LR,Loc,[X|More]),map_sent(rl,Loc,List).
map_sent(LR,Loc,[A,B,C,D|More]):- some_ngram(Loc,A,B,C,D,_Fire), map_sent(LR,Loc,[C,D|More]).
map_sent(LR,Loc,[A,B,C,D|More]):- some_ngram(Loc,A,B,C,_,_Fire), map_sent(LR,Loc,[B,C,D|More]).
map_sent(_,Loc,List):- ABCDO=[_,_,_,_,_Occurs],append(List,_,ABCDO), apply(some_ngram,[Loc|ABCDO]).


some_ngram(_PrevLoc,A,B,C,D,N):- pick_ngram(Loc,A,B,C,D,N),may_use(Loc,A,B,C,D,N).

pick_ngram(Loc,A,B,C,D,N):- maplist(var,[A,B,C,D])->rnd_ngram(Loc,A,B,C,D,N);ngram(Loc,A,B,C,D,N).

rnd_ngram(Loc,A,B,C,D,N):-  G = ngram(Loc,A,B,C,D,N),
 predicate_property(G,number_of_clauses(R)),
  CN is random(R)+1, nth_clause(G,CN,Ref),clause(G,Body,Ref),Body.


:- style_check(- singleton).

:- add_history((good_toks(Key,E),E>20)).
:- add_history((autoc([ 'music:you',len(200)]))).
:- add_history((autoc([oc,'music:you',len(200)]))).
:- add_history((autoc([ 'oc:music', 'music:you',len(200)]))).
:- add_history((autoc([ 'music',len(200)]))).
:- add_history((autoc([len(10),music,len(200)]))).

may_use(Loc,_,B,C,D,_):- \+ used_cl(ngram(A,B,C,D)), assert(used_cl(ngram(A,B,C,D)),Cl2), undo(erase(Cl2)), !.


gen6([A,B,C,D,E,F,G,H]=N):-
  ngram(Loc1,E,F,G,H,Z), ngram(Loc2,C,D,E,F,Y), ngram(Loc3,A,B,C,D,X), N is X+Y+Z.
:- fixup_exports.

:- if(\+ prolog_load_context(reloading, true)).
:- load_training.
:- compile_corpus.
:- endif.


