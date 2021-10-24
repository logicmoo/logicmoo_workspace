:- encoding(iso_latin_1).
:- module(pllm,[]).
:- encoding(iso_latin_1).

% :- include(weightless_pllm).

pllm_preds([training/3,is_word/1,is_word/2,ngram/5,ngram/6,trigram/3,trigram/4,tok_split/3,tok_split/4]).

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

% train_from_corpus:- training(_,string,_),!,forall(training(XX,string,Val),add_training_str(XX,Val)).
train_from_corpus:- train_from_corpus(pldata('corpus/self_dialogue_corpus/train_from_topic_star_wars.txt')).

in_temp_dir(G):-
 must(absolute_file_name(pldata('corpus/tmpdata'),Dir,[access(read),file_type(directory)])),
 setup_call_cleanup(working_directory(X,Dir),must_or_rtrace(G),working_directory(_,X)).
 

train_from_corpus(Path):-
 debugln(["reading corpus...",Path]),
setup_call_cleanup(
 must(absolute_file_name(Path,File,[access(read)])),

 time((open(File,read,In), 
 forall(corpus_stat(Stat),set_flag(Stat,0)),
 set_flag(file_line,0),
 repeat,
 (at_end_of_stream(In) -> ! ; 
 inc_flag(file_line), read_line_to_string(In,Str),get_flag(file_line,X),once(add_training(X,Str)), fail),
 forall(corpus_stat(Stat),(get_flag(Stat,Value),debugln(Stat=Value))))),
 save_training).


:- add_history(load_training).
load_training:- in_temp_dir(load_training0).
load_training0:-
  pllm_preds(L),maplist(load_training,L).

load_training(MFA):- !, compute_module(MFA,M,F/A),
 functor(P,F,A),MP=M:P,
 atomic_list_concat(['done_',M,'_',F,'_',A,'.pl'],File),
 (predicate_property(MP,number_of_clauses(Before));Before=0),!,
 ignore((exists_file(File) -> ensure_loaded(File) ; true)),
 (predicate_property(MP,number_of_clauses(After));After=0),!,
 debugln(M:F/A=(Before->After)).

compute_module(MFA,M,FA):- strip_module(MFA,M0,FA),compute_m(M0,M),!.

compute_m(user,pllm).
compute_m(M,M).

save_training:- in_temp_dir(save_training0).
save_training0:-
  pllm_preds(L),maplist(save_training,L).
save_training(MFA):- !, compute_module(MFA,M,F/A),
 atomic_list_concat(['done_',M,'_',F,'_',A,'.pl'],File),
 tell(File),
 writeq(:- encoding(iso_latin_1)),writeln('.'),
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

add_punct(X,X):- last(X,E),member(E,['?','.','!']).
add_punct(X,Y):- append(X,['.'],Y).

add_training_str(_,"XXXXXXXXXXX"):- inc_flag(corpus_convos), 
  %C = 100_000_000_000, Buffer is floor(XX/C)*C + 01111111111,  
  %ignore(add_conversation_training(Buffer)), !,
  set_flag(speech_act,1),!.
%add_training_str(XX,Str):- 1 is XX mod 2, !, add_training_said(said,"Al",XX,Str),!. 
%add_training_str(XX,Str):- add_training_said(said,"Jo",XX,Str),!. 


add_training_str(XX,Str) :-
 must_det_l((
   string(Str),
   assert_training_v(XX,string,Str),
   tokenize_atom(Str,Toks),!,
   pretok(Toks,PreToks0),
   add_punct(PreToks0,PreToks),
   text_to_tree(PreToks,Tree),
   assert_training(XX,text_to_tree,Tree),
   %writeq(sample_tree(Tree)),writeln('.'),
   unphrasify(Tree,List),
   assert_training(XX,unphrasify,List),
   tree_to_toks(List,PostToks),!,
   assert_training(XX,tree_to_toks,PostToks),
   add_training_toks(XX,PostToks))).

 
/* Old Way
add_training_str(XX,Str):- 
 tokenize_atom(Str,Toks),
 maplist(downcase_atom,Toks,TokList), 
 pretok(TokList,PreToks),!,
 add_training_toks(XX,PreToks).
 
*/

tree_to_toks:- mmake, forall(sample_tree(Tree),tree_to_toks1(Tree)).
sample_tree(['SEQBAR',['CORENLP',['S',['CC','And'],['ADVP',['RB',then]],['NP',['NP',['PRP$',her],['NN',son]],[',',','],['NP',['NNP','Ben']],[',',',']],['VP',['VP',['VBZ',turns],['NP',['DT',all],['NNP','Sith']]],['CC',and],['VP',['VBZ',joins],['NP',['DT',the],['JJ',dark],['NN',side]]]],['.','.']]],['CORENLP',['S',['PRN',['S',['NP',['DT','That']],['VP',['VBD',had],['S',['VP',['TO',to],['VP',['VB',have],['VP',['VBN',factored],['PP',['IN',into],['NP',['PRP$',her],['NNS',reasons]]],['S',['VP',['TO',to],['VP',['VB',stay],['ADVP',['RB',away]],['PP',['IN',from],['NP',['NP',['DT',the],['NN',call]],['PP',['IN',of],['NP',['DT',the],['NN',force]]]]]]]]]]]]]]],[',',','],['VB',do],['RB',not],['NP',['PRP',you]],['VP',['VB',think]],['.',?]]]]).
sample_tree(['CORENLP',['S',['NP',['PRP','I']],['VP',['VB',hate],['S',['VP',['TO',to],['VP',['VB',say],['S',['NP',['PRP',it]],['VP',['VB','buuut.']],[',',',']]]]]],['.','.']],['S',['VP',[',',',']],['.','.']]]).
sample_tree(['SEQBAR',['CORENLP',['SBAR',['NP',['WP',who]],['S',['VP',['MD',would],['VP',['VB',pick],['NP',['NN',kylo]]]]],['.',?]]],['CORENLP',['S',['ADVP',['RB',definitely]],['ADVP',['RB',not]],['NP',['PRP',me]]]]]).
sample_tree(['SEQBAR',['CORENLP',['S',['S',['NP',['PRP','He']],['VP',['VBD',was],['NP',['NP',['NNP','Luke'],['POS','\'s']],['NNP','Padwan']]]],[',',','],['CC',but],['S',['NP',['PRP',he]],['VP',['VBD',turned]]],['.','.']]],['SEQBAR',['S',['NP',['PRP','It']],['VP',['AUX',has],['RB',not],['VP',['AUX',been],['VP',['VBN',shown],['FRAG',['WHADVP',['WRB',why]]]]]],['.','.']],['CORENLP',['S',['PRN',['S',['NP',['PRP','He']],['VP',['VBZ',is],['ADVP',['RB',no],['RBR',longer]],['NP',['NNP','Jedi']]]]],[',',','],['NP',['PRP',he]],['VP',['VBZ',is],['ADJP',['JJ',sith]],['ADVP',['RB',now]]]]]]]).
sample_tree(['CORENLP',['SBAR',['INTJ',['UH','Well']],[',',','],['SBAR',['IN',if],['S',['NP',['PRP',it]],['VP',['VBZ',is],['NP',['NNP','Rey']]]]],[',',','],['ADVP',['RB',then]],['WHADVP',['WRB',why]],['S',['VBD',did],['NP',['PRP',it]],['RB',not],['VP',['VB',wake],['SBAR',['WHADVP',['WRB',when]],['S',['NP',['NNP','Klyo']],['VP',['VBD',came],['PP',['IN',into],['NP',['NN',power]]]]]]]]]]).
sample_tree([ 'CORENLP',
     [ 'SBAR',
       [ 'NP',
         ['WP','Who']],
       [ 'S',
         ['VBZ',is],
         [ 'NP',
           ['PRP$',your],
           ['JJ',favorite],
           ['NN',character]]],
       ['.',?]]]).
sample_tree(['SEQBAR',['CORENLP',['S',['INTJ',['UH','Well']],[',',','],['NP',['PRP',it]],['VP',['VBZ','\'s'],['NP',['DT',a],['NN',movie]]],['.','.']]],['CORENLP',['S',['NP',['PRP','He']],['VP',['MD',could],['VP',['VB',show],['PRT',['RP',up]]]]]]]).
sample_tree(['CORENLP',['S',['VB','Are'],['NP',['PRP',you]],['NP',['NP',['DT',a],['NN',fan]],['PP',['IN',of],['NP',['DT',the],['NML',['NNP','Star'],['NNPS','Wars']],['NN',series]]]],['.',?]]]).
sample_tree(['CORENLP',['S',['NP',['PRP','I']],['VP',['VB',think],['SBAR',['S',['NP',['PRP',he]],['VP',['VBD',was],['ADVP',['RB',just]],['VP',['VBG',giving],['NP',['DT',a],['JJ',giant],['JJ',middle],['NN',finger]],['PP',['IN',to],['NP',['DT',the],['NN',audience]]]]]]]]]]).
sample_tree(['CORENLP',['S',['ADVP',['RB','Obviously']],['NP',['NNP','Darth'],['NNP','Vader']],['VP',['VBZ',is],['NP',['NP',['DT',the],['JJS',best]],['CC',and],['NP',['NP',['DT',the],['JJ',original],['JJ',bad],['NN',guy]],['PP',['IN',of],['NP',['NNP','Star'],['NNPS','Wars']]]]]]]]).
sample_tree(['SEQBAR',['CORENLP',['S',['NP',['NNP','James'],['NNP','Earl'],['NNP','Jones']],['VP',['VBZ',does],['NP',['DT',the],['NN',voice]],[',',','],['SBAR',['RB',even],['IN',though],['S',['NP',['PRP',he]],['VP',['VBZ',is],['RB',not],['VP',['VBN',listed],['PP',['IN',in],['NP',['DT',the],['NNS',credits]]]]]]]],['.','.']]],['CORENLP',['S',['NP',['NNP','David'],['NNP','Prowse']],['VP',['VBD',did],['NP',['DT',the],['NN',acting]]]]]]).
sample_tree(['CORENLP',['S',['S',['NP',['PRP','I']],['VP',['VB','\'m'],['ADVP',['RB',still]],['ADJP',['RB',really],['JJ',bummed],['PP',['IN',about],['NP',['DT',that]]]]]],[',',','],['CC',but],['S',['NP',['PRP','I']],['VP',['VB','\'m'],['ADJP',['JJ',sure],['SBAR',['S',['NP',['PRP',they]],['VP',['MD','\'ll'],['VP',['VB',figure],['NP',['NN',something]],['PRT',['RP',out]],['PP',['IN',for],['NP',['NP',['NNP','Leia']],['PP',['IN',in],['NP',['DT','The'],['JJ','Last'],['NNP','Jedi']]]]]]]]]]]]]]).
tree_to_toks1(Tree):-
 print_tree_nl(i=Tree),
 unphrasify(Tree,UTree),
 print_tree_nl(o:-UTree),
 nop((visible_rtrace([+call,+exit],tree_to_toks(Tree,O)),
 notrace(wdmsg(O)))).


contains_phrase(Ls):- sub_term(E,Ls),atom(E),(is_penn_long(E);E=='NP').
contains_phrase(Ls):- member(E,Ls),is_list(E),member(Sl,E),is_list(Sl).

unphrasify([], []) :- !.
%unphrasify([S|Ls], FlatL) :- is_penn_long(S), unphrasify(Ls, FlatL).
unphrasify(['VP'|Ls], FlatL) :- !, unphrasify(Ls, FlatL).
unphrasify(['PP'|Ls], FlatL) :- !, unphrasify(Ls, FlatL).
unphrasify([S|Ls], [mark(S)|FlatL]) :- (is_penn_long(S), contains_phrase(Ls)  ),!, unphrasify(Ls, FlatL).
unphrasify([S|Ls], FlatL) :- S=='NP', sub_var('NP', Ls), unphrasify(Ls, FlatL).
unphrasify([L|Ls], [L|NewLs]) :- 
    dont_flatten(L),!,
    unphrasify(Ls, NewLs),!.
unphrasify([L|Ls], FlatL) :-
    unphrasify(L, NewL),
    unphrasify(Ls, NewLs),
    append(NewL, NewLs, FlatL).
unphrasify(L, [L]).

not_is_list(X):- \+ is_list(X).

dont_flatten([_|L]):- sub_var('NP',L),!, fail.
dont_flatten([S|_]):- is_penn_long(S),!, fail.
dont_flatten([S|_]):- is_penn_tag(S).

tree_to_toks(X,Y):- notrace(unphrasify(X,XX)), tree_to_toks(s,XX,YY),cleanup_toks(YY,Y).
tree_to_toks(C,X,Y):- tree_to_tokz(C,X,M),!,notrace(flatten([M],Y)).

cleanup_toks([],[]).
cleanup_toks([mark(_)|YY],Y):-!,cleanup_toks(YY,Y).
cleanup_toks([np,X,np|YY],[X|Y]):-!,cleanup_toks(YY,Y).
cleanup_toks([np|Rest],[X|Y]):- append(Toks,[np|More],Rest),atomic_list_concat(Toks,'-',X),!,cleanup_toks(More,Y).
cleanup_toks([X|YY],[X|Y]):-!,cleanup_toks(YY,Y).

too_long('CORENLP').
too_long('VP').
too_long('PP').
too_long('NML').
too_long('FRAG').
too_long(X):- atom_concat(_,'BAR',X).
too_long(X):- atom_concat('S',_,X).
is_penn_tag(S):- atom(S),upcase_atom(S,S), S\=='I'.
is_penn_long(S):-is_penn_tag(S),too_long(S).

tree_to_tokz(_,Item,Item):- atomic(Item),!.
tree_to_tokz(C,['NP'|Items],X):- !, tree_l_to_toks(C,Items,List), notrace(undbltok(List,Un)), wrap_seg(np,Un,X).
%tree_to_tokz(C,[_,Item],X):- !, tree_to_tokz(C,Item,X).
tree_to_tokz(C,[S|Items],List):- notrace(is_penn_long(S)), Items\==[], !, tree_to_tokz(C,Items,List).
tree_to_tokz(C,[S|Items],X):- notrace(is_penn_tag(S)), Items\==[], !, tree_l_to_toks(C,Items,List), =(S,D), wrap_seg(D,List,X).
tree_to_tokz(C,Items,Toks):-  is_list(Items),!,tree_l_to_toks(C,Items,List),!,flatten(List,Toks),!.
tree_to_tokz(_C,X,X):- !.

clean_innerd([],[]).
clean_innerd([D,E,D|Inner],[E|ReIn]):-!,clean_innerd(Inner,ReIn).
clean_innerd([S|Inner],[S|ReIn]):- clean_innerd(Inner,ReIn).
wrap_seg(O,List,X):- O\=='np',List=X.
wrap_seg(O,List,X):- append([D|Inner],[D],List),clean_innerd(Inner,ReIn),wrap_seg(O,ReIn,X).
wrap_seg(D,List,X):- append([D|List],[D],X),!.
%wrap_seg(D,List,X):- dbltok(D,List,X).

tree_l_to_toks(C,Items,O):- maplist(tree_to_toks(C),Items,List),flatten(List,O).

assert_training(XX,P,Parse):- assert_if_new(training(XX,P,Parse)),nop(save_training(training/3)).
assert_training_v(XX,P,Parse):- assert_training(XX,P,Parse),dmsg(training(XX,P,Parse)).

do_training(XX,_Str,F2):- training(XX,F2,_),!.
do_training(XX,Str,F2):-
  catch(call(F2,Str,Result),E,(dumpST,format('% % % ERROR: ~p~n',[E --> call(F2,Str,Result)]),fail)),!,
  assert_training(XX,F2,Result),!.

text_to_tree([],[]).
text_to_tree(TokList, Tree):- \+ string(TokList),!, atomics_to_string(TokList,' ',Text),!,text_to_tree(TokList,Text,Tree).
text_to_tree(Text,    Tree):- tokenize_atom(Text,TokList), text_to_tree(TokList,Text,Tree).

text_to_tree(TokList,Text,Tree):- member('"',TokList), !, text_to_best_tree(Text,Tree).
text_to_tree(TokList, _,['SEQBAR',X,Y]):- append(Left,[LE|Right],TokList), Right\==[],
  member(LE,['.','?','!']),append(Left,[LE],Said),!, text_to_tree(Said,X), text_to_tree(Right,Y).
text_to_tree(_TokList,Text,Tree):- text_to_best_tree(Text,Tree),!.
text_to_tree(_TokList,Text,Tree):- text_to_lgp_tree(Text,Tree),!.


all_letters(X):- \+ (upcase_atom(X,U),downcase_atom(X,U)).

retokify([],[]).
retokify([E|APreToks],[sp|PreToks]):- \+ atomic(E), retokify(APreToks,PreToks).
retokify([E|APreToks],[F|PreToks]):- downcase_atom(E,F),retokify(APreToks,PreToks).

add_training_toks(_,[]):- !.
add_training_toks(X,[A]):- !, add_training_toks(X,[A,'.']).
add_training_toks(XX,APreToks):-
 retokify(APreToks,PreToks),
 maplist(add_occurs(is_word),PreToks),
 inc_flag(corpus_training),
 ignore(add_ngrams(except_symbols,trigram,3,skip,PreToks)),
 predbltok(PreToks,ReToks0),
 subst(ReToks0,'.',oc(XX),ReToks1),
 dbltok(oc,ReToks1,ReToks),!,
 XX1 is XX+1,
 append([oc(XX)|ReToks],[oc(XX1)],Grams),!,
 assert_training_v(XX,grams,Grams),
 add_ngrams(except_none,ngram,4,XX,Grams).

add_ngrams(Except,F,N,Loc,Grams):- length(NGram,N),
 append(NGram,_,Mid),
 forall(append(_,Mid,Grams),add_1ngram(Except,F,Loc,NGram)).

except_none(_).
add_1ngram(Except,F,Loc,List):- 
 (Except == except_none ; maplist(Except,List)),!,
 (Loc==skip->W=..[F|List];W=..[F,Loc|List]),
 ngram_inc(W,X),
 (Loc==skip -> (( \+ W -> assert(W) ; true)) ; assert(W)),
 (X=0->(inc_flag(corpus_nodes));inc_flag(corpus_node_overlap)),!.

add_occurs(F,Tok):- P=..[F,Tok],
  ignore(( \+ P, assert(P), inc_flag(corpus_unique_toks) )),
  ngram_inc(P),inc_flag(corpus_total_toks).

except_symbols(X):- \+ (upcase_atom(X,U),downcase_atom(X,U)).

pretok([],[]).
%pretok(['.'],[]):-!.
pretok([X,X,X|Nxt],O):-!,atomic_list_concat([X,X,X],',',Y),pretok([Y|Nxt],O).
pretok([A,'-',S|Grams],[F|ReTok]):- atomic_list_concat([A,S],'-',F),!, pretok(Grams,ReTok).
pretok([A,'\'',S|Grams],[F|ReTok]):- all_letters(A),all_letters(S), atomic_list_concat([A,S],'\'',F),!, pretok(Grams,ReTok).
pretok([A,'´',S|Grams],[F|ReTok]):- all_letters(A),all_letters(S), atomic_list_concat([A,S],'\'',F),!, pretok(Grams,ReTok).
pretok([A,'`',S|Grams],[F|ReTok]):- all_letters(A),all_letters(S), atomic_list_concat([A,S],'\'',F),!, pretok(Grams,ReTok).
%pretok([','|Grams],ReTok):- pretok(Grams,ReTok).
%pretok(['-'|Grams],ReTok):- pretok(Grams,ReTok).
%pretok([A,B,C|Grams],ReTok):- trigram(A,B,C,N), N>40, !,ngram_key(trigram(A,B,C),Key),pretok([Key|Grams],ReTok).
pretok(['!'|Grams],ReTok):- pretok(['.'|Grams],ReTok).
pretok([S|Grams],[S|ReTok]):- pretok(Grams,ReTok).

predbltok([],[]).
predbltok(['.'],[]):-!.
predbltok([X,X,X|Nxt],O):-!,atomic_list_concat([X,X,X],',',Y),predbltok([Y|Nxt],O).
predbltok([A,'-',S|Grams],[F|ReTok]):- atomic_list_concat([A,S],'-',F),!, predbltok(Grams,ReTok).
predbltok([A,'\'',S|Grams],[F|ReTok]):- all_letters(A),all_letters(S), atomic_list_concat([A,S],'\'',F),!, predbltok(Grams,ReTok).
predbltok([A,'´',S|Grams],[F|ReTok]):- all_letters(A),all_letters(S), atomic_list_concat([A,S],'\'',F),!, predbltok(Grams,ReTok).
predbltok([A,'`',S|Grams],[F|ReTok]):- all_letters(A),all_letters(S), atomic_list_concat([A,S],'\'',F),!, predbltok(Grams,ReTok).
predbltok([','|Grams],ReTok):- predbltok(Grams,ReTok).
predbltok(['!'|Grams],ReTok):- predbltok(['.'|Grams],ReTok).
predbltok([S|Grams],[S|ReTok]):- predbltok(Grams,ReTok).

% dbltok(_,X,X):-!.
%dbltok(oc,[],[]):-!.
dbltok(_,[S],[S]):- is_full_tok(S),!.
%dbltok(Pre,[S],[PS]):- atoms_join(Pre,S,PS).
dbltok(Pre,[],[PS]):-!, atoms_join(Pre,oc,PS).
dbltok(Pre,[S|I],[S|O]):- is_full_tok(S),!,dbltok(Pre,I,O).
dbltok(Pre,[S|Grams],[PS|ReTok]):- atoms_join(Pre,S,PS), dbltok(S,Grams,ReTok).



undbltok(I,O):- is_list(I),!,maplist(undbltok,I,O).
undbltok(S,PS):- into_mw(S,[PS|_]),!.
undbltok(S,S):- !.

is_full_tok(O):- atom(O),atomic_list_concat([_,_|_],':',O).

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
:- add_history((autoc([ 'like:you',len(200)]))).
:- add_history((autoc([oc,'like:you',len(200)]))).
:- add_history((autoc([ 'oc:like', 'like:you',len(200)]))).
:- add_history((autoc([ 'like',len(200)]))).
:- add_history((autoc([len(10),like,len(200)]))).
:- add_history(load_training).
:- add_history(compile_corpus).
:- add_history(tree_to_toks).

may_use(Loc,_,B,C,D,_):- \+ used_cl(ngram(A,B,C,D)), assert(used_cl(ngram(A,B,C,D)),Cl2), undo(erase(Cl2)), !.


gen6([A,B,C,D,E,F,G,H]=N):-
  ngram(Loc1,E,F,G,H,Z), ngram(Loc2,C,D,E,F,Y), ngram(Loc3,A,B,C,D,X), N is X+Y+Z.
:- fixup_exports.

dotit:- 
  ignore(( 
    \+ prolog_load_context(reloading, true), 
     ignore(load_training), 
     ignore(compile_corpus))).


