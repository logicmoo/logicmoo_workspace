:- encoding(iso_latin_1).
:- module(pllm,[]).
:- encoding(iso_latin_1).

% :- include(weightless_pllm).

pllm_preds([training/4,training/3,np_save/3,is_word/2,ngram/5,ngram/6,ngram/7,ngram/8,ngram/9,trigram/3,trigram/4,tok_split/3,tok_split/4]).

declare_preds(X):- dynamic(X),multifile(X).

:- pllm_preds(L), maplist(declare_preds,L).

% :- ensure_loaded(trains_trigrams).
:- use_module(library(logicmoo_utils)).

% debug printing
debugln(X):- debugln_xfrm(X,S), dmsg(S).
fmt_pllm(X):- debugln_xfrm(X,S), fmt(S).

debugln_xfrm(Insts,S):- var(Insts), !, sformat(S,"~p",[Insts]).
debugln_xfrm(i(X),S):- \+ is_list(X) -> debugln_xfrm(X,S) ; maplist(debugln_xfrm,X,Y),atomics_to_string(Y,' ',S).
debugln_xfrm([N|A],S):- is_list(A),!,maplist(debugln_xfrm,[N|A],Y),atomics_to_string(Y,' ',S).
debugln_xfrm((F/A),S):- functor(P,F,A),predicate_property(P,number_of_clauses(Insts)),!,sformat(S,"~w=~:d~n",[(F/A),Insts]).
debugln_xfrm(w(E),S):- sformat(S,'~p',E),!.
debugln_xfrm('$'(E),S):- get_flag(E,Insts),!,sformat(S,"~w=~:d~n",[E,Insts]).
debugln_xfrm(N=V,S):- integer(V),!,sformat(S,"~n\t~w\t= ~:d ",[N,V]).
debugln_xfrm(N=V,S):- !,sformat(S,"~n\t~w\t= ~w ",[N,V]).
debugln_xfrm([N],S):- !, debugln_xfrm(N,S).
debugln_xfrm(C,S):- if_defined(tok_split(_,C,S,_)),!.
debugln_xfrm(C,S):- if_defined(tok_split(C,S,_)),!.
debugln_xfrm(C,S):- compound(C),!,sformat(S,"~p",[C]).
%debugln_xfrm(C,S):- compound(C),compound_name_arguments(C,N,A),debugln_xfrm([N|A],S).
debugln_xfrm(nl,'\n'):-!.
debugln_xfrm([],''):-!.
debugln_xfrm(E,E).

:- ensure_loaded(library(logicmoo_nlu)).
:- ensure_loaded(library(logicmoo_nlu/parser_link_grammar)).

%compile_corpus:- functor(P,ngram,6), predicate_property(P,number_of_clauses(N)),N>2.
compile_corpus:- mmake,
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
%train_from_corpus:- train_from_corpus(pldata('corpus/self_dialogue_corpus/train_from.txt')).
train_from_corpus:- train_from_corpus(pldata('corpus/self_dialogue_corpus/train_from_topic_harry_potter.txt')).
%train_from_corpus:- train_from_corpus(pldata('corpus/self_dialogue_corpus/train_from_topic_star_wars.txt')).

in_temp_dir(G):-
 must(absolute_file_name(pldata('corpus/tmpdata'),Dir,[access(read),file_type(directory)])),
 setup_call_cleanup(working_directory(X,Dir),must_or_rtrace(G),working_directory(_,X)).

train_from_corpus(Path):-
 must(absolute_file_name(Path,File,[access(read)])),
 Path\==File,!,
 train_from_corpus(File).

train_from_corpus(File):- atom_concat(File,'.pl',CFile),train_from_corpus(File,CFile).

exists_file_time(File,Time):- exists_file(File),!,time_file(File,Time).
exists_file_time(_,Time):- Time is -1.

train_from_corpus(File,CFile):- fail,
  exists_file_time(CFile,C),
  exists_file_time(File,T),  
  C > T, !,
  debugln(["Corpus up_to_date...",CFile]),
  ensure_loaded(CFile).

train_from_corpus(File,CFile):-
 debugln(["reading corpus...",File]),
 setup_call_cleanup(open(File,read,In), 
 (forall(corpus_stat(Stat),set_flag(Stat,1)),
  set_flag(file_line,1),
  repeat,
  (at_end_of_stream(In) -> ! ; 
  inc_flag(file_line), read_line_to_string(In,Str),get_flag(file_line,X),once(add_training(X,Str)), fail),
  forall(corpus_stat(Stat),(get_flag(Stat,Value),debugln(Stat=Value))),
  save_training_file(CFile),
  debugln(["saving corpus...",CFile])),
 close(In)).

save_training_file(File):-
 tell(File),
  writeln(':- encoding(iso_latin_1).'),
  writeln(':- style_check(- discontiguous).'),
  writeln(':- style_check(- singleton).'),
  format('~n~n',[]),
  pllm_preds(L), maplist(save_listing,L),  
  told.

save_listing(F/A):-
  functor(P,F,A),
  format('~n~n',[]),
  format(':- discontiguous((~q)/~q).~n',[F,A]),
  format(':- multifile((~q)/~q).~n',[F,A]),
  format(':- dynamic((~q)/~q).~n',[F,A]),
  format('~n',[]),
  forall(P,(write_term(P,[quote_non_ascii(true),portrayed(false),no_lists(false),dotlists(false),
   quoted(true),character_escapes_unicode(true),character_escapes(true),
   quote_non_ascii(true),brace_terms(true),ignore_ops(true)]),writeln('.'))),
  format('~n~n',[]).

:- add_history(load_training).
load_training:- in_temp_dir(load_training0).
load_training0:-
  pllm_preds(L),maplist(load_training,L).

load_training(MFA):- !, compute_module(MFA,M,F/A),
 functor(P,F,A),MP=M:P,
 atomic_list_concat_t(['done_',M,'_',F,'_',A,'.pl'],File),
 (predicate_property(MP,number_of_clauses(Before));Before=1),!,
 ignore((exists_file(File) -> ensure_loaded(File) ; true)),
 (predicate_property(MP,number_of_clauses(After));After=1),!,
 debugln(M:F/A=(Before->After)).

compute_module(MFA,M,FA):- strip_module(MFA,M0,FA),compute_m(M0,M),!.

compute_m(user,pllm).
compute_m(M,M).

save_training:- in_temp_dir(save_training0).
save_training0:-
  pllm_preds(L),maplist(save_training,L).
save_training(MFA):- !, compute_module(MFA,M,F/A),
 atomic_list_concat_t(['done_',M,'_',F,'_',A,'.pl'],File),
 tell(File),
  writeq(:- encoding(iso_latin_1)),writeln('.'),
  save_listing(F/A),
 % functor(P,F,A),forall(P,(writeq(P),writeln('.'))),
 told.


save_stat(G):- 
  ( \+ G -> assert(G) ; true),
  nop((writeq(G),writeln('.'))).

:- dynamic(use_extent/2).
%use_extent(is_word,1). 
use_extent(tok_split,3). use_extent(trigram,3). use_extent(ngram,5).
%use_extent(trigram,4). 
%use_extent(ngram,4). 
%use_extent(ngram,5). use_extent(ngram,6). use_extent(ngram,7). use_extent(ngram,8).
compute_corpus_extents:-
 debugln("compute corpus extents..."),
 time((forall(use_extent(F,A),compute_extent(F,A)))).


min_of(X,Y,X):-X<Y,!. min_of(_,Y,Y).
max_of(X,Y,X):-X>Y,!. max_of(_,Y,Y).
inc_flag(F):- flag(F,X,X+1).
compute_extent(F,A):-
  functor(NGram,F,A),
  A2 is A + 1, dynamic(F/A2),
  set_flag(total_fa,1),
  set_flag(min_fa,999999999),
  set_flag(max_fa,1),
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
  set_flag(above_mean_fa, 1), set_flag(above_med_high_fa, 1), set_flag(num_min_fa, 1),
  set_flag(below_mean_fa, 1), set_flag(below_med_low_fa, 1),
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

ngram_key(P,Key):- P=..[ngram,_|Args],!,join_text(Args,Key).
ngram_key(P,Key):- P=..[_|Args],join_text(Args,Key).

join_text(List,Key):- include(all_letters,List,LList),atomic_list_concat_t(LList,',',Key).

save_corpus_stats:-
 time((tell('plm.pl'),
 write('
 :- style_check(- discontiguous).
 :- X= (is_word/2,ngram/6),
    dynamic(X),multifile(X). \n'),
  listing([is_word/2,ngram/6]), 
 told)).

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


do_pre_buffer(XX):-
 findall(['$swap',X|Grams],retract(training(X,temp_grams,Grams)),GramsL),
 append(GramsL,GramsAll),
 add_trainng_graph(XX,GramsAll).

add_training_str(XX,"XXXXXXXXXXX"):- 
  do_pre_buffer(XX),
  inc_flag(corpus_convos),
  set_flag(np_num,1),
  %C = 100_000_000_000, Buffer is floor(XX/C)*C + 01111111111,  
  %ignore(add_conversation_training(Buffer)), !,
  set_flag(speech_act,1),!.
%add_training_str(XX,Str):- 1 is XX mod 2, !, add_training_said(said,"Al",XX,Str),!. 
%add_training_str(XX,Str):- add_training_said(said,"Jo",XX,Str),!. 


add_training_str(XX,Str) :-
 must_det_ll((
   string(Str),
   assert_training(XX,string,Str),
   tokenize_atom(Str,Toks),!,
   pretok(Toks,PreToks0),
   add_punct(PreToks0,PreToks),
  %add_training_toks(XX,PreToks),
  assert_training_tree(XX,PreToks))).


assert_training_tree(XX,PreToks):- 
  must_det_ll((
   text_to_tree(PreToks,Tree),
   assert_training_v(XX,text_to_tree,Tree),
   %writeq(sample_tree(Tree)),writeln('.'),
  unphrasify(Tree,UTree1),
   assert_training(XX,unphrasify,UTree1),
   flatten(UTree1,UTree2),
   exclude(is_penn_tag_t,UTree2,List),
   tree_to_toks(List,PostToks),!,
   assert_training(XX,tree_to_toks,PostToks),
   add_training_toks(XX,PostToks))).

must_det_ll((A,B)):- !, must_det_ll(A),must_det_ll(B).
must_det_ll(A):- catch(A,E,(wdmsg(E),fail)),!.
must_det_ll(A):- rtrace(A).
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
 unphrasify(Tree,UTree1),
 flatten(UTree1,UTree2),
 exclude(is_penn_tag_t,UTree2,UTree),
 print_tree_nl(o:-UTree),
 nop((visible_rtrace([+call,+exit],tree_to_toks(Tree,O)),
 notrace(wdmsg(O)))).
/*


tree_to_toks1(Tree):-
 print_tree_nl(i=Tree),
 unphrasify(Tree,UTree1),
 maplist(unphrasify,UTree1,UTree2),
 unphrasify(UTree2,UTree3),
 funphrasify(UTree3,UTree),
 print_tree_nl(o:-UTree),
 nop((visible_rtrace([+call,+exit],tree_to_toks(Tree,O)),
 notrace(wdmsg(O)))).

*/




funphrasify(UTree3,UTree):- maplist(funphrasify1,UTree3,UTree),!.
funphrasify(UTree3,UTree):- funphrasify1(UTree3,UTree).
%funphrasify1([_,X],O):- funphrasify1(X,O).
funphrasify1(['NP'|X],O):- combined(X,O).
funphrasify1(List,O):- is_list(List),maplist(funphrasify1,List,M),!,funphrasify(M,XO),combined(XO,O).
funphrasify1([O],O).
funphrasify1(O,O).

sub_combine(Ls,O):- unphrasify(Ls, FlatL),combined(FlatL,O),!.

%contains_phrase(Ls):- sub_term(E,Ls),atom(E),(is_penn_long(E);E=='NP').
contains_phrase(O):-  sub_term(Tag,O), (Tag=='NP' ; (compound(Tag), Tag='npo'(_))),!.
%contains_phrase(Ls):- member(E,Ls),is_list(E),member(Sl,E),is_list(Sl).

no_sub(Stuff,E):- sub_var(E,Stuff).

np_like('NML').
np_like('NP').


unphrasify(E,OO):- unphrasify0(E,EO),unphrasify1(EO,EO2),unphrasify2(EO2,OO).

unphrasify0(E,OO):- sub_term(S,E),S=[O,O],subst(E,S,O,MM),!,unphrasify0(MM,OO).
unphrasify0(E,OO):- sub_term(S,E),S=['.',O],subst(E,S,O,MM),!,unphrasify0(MM,OO).
unphrasify0(E,OO):- sub_term(S,E),S=['NP'|O], \+ contains_phrase(O), subst(E,S,npo(O),MM),!,unphrasify0(MM,OO).
unphrasify0(E,OO):- sub_term(S,E),S=['NP'|O],    contains_phrase(O), subst(E,S,oo(O),MM),!,unphrasify0(MM,OO).
%unphrasify0(E,OO):- sub_term(S,E),S=[Tag,O|VP],Tag='VP',subst(E,S,[O|VP],MM),!,unphrasify0(MM,OO).
%unphrasify0(E,OO):- sub_term(S,E),S=[Tag|O],Tag='VP',subst(E,S,O,MM),!,unphrasify0(MM,OO).
unphrasify0(E,OO):- sub_term(S,E),S=[NP,O],atom(O), is_penn_tag_t(NP), subst(E,S,[O],MM),!,unphrasify0(MM,OO).
unphrasify0(E,OO):- sub_term(S,E),S=[NP|O],atom(O), is_penn_tag_t(NP), subst(E,S,O,MM),!,unphrasify0(MM,OO).
unphrasify0(E,E):-!.

%unphrasify2(E,OO):- sub_term(S,E),S=[Tag|O],is_penn_long(Tag),subst(E,S,O,MM),!,unphrasify2(MM,OO).
%unphrasify2(E,OO):- sub_term(S,E),S=[Tag|O],is_penn_long(Tag),subst(E,S,['S',O],MM),!,unphrasify2(MM,OO).

unphrasify1(E,OO):- sub_term(S,E),S=[Tag,O],is_penn_long(Tag),subst(E,S,O,MM),!,unphrasify1(MM,OO).
unphrasify1(E,OO):- sub_term(S,E),S=[NP|O], is_penn_tag_t(NP),subst(E,S,O,MM),!,unphrasify1(MM,OO).
%unphrasify1(List, FlatL) :- is_list(List),maplist(sorta_f,List,FlatL).
unphrasify1(E,OO):- sub_term(S,E),is_list(S), flatten(S,O),O\==S, subst(E,S,O,MM),!,unphrasify1(MM,OO).
unphrasify1(F,FF):- sorta_f(F,FF).

sorta_f(F,FF):- flatten(F,FF).
sorta_f(F,F).

sorta_f2(F,FF):- flatten(F,FF).
sorta_f2(F,F).

unphrasify2(F,O):- is_list(F), maplist(unphrasify2,F,FF),!,flatten(FF,O).
unphrasify2(oo(O),o(OO)):- unphrasify2(O,OO).
unphrasify2(npo(O),np(OO)):- unphrasify2(O,OO).
unphrasify2(F,FF):- sorta_f2(F,FF).
%unphrasify2(E,E):-!.

% unphrasify1([S|Ls], FlatL) :- is_penn_long(S), unphrasify1(Ls, FlatL).
% %unphrasify1(E,OO):- sub_term(S,E),S=[Tag,O],is_penn_tag_t(Tag),\+ np_like(Tag),subst(E,S,O,MM),!,unphrasify1(MM,OO).

% %unphrasify1(E,OO):- sub_term(S,E),S=[NP,O],np_like(NP),atom(O),subst(E,S,O,MM),!,unphrasify1(MM,OO).
% %unphrasify1(E,OO):- sub_term(S,E),S=[Tag|OL],is_penn_tag_t(Tag),maplist(atom,OL),atomic_list_concat_t(OL,'-',O),subst(E,S,O,MM),!,unphrasify1(MM,OO).
%unphrasify1(E,OO):- sub_term(S,E),S=['NP',O],atom(O),subst(E,S,O,MM),!,unphrasify1(MM,OO).
%unphrasify1(E,OO):- sub_term(S,E),S=[Tag|O],is_penn_tag_t(Tag),Tag\=='NP',subst(E,S,O,MM),!,unphrasify1(MM,OO).
%unphrasify1(E,OO):- sub_term(S,E),S=['VP',AV,N],unphrasify1([AV,N],O),subst(E,S,O,MM),!,unphrasify1(MM,OO).
%unphrasify1(E,OO):- sub_term(S,E),S=['NP'|Stuff],\+ no_sub(Stuff,'NP'),tree_to_toks(Stuff,O),subst(E,S,O,MM),unphrasify1(MM,OO).
/*
%unphrasify1(['NP'|Ls], O) :- unphrasify1(Ls, FlatL),tree_to_toks(FlatL,O).
unphrasify1(['NP'|Ls1], O) :- sub_combine(Ls1,O).
%unphrasify1(['ADVP'|Ls1], O) :- maplist(unphrasify1,Ls1,Ls), unphrasify1(Ls, FlatL),tree_to_toks(FlatL,O),!.
unphrasify1(['PP'|Ls1], O) :- sub_combine(Ls1,O).
unphrasify1(['ADVP'|Ls1], O) :- sub_combine(Ls1,O).
unphrasify1(['ADJP'|Ls1], O) :- sub_combine(Ls1,O).
%unphrasify1(E,OO):- wdmsg(unphrasify1(E,OO)),fail.
unphrasify1(['VP'|Ls], FlatL) :- !, unphrasify1(Ls, FlatL).
unphrasify1(['PP'|Ls], FlatL) :- !, unphrasify1(Ls, FlatL).
unphrasify1([S|Ls], [mark(S)|FlatL]) :- (is_penn_long(S), contains_phrase(Ls)  ), unphrasify1(Ls, FlatL),!.
unphrasify1([S|Ls], FlatL) :- S=='NP', sub_var('NP', Ls), unphrasify1(Ls, FlatL).
unphrasify1([L|Ls], [L|NewLs]) :- 
    dont_flatten(L),!,
    unphrasify1(Ls, NewLs),!.
unphrasify1([L|Ls], FlatL) :-
    unphrasify1(L, NewL),
    unphrasify1(Ls, NewLs),
    append([NewL], NewLs, FlatL).
unphrasify1(L, O):- is_list(L),tree_to_toks_only(L,O),!.
*/


no_dash(MM):- \+ atom_contains(MM,'-').

combine_dash(MM,O):- maplist(no_dash,MM),atomic_list_concat_t(MM,'-',O),!.
combine_dash(MM,MM).

combined(M,O):- tree_to_toks_f(M,MM),combine_dash(MM,O).
tree_to_toks_f(M0,O):- tree_to_toks(M0,MM),flatten([MM],O),!.

not_is_list(X):- \+ is_list(X).

dont_flatten([_|L]):- sub_var('NP',L),!, fail.
dont_flatten([S|_]):- is_penn_long(S),!, fail.
dont_flatten([S|_]):- is_penn_tag_t(S).

tree_to_toks_only(X,Y):- tree_to_toks(s,X,YY),cleanup_toks(YY,Y).
tree_to_toks(X,Y):- notrace(unphrasify(X,XX)),tree_to_toks_only(XX,Y).
tree_to_toks(C,X,Y):- tree_to_tokz(C,X,M),!,notrace(flatten([M],Y)).

cleanup_toks([],[]).
cleanup_toks([mark(_)|YY],Y):-!,cleanup_toks(YY,Y).
cleanup_toks([np,X,np|YY],[X|Y]):-!,cleanup_toks(YY,Y).
cleanup_toks([np|Rest],[X|Y]):- append(Toks,[np|More],Rest),
  include(all_letters,Toks,ToksA),
  maplist(no_dash,ToksA), ToksA\==[],atomic_list_concat_t(ToksA,'-',X),!,cleanup_toks(More,Y).
cleanup_toks([X|YY],[X|Y]):-!,cleanup_toks(YY,Y).

too_long('CORENLP').
too_long('VP').
too_long('PP').
too_long('NML').
too_long('FRAG').
too_long(X):- atom_concat(_,'BAR',X).
too_long(X):- atom_concat('S',_,X).
is_penn_tag_t(S):- atom(S),upcase_atom(S,S), \+ downcase_atom(S,S), S\=='I'.
is_penn_long(S):-is_penn_tag_t(S),too_long(S).

tree_to_tokz(_,Item,Item):- atomic(Item),!.
tree_to_tokz(C,E,OO):- sub_term(S,E),S=[Tag,O],is_penn_tag_t(Tag),atom(O),subst(E,S,O,MM),!,tree_to_tokz(C,MM,OO).
/*
tree_to_tokz(C,['NP'|Items],X):- !, tree_l_to_toks(C,Items,List), notrace(undbltok(List,Un)), wrap_seg(np,Un,X).
tree_to_tokz(C,[mark(SBAR)|Items],X):- !, tree_to_tokz(C,[SBAR|Items],X).
*/tree_to_tokz(C,mark(SBAR),X):- !, tree_to_tokz(C,SBAR,X).

%tree_to_tokz(C,[_,Item],X):- !, tree_to_tokz(C,Item,X).
%tree_to_tokz(C,[S|Items],List):- notrace(is_penn_long(S)), Items\==[], !, tree_to_tokz(C,Items,List).
%tree_to_tokz(C,[S|Items],X):- notrace(is_penn_tag_t(S)), Items\==[], !, tree_l_to_toks(C,Items,List), =(S,D), wrap_seg(D,List,X).
%tree_to_tokz(C,Items,Toks):-  is_list(Items),!,tree_l_to_toks(C,Items,List),!,flatten(List,Toks),!.
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
assert_training_v(XX,P,Parse):- assert_training(XX,P,Parse),debugln(training(XX,P,Parse)).

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

%all_letters(X):- compound(X),X=np(_),!.
%all_letters(X):- compound(X),X=o(_),!.
all_letters(X):- \+ compound(X), \+ (upcase_atom(X,U),downcase_atom(X,U)).

retokify([],[]).
retokify([','|APreToks],PreToks):- retokify(APreToks,PreToks).
retokify([X,X|APreToks],PreToks):- retokify([X|APreToks],PreToks).

retokify([E|APreToks],[E|PreToks]):- ( \+ atomic(E) ),!,retokify(APreToks,PreToks).
retokify([E|APreToks],PreToks):- ( \+ atomic(E); is_penn_tag_t(E) ),!,retokify(APreToks,PreToks).
retokify([E|APreToks],[F|PreToks]):- downcase_atom(E,F),retokify(APreToks,PreToks).

add_training_toks(_,[]):- !.
add_training_toks(X,[A]):- !, add_training_toks(X,[A,'.']).
add_training_toks(XX,APreToks):-
 must_det_l((
  notrace((retokify(APreToks,PreToks), maplist(add_occurs(is_word),PreToks),inc_flag(corpus_training), XX1 is XX+1)),
 notrace((subst(PreToks,'.',(XX),PreToks1),
 %subst(PreToks1,',','$dead',PreToks2),
 subst(PreToks1,'?',['?',XX1],PreToks3),
 flatten(PreToks3,PreToks333),
 PreToks333=PreToks33,
 %get_flag(np_num,NPNUM),
 %set_np_nums(XX,NPNUM,PreToks333,PreToks33),
 (contains_var(XX1,PreToks33) -> append([XX|PreToks33],[],ReToks) ; append([XX|PreToks33],[(XX1)],ReToks))
 )),!,
 predbltok(ReToks,_Grams),
 assert_training(XX,temp_grams,PreToks33))),!.
 

add_trainng_graph(_,[]):- !.
add_trainng_graph(X,[A]):- !, add_trainng_graph(X,[A,'.']).
add_trainng_graph(XX,APreToks):-
 must_det_l((
 retokify(APreToks,ReToks),
 predbltok(ReToks,Grams),
 assert_training_v(XX,grams,Grams))),!,
 forall(between(3,7,N),add_ngrams(except_none,ngram,N,XX,Grams)),!.


np_saver(XX,N,K,O):- asserta_if_new(np_save(XX,N,K)),O=N.

set_np_nums(_,_,[],[]).
set_np_nums(XX,N,['$dead'|PreToks3],PreToks33):-
  set_np_nums(XX,N,PreToks3,PreToks33).
set_np_nums(XX,N,[np(K)|PreToks3],[np(O)|PreToks33]):-
  np_saver(XX,N,K,O),
  NN is N +1,
  set_flag(np_num,NN),
  set_np_nums(XX,NN,PreToks3,PreToks33).
set_np_nums(XX,N,[o(K)|PreToks3],[o(O)|PreToks33]):-
  np_saver(XX,N,K,O),
  NN is N +1,
  set_flag(np_num,NN),
  set_np_nums(XX,NN,PreToks3,PreToks33).
set_np_nums(XX,N,[K|PreToks3],[K|PreToks33]):-
  set_np_nums(XX,N,PreToks3,PreToks33).


add_ngrams(_Except,_F,N,_Loc,Grams):- length(Grams,L),L<N,!.

add_ngrams(Except,F,N,_Loc,Grams):- 
 length(NGram,N),
 append(NGram,_,Mid),
 forall(append(_,Mid,Grams),add_1ngram(Except,F,skip,NGram)).
/*
add_ngrams(Except,F,N,Loc,Grams):- 
 length(NGram,N),
 append(NGram,_,Grams),
 once(add_1ngram(Except,F,Loc,NGram)), fail.
add_ngrams(Except,F,N,Loc,Grams):- 
 length(NGram,N),
 append(_,NGram,Grams),
 once(add_1ngram(Except,F,Loc,NGram)), fail.
add_ngrams(_,_,_,_,_).
*/
except_none(_).
add_1ngram(Except,_F,_Loc,List):-  Except \== except_none , \+ maplist(Except,List),!.
add_1ngram(_Except,F,Loc,List):- 
 (Loc==skip->P=..[F,Was|List];P=..[F,Was,Loc|List]),
 (Loc==skip->Q=..[F,N|List];Q=..[F,N,Loc|List]),
  ngram_inc(P),!,
  (clause(P,true,Ref)
   -> (erase(Ref),N is Was+1, assertz(Q),inc_flag(corpus_node_overlap))
    ; (N=1, assertz(Q), inc_flag(corpus_nodes))),!.

add_occurs(F,Tok):- P=..[F,Was,Tok],Q=..[F,N,Tok],
  (clause(P,true,Ref)
   -> (erase(Ref),N is Was+1, assert(Q))
   ; (N=1, assert(Q), inc_flag(corpus_unique_toks))),
  inc_flag(corpus_total_toks).

except_symbols(X):- \+ (upcase_atom(X,U),downcase_atom(X,U)).

atomic_list_concat_t(A,B,C):- catch(atomic_list_concat(A,B,C),_,fail),!.
atomic_list_concat_t(A,B,C):- rtrace(atomic_list_concat(A,B,C)),!.
atomic_list_concat_t(A,C):- catch(atomic_list_concat(A,C),_,fail),!.
atomic_list_concat_t(A,C):- rtrace(atomic_list_concat(A,C)),!.
pretok([],[]).
%pretok(['.'],[]):-!.
pretok([X|Nxt],[X|O]):- compound(X),pretok(Nxt,O).
pretok([A,X|Nxt],[A,X|O]):- compound(X),pretok(Nxt,O).

pretok([X,X,X|Nxt],O):-!,atomic_list_concat_t([X,X,X],',',Y),pretok([Y|Nxt],O).
pretok([A,'-',S|Grams],[F|ReTok]):- atomic_list_concat_t([A,S],'-',F),!, pretok(Grams,ReTok).
pretok([A,'\'',S|Grams],[F|ReTok]):- all_letters(A),all_letters(S), atomic_list_concat_t([A,S],'\'',F),!, pretok(Grams,ReTok).
pretok([A,'´',S|Grams],[F|ReTok]):- all_letters(A),all_letters(S), atomic_list_concat_t([A,S],'\'',F),!, pretok(Grams,ReTok).
% backtick
pretok([A,B,S|Grams],[F|ReTok]):- atom(B),name(B,[96]),all_letters(A),all_letters(S), atomic_list_concat_t([A,S],'\'',F),!, pretok(Grams,ReTok).
%pretok([','|Grams],ReTok):- pretok(Grams,ReTok).
%pretok(['-'|Grams],ReTok):- pretok(Grams,ReTok).
%pretok([A,B,C|Grams],ReTok):- trigram(A,B,C,N), N>40, !,ngram_key(trigram(A,B,C),Key),pretok([Key|Grams],ReTok).
pretok(['!'|Grams],ReTok):- pretok(['.'|Grams],ReTok).
pretok([S|Grams],[S|ReTok]):- pretok(Grams,ReTok).

predbltok([],[]).
predbltok(['.'],[]):-!.
predbltok([X,X|Nxt],O):-number(X),predbltok([X|Nxt],O).
predbltok([X|Nxt],[X|O]):- compound(X),predbltok(Nxt,O).
predbltok([A,X|Nxt],[A,X|O]):- compound(X),predbltok(Nxt,O).
predbltok([A,B,X|Nxt],[A,B,X|O]):- compound(X),predbltok(Nxt,O).
predbltok([X,Y|Nxt],O):-number(X),number(Y),X<Y,!,predbltok([Y|Nxt],O).
predbltok([X,X,X|Nxt],O):-!,atomic_list_concat_t([X,X,X],',',Y),predbltok([Y|Nxt],O).
predbltok([A,'-',S|Grams],[F|ReTok]):- atomic_list_concat_t([A,S],'-',F),!, predbltok(Grams,ReTok).
predbltok([A,'\'',S|Grams],[F|ReTok]):- all_letters(A),all_letters(S), atomic_list_concat_t([A,S],'\'',F),!, predbltok(Grams,ReTok).
predbltok([A,'´',S|Grams],[F|ReTok]):- all_letters(A),all_letters(S), atomic_list_concat_t([A,S],'\'',F),!, predbltok(Grams,ReTok).
predbltok([A,B,S|Grams],[F|ReTok]):- atom(B), name(B,[96]), all_letters(A),all_letters(S), atomic_list_concat_t([A,S],'\'',F),!, predbltok(Grams,ReTok).

predbltok([','|Grams],ReTok):- predbltok(Grams,ReTok).
predbltok(['!'|Grams],ReTok):- predbltok(['.'|Grams],ReTok).
predbltok([S|Grams],[S|ReTok]):- predbltok(Grams,ReTok).

% dbltok(_,X,X):-!.
dbltok(_,[X],[X]):-number(X),!.
%dbltok(_,[S],[S]):- is_full_tok(S),!.
%dbltok(Pre,[S],[PS]):- atoms_join(Pre,S,PS).
%dbltok(Pre,[],[PS]):-!, atoms_join(Pre,,PS).
dbltok(Pre,[S|I],[S|O]):- is_full_tok(S),!,dbltok(Pre,I,O).
dbltok(Pre,[S|Grams],[PS|ReTok]):- atoms_join(Pre,S,PS), dbltok(S,Grams,ReTok).



undbltok(I,O):- is_list(I),!,maplist(undbltok,I,O).
undbltok(S,PS):- into_mw(S,[PS|_]),!.
undbltok(S,S):- !.

is_full_tok(O):- compound(O),!.
is_full_tok(O):- atom(O),atomic_list_concat_t([_,_|_],':',O).

atoms_join(A,B,O):- tok_split(Was,O,A,B),!,N is Was+1,assert(tok_split(N,O,A,B)).
atoms_join(A,B,O):- atomic_list_concat_t([A,B],':',O),!,assert(tok_split(1,O,A,B)).

% @TODO use average 
%as_good(T,X):- is_word(T,X),(Nxt>500->X=1;X is 500-Nxt).
%ngram_rate(A,B,C,D,N,NN):- ngram(N,Loc,A,B,C,D), maplist(as_good,[A,B,C,D],Num), sumlist(Num,NN).

add_blanks(N,S,Slotted):- \+ is_list(S),!,add_blanks(N,[S],Slotted).
add_blanks(_,[],[]):-!.

add_blanks(N,[A,B|Sent],[O|Slotted]):- tok_split(_Cnt,O,A,B),!,add_blanks(N,Sent,Slotted).
add_blanks(N,[S|Sent],[O|Slotted]):- \+ \+ tok_split(_,_,S,_),!, tok_split(_Cnt,O,S,_),add_blanks(N,Sent,Slotted).
add_blanks(N,[O|Sent],[O|Slotted]):- atom(O), tok_split(_,O,_,_),!,add_blanks(N,Sent,Slotted).

add_blanks(N,[len(S)|Sent],Slotted):- integer(S),length(L,S),!,add_blanks(N,Sent,Mid),append(L,Mid,Slotted).
add_blanks(N,[S|Sent],[A|Slotted]):- string(S),atom_string(A,S),!,add_blanks(N,Sent,Slotted).
add_blanks(N,[S|Sent],Slotted):- var(S),!,between(1,N,L),add_blanks(N,[1-L|Sent],Slotted).
add_blanks(N,[Lo-Hi|Sent],Slotted):- (integer(Lo);integer(Hi)),!,between(Lo,Hi,L),length(S,L),add_blanks(N,Sent,Mid),append(S,Mid,Slotted).
add_blanks(N,[S|Sent],Slotted):- is_list(S),!,flatten(S,SL),append(SL,Sent,SLSent),!,add_blanks(N,SLSent,Slotted).
add_blanks(N,[S|Sent],Slotted):- atom(S),into_mw(S,SL),!,append(SL,Sent,SLSent),!,add_blanks(N,SLSent,Slotted).
add_blanks(N,[S|Sent],[S|Slotted]):- add_blanks(N,Sent,Slotted).

into_mw(S,SL):- into_mw0(S,SL),SL\==[S],!.
into_mw0(S,SL):- atomic_list_concat_t([M,_|_],':',S),!,into_mw0(M,SL).
into_mw0(S,SL):- atomic_list_concat_t(SL,',',S).
into_mw0(S,SL):- atomic_list_concat_t(SL,' ',S).
into_mw0(S,SL):- atomic_list_concat_t(SL,'_',S).

loc_dists(Loc1,Loc2, NN):- NN is abs(Loc1-Loc2).
loc_dists(Loc1,Loc2,Loc3, NN):- NN is (abs(Loc1-Loc2) + abs(Loc3-Loc2) + abs(Loc1-Loc3))/3.

%:- pllm:ensure_loaded(plm).
% added for conversations
ngraml1(Loc,A,(X),B,C,NN):- nonvar(X), ngram(Loc,_,_,A,(X),_),ngram(_ULoc,(X),B,C,_,NN).
ngraml1(Loc,A,B,(X),C,NN):- nonvar(X), ngram(Loc,_,A,B,(X),_),ngram(_ULoc,(X),C,_,_,NN).

autoc(Sent):- autoc(1,Sent).
autoc(N,Sent):- 
  retractall(used_cl(ngram(_,_,_,_))),
  add_blanks(N,Sent,Slotted),no_repeats( map_sent(_,_Loc,Slotted)),fmt_pllm(Slotted).

good_toks(Key,E):- functor(P,ngram,6),arg(6,P,E),no_repeats(Key,(P,ngram_key(P,Key))).

lg(X):- \+ is_list(X),!,between(22,66,L),lg(L,X).
lg(X):-lg0(X),write_phr(X).

lg(L,X):- var(L),!, lg0(X), length(X,L),write_phr(X).
lg(L,X):- length(X,L),lg0(X),write_phr(X).

write_phr(X):- is_list(X),!,maplist(write_phr0,X),nl.
write_phr(X):- write_phr0(X),nl.

write_phr0(X):- is_list(X),!,maplist(write_phr0,X).
write_phr0(np(X)):- write_phr0((X)).
write_phr0(o(X)):- write_phr0((X)).
write_phr0(X):- number(X).
write_phr0('$swap'):- nl.
write_phr0(X):- write(X),write(' ').

lg0(X):- freeze(N,number(N)),lg1([N|X]),last(X,E),number(E).

lg1([_,N]):- number(N),!.
lg1([_,_,N]):- number(N),!.
lg1([Z,X,Y,A,B,C|More]):- lgram(Z,X,Y,A,B,C),lg1([A,B,C|More]).
lg1([X,Y,A,B,C|More]):- lgram(X,Y,A,B,C),lg1([A,B,C|More]).
lg1([X,Y,A,B|More]):- lgram(X,Y,A,B),lg1([A,B|More]).

lg1([_,_,_]).
lg1([_,_]).



:- add_history(recompile_corpus).

is_word(_):- dumpST,break.
/*
how are you?  i am fine
 V
1000 how are you ? 1001 i am fine 1002
 V
1000 how:are are:you you:? 1001 i:am am:fine 1002
*/

scene_info( 'Smallville_S03E14_scene_12_with_2_characters_Chloe_Clark', 2, ['Chloe','Clark'], [Chloe,Clark], [

   Chloe:      ['I',am,really,sorry,she,went,all,'Glenn','Close',on,you,'.'],
   Clark:      ['I',should,have,told,you,about,'Alicia','\'s',ability,before,'.'],
   Clark:      ['But',she, had, asked,me,to,keep,it,a,secret,'.'],
   Chloe:      ['Yeah',',',and,'I',respect,you,for,keeping,her,confidence,'.'],
   Chloe:      ['But',once,she,went,psycho,',',all,bets,are,off,'.'],
   Clark:      ['So',how,do,you,look,for,someone,who,can,disappear,in,the,blink,of,an,eye,whenever,she,wants,?],
   Chloe:      ['Well',',',we,know,that,'Alicia',has,at,least,one,weakness,'.'],
   Chloe:      ['You','.'],
   Clark:      ['We',may,have,one,more,'.'],
   Clark:      [Chloe,',','I',have,an,idea,'.'],
   Clark:      ['I',am,going,to,need,your,help,'.']
 ]).

two_way_convo("
  So, I watched the Force Awakens, I am lost.
  By what? Good film.
  I liked it, but was the force sleeping before?
  No, it just means that a new Jedi had emerged.
  Well, if it is Rey, then why did it not wake when Klyo came into power.
  Klyo went to the dark side, he is not a Jedi.
  If he is not a Jedi, why was he at Jedi school.
  He was Luke's Padwan, but he turned. It has not been shown why. He is no longer Jedi, he is sith now.
  Wait, I thought sith was a race.
  No, Vader was human, well pretty much, but went from Jedi to Sith. Like Yoda is whatever he is, but is also a Jedi.
").

use_scene_info:- 
  G=scene_info(_Name,_,_Chars,_Vars,_Events),
  forall(G,use_scene_info(G)).

% /opt/logicmoo_workspace/packs_xtra/logicmoo_chat/corpus/soap_opera_corpus/
fix_scene_events(W:action(Action),W:[action(WAction)]):- !, term_to_atom(W,WW), fix_scene_events([WW|Action],WAction).
fix_scene_events(W:Says,W:Said):- !, fix_scene_events(Says,Said).
fix_scene_events(L,LL):- is_list(L), maplist(any_to_atom,L,LLL),!,text_to_tree( LLL,LL).
fix_scene_events(Says,Said):- text_to_tree( Says,Said).

use_scene_info(scene_info(_Name,_,_Chars,Vars,AllEvents)):- 
  numbervars(Vars,1,_,[attvar(skip)]),
  maplist(fix_scene_events,AllEvents,AllEvents2),
  [Who1:Did|Events] = AllEvents2,
  cobined_sers(Ser,_),
  combine_whos(Who1,[Ser|Did],Events,LinearEvents),
  use_linear_events(LinearEvents). 

use_linear_events(LE):- wdmsg(LE).

combine_whos(Who,Did,[],[Who:Did]):- !.
combine_whos(Who,Did,[Who1:Does|Events],LinearEvents):- Who==Who1,!,
  combine_events(Did,Does,DidDoes),
  combine_whos(Who1,DidDoes,Events,LinearEvents).
combine_whos(Who,Did,[Who1:Does|Events],[Who:Did|LinearEvents]):- Who\==Who1,!,
  cobined_sers(Ser,_),
  combine_whos(Who1,[Ser|Does],Events,LinearEvents).

cobined_sers(Ser,Ser2):-
  flag(says,SayDo,SayDo+1), Ser is SayDo + 10000,
  Ser2 is Ser + 2.

combine_events(Did,Does,DidDoes):- \+ is_list(Did),!,combine_events([Did],Does,DidDoes).
combine_events(Did,Does,DidDoes):- \+ is_list(Does),!,combine_events(Did,[Does],DidDoes).
combine_events(Did,Does,DidDoes):- append(Did,Does,DidDoes).

lst_3_2([_,_,_]).
lst_3_2([_,_]).

lst_2_3([_,_]).
lst_2_3([_,_,_]).

%lst_4_1(NV):- nonvar(NV),!.
lst_4_1([_,_,_,_]).
lst_4_1([_,_,_]).
lst_4_1([_,_]).
lst_4_1([_]).


% 4 - 12
s1(List):-
 rnd_cl(lst_4_1(A), lst_3_2(B), lst_4_1(C)),
 append([A,B,C],List),
 sent(A,B,C).

% 8 - 15
s2(List):- 
 rnd_cl(lst_4_1(A),lst_3_2(C),lst_4_1(E)),
 rnd_cl(lst_3_2(B),lst_3_2(D)),
 append([A,B,C,D,E],List),
 sent(A,B,C),sent(C,D,E).


% 12 - 35
s3(List):- 
 rnd_cl(lst_3_2(C), lst_3_2(E)), 
 (nonvar(List)->append([A,B,C,D,E,F,G],List);true),
 % rnd_cl(lst_4_1(A), lst_4_1(G)), rnd_cl(lst_3_2(B), lst_3_2(D), lst_3_2(F)),
 sent(A,B,C),sent(C,D,E),sent(E,F,G),
 append([A,B,C,D,E,F,G],List).

lgram(X):- rnd_cl(lgram0(X)).
lgram0([X,Y,Z,A,B,C,D]):- var_or_rand(ngram(_,_,X,Y,Z,A,B,C,D)).
lgram0([X,Y,Z,A,B,C]):- var_or_rand(ngram(_,_,X,Y,Z,A,B,C)).
lgram0([X,Y,Z,A,B]):- var_or_rand(ngram(_,_,X,Y,Z,A,B)).
lgram0([X,Y,Z,A]):- var_or_rand(ngram(_,_,X,Y,Z,A)).
lgram0([X,Y,Z]):- var_or_rand(ngram(_,_,X,Y,Z)).


lgram(X,Y,Z,A,B,C,D):- var_or_rand(ngram(_,_,X,Y,Z,A,B,C,D)).
lgram(X,Y,Z,A,B,C):- var_or_rand(ngram(_,_,X,Y,Z,A,B,C)).
lgram(X,Y,Z,A,B):- var_or_rand(ngram(_,_,X,Y,Z,A,B)).
lgram(X,Y,Z,A):- var_or_rand(ngram(_,_,X,Y,Z,A)).
lgram(X,Y,Z):- var_or_rand(ngram(_,_,X,Y,Z)).

has_nonvar(P):- arg(_,P,E),nonvar(E),E\==[],!.
var_or_rand(P):- has_nonvar(P)->P;rnd_cl(P).
%var_or_rand(P):- rnd_cl(P).
var_or_once(Mid,P):- (var(Mid)->rnd_cl(P);once(P)).

sent(LeftMid,Left,Mid,Right,MidRight):- 
  var_or_once(Mid,lst_3_2(Mid)),
  var_or_once(Left,lst_4_1(Left)),append(Left,Mid,LeftMid),apply(lgram,LeftMid),
  var_or_once(Right,lst_4_1(Right)),append(Mid,Right,MidRight),apply(lgram,MidRight),  
  true.
sent(Left,Mid,Right):- sent(_,Left,Mid,Right,_).


sent(Sent):- s1(Sent).
sent(Sent):- s2(Sent).
sent(Sent):- s3(Sent).

:- fixup_exports.



:-dynamic(used_cl/1).

map_sent(_,_,Sent):- ground(Sent),!.
map_sent(LR,Loc,Sent):- var(Sent), length(Sent,9),map_sent(LR,Loc,Sent).
map_sent(LR,Loc,List):- LR=lr,append(Left,[X|More],List),nonvar(X),Left\==[],!,map_sent(LR,Loc,[X|More]),map_sent(rl,Loc,List).
map_sent(LR,Loc,[A,B,C,D|More]):- some_ngram(Loc,A,B,C,D,_Fire), map_sent(LR,Loc,[C,D|More]).
map_sent(LR,Loc,[A,B,C,D|More]):- some_ngram(Loc,A,B,C,_,_Fire), map_sent(LR,Loc,[B,C,D|More]).
map_sent(_,Loc,List):- ABCDO=[_,_,_,_,_Occurs],append(List,_,ABCDO), apply(some_ngram,[Loc|ABCDO]).


some_ngram(N,_PrevLoc,A,B,C,D):- pick_ngram(N,Loc,A,B,C,D),may_use(N,Loc,A,B,C,D).

pick_ngram(N,Loc,A,B,C,D):- maplist(var,[A,B,C,D])->rnd_ngram(N,Loc,A,B,C,D);ngram(N,Loc,A,B,C,D).

rnd_ngram(N,Loc,A,B,C,D):-  rnd_cl(ngram(N,Loc,A,B,C,D)).

rnd_cl(G):- 
 predicate_property(G,number_of_clauses(R)),
 randseq(R,R,Seq),member(CN,Seq),
 nth_clause(G,CN,Ref),clause(G,Body,Ref),Body.

rnd_cl(G1,G2):- 
 predicate_property(G1,number_of_clauses(N1)), 
 predicate_property(G2,number_of_clauses(N2)), 
 rand_xy(N1,N2,List), member(X+Y,List),
 nth_clause(G1,X,Ref1),clause(G1,Body1,Ref1),
 nth_clause(G2,Y,Ref2),clause(G2,Body2,Ref2),
 Body1, Body2.

rnd_cl(G1,G2, G3):- 
 predicate_property(G1,number_of_clauses(N1)), 
 predicate_property(G2,number_of_clauses(N2)),
 predicate_property(G3,number_of_clauses(N3)),
 rand_xyz(N1,N2,N3,List), member(X+Y+Z,List),
 nth_clause(G1,X,Ref1),clause(G1,Body1,Ref1),
 nth_clause(G2,Y,Ref2),clause(G2,Body2,Ref2),
 nth_clause(G3,Z,Ref3),clause(G3,Body3,Ref3),
 Body1, Body2, Body3.


rand_x(X,Seq):- randseq(X,X,Seq).
rand_xy(X,Y,Rnd):-  rand_x(X,Seq), assign_rands(Seq,Y,List), flatten(List,Set),random_permutation(Set,Rnd).
rand_xyz(X,Y,Z,Rnd):- rand_xy(X,Y,Seq), assign_rands(Seq,Z,List), flatten(List,Set),random_permutation(Set,Rnd).

assign_rand(E,R,EE):- rand_x(R,Seq),add_pair(Seq,E,EE).
add_pair([],_,[]).
add_pair([X|Seq],Y,[Y+X|ER]):- add_pair(Seq,Y,ER).

assign_rands([],_,[]).
assign_rands([E|L],R,[ER|LL]):-
  assign_rand(E,R,ER),
  assign_rands(L,R,LL).


:- style_check(- singleton).


:- add_history((good_toks(Key,E),E>20)).
:- add_history((autoc([ 'like:you',len(200)]))).
:- add_history((autoc([ ':like', 'like:you',len(200)]))).
:- add_history((autoc([ 'like',len(200)]))).
:- add_history((autoc([len(10),like,len(200)]))).
:- add_history(load_training).
:- add_history(tree_to_toks).
:- add_history(compile_corpus).

may_use(Loc,_,B,C,D,_):- \+ used_cl(lgram(A,B,C,D)), assert(used_cl(lgram(A,B,C,D)),Cl2), undo(erase(Cl2)), !.


gen6([A,B,C,D,E,F,G,H]=N):-
  ngram(_,Loc1,E,F,G,H,Z), ngram(_,Loc2,C,D,E,F,Y), ngram(_,Loc3,A,B,C,D,X), N is X+Y+Z.

:- ensure_loaded(trains_trigrams).

:- fixup_exports.

dotit:- 
  ignore(( 
    \+ prolog_load_context(reloading, true), 
     ignore(load_training), 
     ignore(compile_corpus))).

%:- ensure_loaded(pldata('corpus/soap_opera_corpus/so_convert')).
:- if(\+ prolog_load_context(reloading, true)).
:- compile_corpus.
:- endif.

