% load.pl : Load Chat-80, for Quintus Prolog

/*
 _________________________________________________________________________
|	Copyright (C) 1982						  |
|									  |
|	David Warren,							  |
|		SRI International, 333 Ravenswood Ave., Menlo Park,	  |
|		California 94025, USA;					  |
|									  |
|	Fernando Pereira,						  |
|		Dept. of Architecture, University of Edinburgh,		  |
|		20 Chambers St., Edinburgh EH1 1JZ, Scotland		  |
|									  |
|	This program may Be used, copied, altered or ensure_loadedd in other	  |
|	programs only for academic purposes and provided that the	  |
|	authorship of the initial program is aknowledged.		  |
|	Use for commercial purposes without the previous written 	  |
|	agreement of the authors is forbidden.				  |
|_________________________________________________________________________|

*/


:- use_module(library(statistics)).

%:- autoload_all.
:- use_module(library(logicmoo_common)).
:- use_module(library(logicmoo_nlu)).
:- use_module(library(logicmoo_clif)).
%:- xlisting(lock_predicate/1).
%:- autoload_all.


:- module(parser_chat80).
:- '$set_source_module'(parser_chat80).

add_c80(B):- any_to_string(B,S),add_history1(c80(S)).

is_loading_file:- prolog_load_context(reloading, true),!.
is_loading_file:- prolog_load_context(file,_), prolog_load_context(term,T), T\==[].


c80(B):- is_loading_file, !, add_c80(B).
c80(Text):- 
  cls, make, tracing ~= on,  
  any_to_string(Text,S),!,
  add_c80(S),
  c81(S).


/*

not_is_list(X):- \+ is_list(X).

*/
contains_phrase(Ls):- sub_term(E,Ls),atom(E),(is_penn_long(E);E=='NP').
contains_phrase(Ls):- member(E,Ls),is_list(E),member(Sl,E),is_list(Sl).

too_long('CORENLP').
too_long('VP').
too_long('PP').
too_long('NML').
too_long('FRAG').
too_long(X):- atom_concat(_,'BAR',X).
too_long(X):- atom_concat('S',_,X).
is_penn_tag(S):- atom(S),upcase_atom(S,S), \+ downcase_atom(S,S), S\=='I'.
is_penn_long(S):-is_penn_tag(S),too_long(S).

pre_tree(I,O):- \+ is_list(I),!,I=O.
pre_tree([], []) :- !.
pre_tree(['VP',['NN',Border]|I],['VP',['VB',Border]|O]):-maplist(pre_tree,I,O).
pre_tree([S|I], [TAG|O]) :-  is_penn_tag(S),s_c_code(S,TAG),!, maplist(pre_tree,I,O).
pre_tree(I, O) :- maplist(pre_tree,I,O).

pre_tree_2(I,O):- \+ is_list(I),!,I=O.
pre_tree_2([], []) :- !.
pre_tree_2([tag(_,'CORENLP'),Y],O):- !, pre_tree_2(Y,O).
pre_tree_2([tag(_,FRAG)|Y],O):- too_long(FRAG),!, maplist(pre_tree_2,Y,O).
pre_tree_2([tag(_,FRAG),Y],O):- too_long(FRAG),!, pre_tree_2(Y,O).
pre_tree_2([tag('S',_)|Y],O):- !, maplist(pre_tree_2,Y,O).
pre_tree_2([tag('S',_),Y],O):- !, pre_tree_2(Y,O).
pre_tree_2([tag('I',_),Prep],prep(Prep)):- !.
%pre_tree_2([tag(_,NP),[tag(_,NNP),Word]|More], [[tag('N',NNP),Word]|More]) :- NP=='NP',!.
pre_tree_2([tag(N,NP),[tag(W,NNP),Word]|I],[tag(N,NNP),[tag(W,NNP),Word]|O]) :- NP\==NNP, nop(NP=='NP'),!,maplist(pre_tree_2,I,O).
pre_tree_2(I, O) :- maplist(pre_tree_2,I,O).

%s_c_code(S,tag( C, S)):- atom_chars(S,[C]),!.
%s_c_code(S,tag('S',S)):- atom_chars(S,['S'|_]),!.
s_c_code(S,tag( C, S)):- atom_chars(S,[C|_]).
compile_nl_tree(I,O):- \+ is_list(I),!,I=O.
compile_nl_tree([[tag('N',_NP1)|S],[tag('V',VB)|V],[tag('N',_NP2)|O]|Rest],OUT):- !, vso_out([tag('V',VB)|V],S,O,Rest,OUT).
compile_nl_tree([[tag('V',VB)|V],[tag('N',_NP1)|S],[tag('N',_NP2)|O]|Rest],OUT):- !, vso_out([tag('V',VB)|V],S,O,Rest,OUT).
compile_nl_tree(I, O) :- maplist(compile_nl_tree,I,O).

compile_nl_tree(O,O).

%a few years ago i took an aiml dataset and converted it into dialog moves and deep logical form.. it turned out that 70% was just at best aiml tricks

/*
*/

vso_out(V,S,O,Rest,OUT):- vso_out(V,S,O,M),rest_out(Rest,M,OUT).

rest_out([],M,M):-!.
rest_out(['.',?],M,query(M)):-!.
rest_out([E|R],I,O):- is_list(E),!, rest_out(E,I,M),rest_out(R,M,O).
rest_out(U,M,and(M,U)):-!.

vso_out(V,S,O,OUT):-  OUT = decl(Group1 &Group2 & Group3 & pred(Act,Subj,Obj)),
  arg_group(V,Act,Group1), arg_group(S,Subj,Group2), arg_group(O,Obj,Group3).

arg_group(List,V,Out):- flatten(List,Flat),include(atomic,Flat,NameL),debug_var(NameL,V),
   make_out(List,NameL,V,Out).

make_out(List,_NameL,V,E):- append(Left,[[tag('C','CC'),'and']|Rest],List),
  arg_group(Left,V1,Out1),
  arg_group(Rest,V2,Out2),
  Out3=both(V1,V2,V),
  E = Out1&Out2&Out3,!.
  
make_out(_List,NameL,V,E):-
   C80 = [what,is|NameL],
   process4a(off,C80,_U,S1,_Times),
   answer803(S1,V,E),!.
make_out(List,_NameL,V,denotedBy(V,List)).

c81(S):- locally(set_prolog_flag(gc,false),time(process(debug,S))),fail.
c81(S):- 
 mpred_test_mok(into_lexical_segs(S,U)),
 forall(deepen_pos(sentence80(E,U,[],[],[])),dmsg(E)),
 fail.
c81(S):- 
  mpred_test_mok(text_to_best_tree(S,U)),
  once((print_tree_nl(p=U),
  pre_tree(U,EU),
  print_tree_nl(q=EU),
  pre_tree_2(EU,EUQ),
  print_tree_nl(r=EUQ),
  compile_nl_tree(EUQ,E),
  dmsg(sentence=S),
  print_tree_nl(s=E))),
 fail.
c81(_).


gp_africa(Result):-
  setOf(Size:City, []^(       
       database80(ti(city,City)),
       database80(trans_pred(spatial,contain,africa,City)) ,
       database80(count_pred(spatial,population,City,Size)),
       database80(exceeds(Size,_Other))), List),
   database80(aggregate80(max,List,Result)).

:- add_history1(ensure_loaded(geography/load_kb)).

:- forall(chat_80_ed(_,B,_),add_c80(B)).
:- add_c80("how many rivers are in poland ?").
:- add_c80("iran is bordered by iraq?").
:- add_c80("what city in africa has the greatest population?").
:- add_c80("do oceans border any country?").
:- add_c80("is the population of china greater than india's population?").
:- add_c80("what is the total area of nations that can border iraq?").
:- add_c80("what is the total area of nations that should border iraq?").
:- add_c80("what is the total area of nations that are bordered by iraq?").
:- add_c80("what ocean does not border any country ?").
:- add_history1(test_chat80).
:- add_c80("what oceans should border any country?").
:- add_c80("is china a country?").
:- add_c80("are china and japan a country?").
:- add_c80("is china and japan a country?").
:- add_c80("are china and japan countries?").

%~ decl( s( np(3+sg,nameOf(china),[]),
%~          verb(make,active,past+fin,[],posP(PosP)),
%~          [ arg(dir,np(3+sg,np_head(generic,[],peace),[prep_phrase(prep(with),np(3+sg,nameOf(japan),[]))]))],
%~          []))
%~ decl( s( np(3+sg,nameOf(china),[]),
%~          verb(make,active,past+fin,[],posP(PosP)),
%~          [ arg(dir,np(3+sg,np_head(generic,[],peace),[prep_phrase(prep(with),np(3+sg,nameOf(japan),[]))]))],
%~          []))
%~ decl( s( np(3+sg,nameOf(china),[]),
%~          verb(talk,active,Fin+fin,[],posP(PosP)),
%~          [ arg(dir,np(3+sg,nameOf(japan),[]))],
%~          []))


:- fixup_exports.

:- if(\+ prolog_load_context(reloading, true)).
:- initialization(prolog).
:- endif.
