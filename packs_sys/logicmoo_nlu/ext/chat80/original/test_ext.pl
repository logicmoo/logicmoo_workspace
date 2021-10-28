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

c81(S):- once(c82(S)),fail. 
c81(S):- 
 time_once((mpred_test_mok(into_lexical_segs(S,U)),
 forall(deepen_pos(sentence80(E,U,[],[],[])),dmsg(E)))),
 fail.
c81(S):- time_once(c83(S)),fail. 
c81(_).


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
is_penn_tag(_,S):- atom(S),upcase_atom(S,S), \+ downcase_atom(S,S), S\=='I'.
is_penn_long(S):-is_penn_tag(_,S),too_long(S).

pree_tree_done(I,O):- ( I==[] ;  \+ is_list(I) ),!,I=O.
pree_tree_done([I],[O]):- ( I==[] ;  \+ is_list(I) ),!,I=O.

pree_tree_finish(G2,I,O) :- once(maplist(G2,I,O)),I\==O,!.
pree_tree_finish(G2,[S|I],[S|O]) :- call(G2,I,O),!.

pre_tree_0(I,O):- pree_tree_done(I,O),!.
pre_tree_0(['VP',['NN',Border]|I],O):- !, pre_tree_0(['VP',['VB',Border]|I],O).
pre_tree_0([FRAG,I],O):- is_penn_long(FRAG),!, pre_tree_0(I,O).
pre_tree_0([A,[FRAG|I]|B],O):- is_penn_long(FRAG),append([A|I],B,Y),!,pre_tree_0(Y,O).
pre_tree_0([FRAG|I],O):- is_penn_long(FRAG),!, pre_tree_0(I,O).
pre_tree_0(I,O):-  pree_tree_finish(pre_tree_0,I,O).

pre_tree_1(I,O):- pree_tree_done(I,O),!.
pre_tree_1(I,O):- pree_tree_finish(pre_tree_1,I,O).

pre_tree_2(I,O):- pree_tree_done(I,O),!.
pre_tree_2([S|I], [TAG|O]) :-  is_penn_tag(_,S),s_c_code(S,TAG),!, maplist(pre_tree_2,I,O).
pre_tree_2([I,List],O):- maplist(is_list,[I,List|List]),!, pre_tree_2([I|List],O).
%pre_tree_2([I,List|More],O):- maplist(is_list,[I,List|List]),!, pre_tree_2([I|List],O).
pre_tree_2(I,O):- pree_tree_finish(pre_tree_2,I,O).

pre_tree_3(I,O):- pree_tree_done(I,O),!.
%pre_tree_3([I],O):- is_list(I),!, pre_tree_3(I,O).
%pre_tree_3([tag(_,'I',_),Prep],prep(Prep)):- !.
%pre_tree_3([tag(_,_,NP),[tag(_,_,NNP),Word]|More], [[tag(_,'N',NNP),Word]|More]) :- NP=='NP',!.
pre_tree_3([tag(_,N,NP),[tag(_,W,NNP),Word]|I],O) :- NP\==NNP,!,pre_tree_3([tag(_,N,NNP),[tag(_,W,NNP),Word]|I],O).
pre_tree_3(I,O):- pree_tree_finish(pre_tree_3,I,O).


pre_tree_4(I,O):- pree_tree_done(I,O),!.
pre_tree_4(I,O):- pree_tree_finish(pre_tree_4,I,O).
pree_tree_call(G,X,Y):- pt_call(G,X,Y),ignore((X\==Y,in_cmt(print_tree_nl(G=Y)))),!.

pt_call(_,X,Y):- pree_tree_done(X,Y),!.
pt_call(G,X,Y):- call(G,X,Y).
pt_call(G,X,Y):- pree_tree_finish(pt_call(G),X,Y),!.

%s_c_code(S,tag(_, C, S)):- atom_chars(S,[C]),!.
%s_c_code(S,tag(_,'S',S)):- atom_chars(S,['S'|_]),!.
s_c_code(S,tag(_, C, S)):- atom_chars(S,[C|_]).

words_of(I,Words):- flatten(I,Flat),include(atomic,Flat,Words).

compile_nl_tree(I,O):- pree_tree_done(I,O),!.
compile_nl_tree([[tag(_,'N',_NP1)|S],[tag(_,'V',VB)|V],[tag(_,'N',_NP2)|O]|Rest],OUT):- !, vso_out([tag(_,'V',VB)|V],S,O,Rest,OUT).
compile_nl_tree([[tag(_,'V',VB)|V],[tag(_,'N',_NP1)|S],[tag(_,'N',_NP2)|O]|Rest],OUT):- !, vso_out([tag(_,'V',VB)|V],S,O,Rest,OUT).
compile_nl_tree(I, O) :- once(maplist(compile_nl_tree,I,O)),I\==O,!.
compile_nl_tree([tag(_,N,NP)|I], O):- N=='N', words_of(I,Words),debug_var(Words,V),make_out1([tag(_,N,NP)|I],Words,V,O),!.
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

arg_group(List,V,Out):- flatten(List,Flat),include(atomic,Flat,Words),debug_var(Words,V),
   make_out(List,Words,V,Out).

c80_what_is(Words,V,E):- \+ last(Words,'?'),append(Words,['?'],WordsQ),!,c80_what_is(WordsQ,V,E).
c80_what_is([what|Words],V,E):- !, c80_what_is(Words,V,E).
c80_what_is(Words,V,E):-  C80 = [what,are|Words], wots(_,process4a(off,C80,_U,S1,_Times)), answer803(S1,V,E).
c80_what_is(Words,V,E):-  C80 = Words, wots(_,process4a(off,C80,_U,S1,_Times)), answer803(S1,V,E).


simplify_tree(I,O):- \+ compound(I),!,I=O.
simplify_tree(I&true,O):- simplify_tree(I,O).
simplify_tree(true&I,O):- simplify_tree(I,O).
simplify_tree(seto(V,true,N),valueOf(V)):- N=V,!.
simplify_tree(seto(V,True,N),True):- N=V,!.
simplify_tree(V=N,true):- N==V,!.

simplify_tree(I,O):- compound_name_arguments(I,N,A),maplist(simplify_tree,A,AO),compound_name_arguments(O,N,AO),!.
simplify_tree(E,E).

make_out1(_List,Words,V,E):- c80_what_is(Words,V,E),!.
make_out1([List],Words,V,E):- is_list(List),!,make_out(List,Words,V,E).
make_out1(List,_Words,V,E):- append(Left,[[tag(_,'C','CC'),'and']|Rest],List),
  arg_group(Left,V1,Out1), arg_group(Rest,V2,Out2), Out3=both(V1,V2,V),
  simplify_tree(Out1&Out2&Out3,E),!.

make_out1([tag(V,'N',NNP),[tag(V,'N',NNP),Noun]],_Words,V,V=nounFn(Noun)).
make_out1([tag(V,'W','WP'),Noun],_Words,V,stringValue(V,Noun)).
make_out1([tag(V,'V',_),Verb],_Words,V,V=verbFn(Verb)):- ignore(V=verbFn(Verb)).

make_out(List,Words,V,E):- make_out1(List,Words,V,E),!.
make_out(List,Words,V,denotedBy(V,List,c80(Phrase))):- any_to_string([what,is|Words],Phrase).

%text_to_best_tree80(I,O):-text_to_charniak_tree(I,O).
text_to_best_tree80(I,O):-text_to_best_tree(I,O).

time_once(G):- locally(set_prolog_flag(gc,false),time(once(G))).

c82(S):- time_once(process(debug,S)).
c83(S):-
  mpred_test_mok(text_to_best_tree80(S,U)),
  print_tree_nl(text_to_best_tree80=U),
  transitive([
       pree_tree_call(pre_tree_0),
       pree_tree_call(pre_tree_1),
       pree_tree_call(pre_tree_2),
       pree_tree_call(pre_tree_3),
       pree_tree_call(pre_tree_4)],U,EUQ),  
  transitive(compile_nl_tree,EUQ,E),
  transitive(simplify_tree,E,Simple),
  dmsg(?-c80(S)),
  add_c80(S),
  in_color(blue,  print_tree_nl(e=E)),
  in_color(yellow,print_tree_nl(s=Simple)),!.
 



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
:- add_c80("china is a country?").

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
