:- use_module(library(statistics)).

%:- autoload_all.
:- use_module(library(logicmoo_common)).
:- use_module(library(logicmoo_nlu)).
:- if(\+ prolog_load_context(reloading, true)).
:- use_module(library(logicmoo_clif)).
:- endif.
%:- xlisting(lock_predicate/1).
%:- autoload_all.

:- ensure_loaded(objecteese).


:- '$set_source_module'(parser_chat80).


% text_drs_eval(Evaluation, Id, Text, DRS, LHSs, Timestamp, Author, Comment).
:- if(exists_source(ape(tests/acetexts))).
:- ensure_loaded(ape(tests/acetexts)).
:- endif.

training_data(Text,DRS):-  parser_ape:text_drs_eval(_Evaluation, _Id, Text, DRS, _LHSs, _Timestamp, _Author, _Comment), should_learn(DRS).
:- dynamic(c80:lf_trained/3).
add_c80(B):- any_to_str(B,S),add_history1(c80(S)).
add_c81(B):- any_to_str(B,S),add_c80(S),!, ignore(learn_full(e2c,S)).
add_c82(B):- any_to_str(B,S),add_c80(S),!, print_tree_nl(?-c81(S)).

show_c80(B):- any_to_str(B,S),format(' %~~ ~q. ~n',[c80(S)]).


is_pucnt80('.').
is_pucnt80('?').
is_pucnt80('!').

repairs_of_tree(LHS,[],LHS):- !.
repairs_of_tree(LHS,[P,E|LHSWords],DCLHS):- is_pucnt80(P), !, repairs_of_tree(LHS,[dc(E)|LHSWords],DCLHS).
repairs_of_tree(LHS,[dc(E)|LHSWords],DCLHS):- !,
  downcase_atom(E,DC),subst_ci(LHS,E,DC,MLHS),
  repairs_of_tree(MLHS,LHSWords,DCLHS).
repairs_of_tree(LHS,[E|LHSWords],DCLHS):- is_pucnt80(P),atom_concat(L,P,E),L\=='',!,repairs_of_tree(LHS,[L,'.'|LHSWords],DCLHS).
repairs_of_tree(LHS,[E|LHSWords],DCLHS):-
  downcase_atom(E,DC),subst_ci(LHS,E,DC,MLHS),
  repairs_of_tree(MLHS,LHSWords,DCLHS).



/*
learn_c79(B,RHS):-
  parts_of(Type,B,Words),
  any_to_input_layer(Words,S),
 ignore((
  \+ skip_text_to_tree(S),
  print_tree_nl(?-c81(S)),
  text_to_tree(S,LHS),
  learn_tree_tags(LHS),
  learn_can_become(Type,LHS,RHS))).
*/

%learn_full(Type,S):- known_lf(S,RHS),!,learn_full(Type,S,RHS).
learn_full(S):- learn_full(e2c,S).
learn_full(Type,S):- parts_of(Type,S,B), observe_system_full(B,RHS),should_learn(RHS),!,learn_full(Type,S,RHS).


learn_full(Type,B,RHS):-
  parts_of(Type,B,Words),
  any_to_input_layer(Words,S),
 ignore((
  \+ skip_text_to_tree(S),
  print_tree_nl(?-c81(S)),
  text_to_tree(S,LHS),
  learn_tree_tags(LHS),
  learn_can_become(Type,LHS,RHS))).

parts_of(e2c,RHS,RHSWords):- words_of(RHS,RHSWords).

learn_equivalencies(Type,List):-
 forall((
  select(I,List,Rest),
  member(Rest,O)),
 learn_can_become(Type,I,O)).

skip_learning(X):- var(X),!.
skip_learning([]).
skip_learning(drs([],[])).
should_learn(X):- \+ skip_learning(X).

learn_can_become(Type,LHS,RHS):-
  ignore(( should_learn(LHS), should_learn(RHS),
  %  parts_of(Type,LHS,LHSWords), any_to_input_layer(LHSWords,S), print_tree_nl(?-c80(S)),
  parts_of(Type,RHS,RHSWords), !,
  subst_words(RHSWords,LHS,[],RHS,NewLHS,AprioriHacks,NewRHS),!,
  learn_lf(NewLHS,AprioriHacks,NewRHS))).

learn_lf(LHS,AprioriHacks,RHS):- push_model([set(tree,LHS),set(pre,AprioriHacks),set(drs,RHS),tree->drs]).

normalize_tree(LHS,NLHS):- 
  words_of(LHS,[H|Words]),
  repairs_of_tree(LHS,[dc(H)|Words],DCLHS),!,
  pre_tree_0(DCLHS,NLHS).
normalize_tree(LHS,NLHS):- 
  words_of(LHS,[H|Words]),
  repairs_of_tree(LHS,[dc(H)|Words],DCLHS),
  flatten_tree(DCLHS,NLHS).
  %pre_tree_0(DCLHS,NLHS).

change_lrhs(man,person).
change_lrhs(men,persons).
change_lrhs(woman,person).
change_lrhs(women,persons).

push_model(X):- change_lrhs(B,A),subst(X,B,A,Y),X\==Y,!,push_model(Y).
push_model(X):- push_model(X,[],_).

push_model([])-->[].
push_model([E|More])--> !, push_model(E),push_model(More).
push_model(X->Y) -->  get_from(X,LHS),get_from(Y,RHS), add_lr(LHS,RHS),set_from(ctx,LHS).
push_model(->(Y)) -->  get_from(ctx,LHS),get_from(Y,RHS),add_lr(LHS,RHS).
push_model((X->Y->Z)) -->  push_model([X->Y,Y->Z]).
push_model(set(X,AprioriHacks)) --> set_from(X,AprioriHacks).
push_model(get(X,AprioriHacks)) --> get_from(X,AprioriHacks).
push_model(add(X,AprioriHacks)) --> add_from(X,AprioriHacks).

set_from(N,V,In,[N=V|In]).
get_from(N,V,In,In):- (member(N=V,In);V=[]),!.
add_from(N,V,In,[N=A|In]):- member(N=P,In)->append(P,V,A);V=A.
add_lr(LHS,RHS,In,In):- member(pre=B,In), add_lmr(LHS,B,RHS).

add_lmr(LHS,Pre,RHS):-
  P = c80:lf_trained(LHS,Pre,RHS),
  unnumbervars(P,Q),
  asserta_if_new(Q),
  nop((print_tree_nl(lf_trained=Q), my_drs_to_fol(RHS, FOL), print_tree_nl(fol=FOL))),!.


  
klf(B):- any_to_input_layer(B,S),known_lf(S,RHS),print_tree_nl(RHS).

learnedFrom(X,Name):- debug_var(Name,X).

words_to_base_forms(W,BF):- words_of(W,Words),maplist(into_base_form,Words,BF),!.

subst_words([],LHS,PreQ,RHS,LHS,PreQ,RHS).
subst_words([S|AllWords],LHS,PreQ,RHS,NewLHS,PreQO,NewRHS):- 
  subst_word(S,LHS,PreQ,RHS,MLHS,PreQM,MRHS),!,
  subst_words(AllWords,MLHS,PreQM,MRHS,NewLHS,PreQO,NewRHS).


preserve_lhs(S):- upcase_atom(S,U),downcase_atom(S,U).
preserve_lhs(S):- arg(_,v(which,who,is,if,then,exists,there,of),S).  
preserve_rhs(S):- upcase_atom(S,U),downcase_atom(S,U).
preserve_rhs(S):- arg(_,v(countable,mass,somebody,na,geq,be,of,leq,neq,eq,1),S).
subst_word(S,LHS,PreQ,RHS,LHS,PreQ,RHS):- preserve_rhs(S),!.
subst_word(S,LHS,PreQ,RHS,NewLHS,[learnedFrom(V,S)|PreQ],NewRHS):- \+ preserve_lhs(S), subst_ci(LHS,S,V,NewLHS),LHS\==NewLHS,!,upcase_atom(S,SU),V='$VAR'(SU),debug_var(S,V),subst_ci(RHS,S,V,NewRHS).
subst_word(S,LHS,PreQ,RHS,NewLHS,[rootform(Transitive,V,V2),learnedFrom(V,S)|PreQ],NewRHS):-
   rootform(Transitive,S,S2),
   upcase_atom(S2,SU2),V2='$VAR'(SU2),
   subst_ci(LHS,S2,V2,NewLHS),
   \+ preserve_lhs(S2), LHS\==NewLHS,!,
   upcase_atom(S,SU),V='$VAR'(SU),
   subst_ci(RHS,S,V,NewRHS),
   debug_var(S,V),
   debug_var(S2,V2),!.
subst_word(_,LHS,PreQ,RHS,LHS,PreQ,RHS).

verb_rootform(Transitive,S,S2):- lex_subst(talk_db(intransitive,S,_,_,_,_),3,S2),!,
  (lex_subst(talk_db(transitive,S,_,_,_,_),3,S2)-> Transitive=verb ; intransitive=Transitive).
verb_rootform(transitive,S,S2):- lex_subst(talk_db(transitive,S,_,_,_,_),3,S2),!.

into_base_form(S2,S):- rootform(_,S,S2),!.
into_base_form(S,S).

rootform(Transitive,S,S2):- verb_rootform(Transitive,S,S2).
rootform(Noun1,S,S2):- talk_db(Noun1,S,S2).
rootform(Human,S,S2):- clex:noun_pl(S2,S,Human).
lex_subst(G,N,Arg):-functor(G,_,A),call(G),between(N,A,At),arg(At,G,Arg),atom(Arg).

subst_ci(A,B,C,O):- notrace((subst(A,B,C,D),toPropercase(B,B1),toLowercase(B,B2),subst(D,B1,C,DO),subst(DO,B2,C,O))).
  

skip_learn_contains("_").
skip_learn_contains("'").
skip_learn_contains('"').
skip_learn_contains("york").
skip_learn_contains("volta").
skip_learn_contains("joe").
skip_learn_contains("/*").
skip_learn_contains("**/").
skip_learn_contains("#").


skip_text_to_tree(B):- any_to_str(B,S), skip_learn_contains(F),atom_contains(S,F),!.
maybe_learn_replacements(B):- 
  any_to_str(B,S), \+ (skip_learn_contains(F),atom_contains(S,F)),
    text_to_tree(S,LHS), learn_tree_tags(LHS),print_tree_nl(learn_tree_tags(LHS)).



:- dynamic(wt_replacement/4).
:- dynamic(tmp:used_wt_replacement/5).

learn_wt_replacement(chat80,_,Word,_):-  word81(Word),!.
learn_wt_replacement(chat80,X,Word,Tag):- 
  ignore(retract(wt_replacement(chat80,X,_,_))),ignore(retract(wt_replacement(chat80,_,Word,_))),asserta(wt_replacement(chat80,X,Word,Tag)).


learn_tree_tags(LHS):- forall(sub_term(E,LHS),ignore(learn_tree_tag(E))).
learn_tree_tag(E):- is_list(E),E=[Tag,Word],is_word80(Word),learn_penn_tag(Tag,Word),!.
learn_penn_tag(Tag,Word):- flag('$vr',X,X+1),learn_wt_replacement(chat80,X,Word,Tag).

:- flag('$vr',_,0).

%

is_loading_file:- prolog_load_context(reloading, true),!.
is_loading_file:- prolog_load_context(file,_), prolog_load_context(term,T), T\==[].


c80(B):- is_loading_file, !, add_c80(B).
c80(Text):- 
  %cls, 
  make, 
  tracing ~= on,
  %s80,
  show_c80(Text),
  any_to_input_layer(Text,S),!,
  add_c80(S), 
  c81(S).

c81(S):- fail, once(c82(S)),fail. 
c81(S):-
 time_once((mpred_test_mok(into_lexical_segs(S,U)),
 forall(deepen_pos(sentence80(E,U,[],[],[])),print_tree_nl(E)))),
 fail.
c81(S):- fail,time_once(c83(S)),fail.
c81(S):- fail, time_once(input_to_middle(S)),fail.
c81(S):- time_once(c84(S)),fail.
c81(_).

 

flatten_tree([], []) :- !.
%flatten_tree([S|Ls], FlatL) :- is_penn_long(S), flatten_tree(Ls, FlatL).
flatten_tree(['VP'|Ls], FlatL) :- !, flatten_tree(Ls, FlatL).
flatten_tree(['PP'|Ls], FlatL) :- !, flatten_tree(Ls, FlatL).
%flatten_tree([S|Ls], [mark(S)|FlatL]) :- (is_penn_long(S), contains_phrase(Ls)  ),!, flatten_tree(Ls, FlatL).
flatten_tree([S|Ls], FlatL) :- (is_penn_long(S), contains_phrase(Ls)  ),!, flatten_tree(Ls, FlatL).
flatten_tree([S|Ls], FlatL) :- S=='NP', sub_var('NP', Ls), flatten_tree(Ls, FlatL).
flatten_tree([L|Ls], [L|NewLs]) :- 
    dont_flatten(L),!,
    flatten_tree(Ls, NewLs),!.
flatten_tree([L|Ls], FlatL) :-
    flatten_tree(L, NewL),
    flatten_tree(Ls, NewLs),
    append(NewL, NewLs, FlatL).
flatten_tree(L, [L]).

not_is_list(X):- \+ is_list(X).

dont_flatten([_|L]):- sub_var('NP',L),!, fail.
dont_flatten([S|_]):- is_penn_long(S),!, fail.
dont_flatten([S|_]):- is_penn_tag(S).

contains_phrase(Ls):- sub_term(E,Ls),atom(E),(is_penn_long(E);E=='NP').
contains_phrase(Ls):- member(E,Ls),is_list(E),member(Sl,E),is_list(Sl).

pree_tree_done(I,O):- ( I==[] ;  \+ is_list(I) ),!,I=O.
pree_tree_done([I],[O]):- ( I==[] ;  \+ is_list(I) ),!,I=O.

pree_tree_finish(G2,I,O) :- once(maplist(G2,I,O)),I\==O,!.
pree_tree_finish(G2,[S|I],[S|O]) :- call(G2,I,O),!.

pre_tree_0(I,O):- pree_tree_done(I,O),!.
pre_tree_0(['VP',['NN',Border]|I],O):- !, pre_tree_0(['VP',['VB',Border]|I],O).
pre_tree_0([A,[FRAG|I]|B],O):- is_penn_long(FRAG),append([A|I],B,Y),!, maplist(pre_tree_0,Y,O).
pre_tree_0([FRAG,I],O):- is_penn_long(FRAG),!, pre_tree_0(I,O).
pre_tree_0([FRAG|I],O):- is_penn_long(FRAG),!, maplist(pre_tree_0,I,O).
pre_tree_0(I,O):-  pree_tree_finish(pre_tree_0,I,O).

pre_tree_1(I,O):- pree_tree_done(I,O),!.
pre_tree_1(['VP',['NN',Border]|I],O):- !, pre_tree_1(['VP',['VB',Border]|I],O).
pre_tree_1([FRAG,I],O):- is_penn_long(FRAG),!, pre_tree_1(I,O).
pre_tree_1([FRAG|I],O):- is_penn_long(FRAG),!, maplist(pre_tree_1,I,O).
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

is_word80(X):- (string(X);number(X)),!.
is_word80(X):- \+ atom(X),!,fail.
is_word80(X):- atom_length(X,1),!.
is_word80(X):- X\==[], X\==adv, \+ is_penn_tag(X).

words_of(I,Words):- var(I),!,throw(var_words_of(I,Words)).
words_of(I,Words):- I==[],!,Words=[].
words_of(I,Words):- words_of0(I,Words0),tokenizer:expand_contracted_forms(Words0,Words),!.

finish_tokenize(I,O):- maplist(any_to_atom,I,O).
append_all_but_last(_End,[],[]):- !.
append_all_but_last(_End,[I],[I]):-!.
append_all_but_last(End,[H|T],[HH|TT]):- !, append(H,[End],HH), append_all_but_last(End,T,TT).


my_tokenize_atom('',[]):- !.
my_tokenize_atom(I,O):- member(Sep,['\n','.','?','!']),atom_contains(I,Sep),atomic_list_concat(List0,Sep,I),
  maplist(my_tokenize_atom,List0,List1),append_all_but_last(Sep,List1,List2),append(List2,O),!.
my_tokenize_atom(I,O):- atom_contains(I,' ?'),!,split_string(I, " \s\t\n_", " \s\t\n", Flat),finish_tokenize(Flat,O).
%my_tokenize_atom(I,O):- !,split_string(I, " \s\t\n_", " \s\t\n", Flat),finish_tokenize(Flat,O).
my_tokenize_atom(I,O):- tokenize_atom(I,Flat),finish_tokenize(Flat,O).
words_of0(I,Words):- atomic(I),!,my_tokenize_atom(I,Flat),include(is_word80,Flat,Words).
words_of0(I,Words):- \+ is_list(I),!,findall(E,(sub_term(E,I),is_word80(E)),Words).
words_of0([FW|I],Words):- FW=tag(_,_,_),!,flatten([FW|I],Flat),include(is_word80,Flat,Words).
words_of0([FW|I],Words):- maplist(is_word80,[FW|I]),!,[FW|I]=Words.
words_of0([FW|I],Words):- tree_to_words([FW|I],Words).

tree_to_words([],[]):-!.
tree_to_words(I,[O]):- is_w2(I),!,arg(1,I,O).
tree_to_words([I],[O]):- is_w2(I),!,arg(1,I,O).
tree_to_words(I,[I]):-  \+ is_list(I),!.
%tree_to_words([cp|I],O):- !, maplist(tree_to_words,I,WordsL),flatten([WordsL],O).
%tree_to_words([vbar|I],O):- !, maplist(tree_to_words,I,WordsL),flatten([WordsL],O).
tree_to_words([_|I],O):- I\==[],!, maplist(tree_to_words,I,WordsL),flatten([WordsL],O),!.
%tree_to_words([aux,Can,Not],O):- is_word80(Can),is_word80(Not),O=[Can,Not],!.
%tree_to_words([[]|I],O):- tree_to_words(I,O).
%tree_to_words([_,I,[]],O):- tree_to_words(I,O).
%tree_to_words([A,[]],[]):- atom(A),!.
tree_to_words([I],[I]):- \+ is_list(I),!.
tree_to_words([I|E],O):- E\==[], \+ is_list(I),!,tree_to_words(E,O).
tree_to_words([FW|I],Words):- maplist(tree_to_words,[FW|I],WordsL),flatten([WordsL],Words).
tree_to_words(_,[]).

compile_nl_tree(I,O):- pree_tree_done(I,O),!.
compile_nl_tree([[tag(_,'N',_NP1)|S],[tag(_,'V',VB)|V],[tag(_,'N',_NP2)|O]|Rest],OUT):- !, vso_out([tag(_,'V',VB)|V],S,O,Rest,OUT).
compile_nl_tree([[tag(_,'V',VB)|V],[tag(_,'N',_NP1)|S],[tag(_,'N',_NP2)|O]|Rest],OUT):- !, vso_out([tag(_,'V',VB)|V],S,O,Rest,OUT).
compile_nl_tree(I, O) :- once(maplist(compile_nl_tree,I,O)),I\==O,!.
compile_nl_tree([tag(_,N,NP)|I], O):- N=='N', parts_of(e2c,I,Words),debug_var(Words,V),make_out1([tag(_,N,NP)|I],Words,V,O),!.
compile_nl_tree(O,O).

replace_with_unlearned_words(Program,UU,U):- 
 retract(tmp:used_wt_replacement(Program,_X,E,_Tag,Word)),
 notrace(subst_ci(UU,E,Word,MU)),replace_with_unlearned_words(Program,MU,U).
replace_with_unlearned_words(_Program,U,U).


ok_replace_for(chat80,X):- !, word80(X).
ok_replace_for(_,_).

may_replace(_Chat80,Word,_Tag,OWord):- \+ word81(OWord), \+ word81(Word).

get_tag_and_word(E,Tag,Word):- is_list(E),E=[Tag,Word],is_word80(Word).

replace_with_learned_words(Program,UU,U):- 
 sub_term(E,UU),get_tag_and_word(E,Tag,Word), 
  ok_replace_for(Program,Word),
 \+ wt_replacement(Program,_,Word,Tag),
  do_wt_replacement(Program,UU,U,Word,Tag),!.
replace_with_learned_words(_Program,U,U).


do_wt_replacement(Program,UU,U,Word,Tag):-     
   warn_failure(wt_replacement(Program,X,OWord,Tag)),
   warn_failure(may_replace(Program,Word,Tag,OWord)),
   notrace(subst_ci(UU,[Tag,Word],[Tag,OWord],MU)),
   ignore(retract(tmp:used_wt_replacement(Program,X,_,_,_))),
   assert(tmp:used_wt_replacement(Program,X,OWord,Tag,Word)),
 replace_with_learned_words(Program,MU,U).
do_wt_replacement(Program,U,U,Word,Tag):- print_tree_nl(warn(throw(cant_wt_replacement(Program,Word,Tag)))).

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
make_out1(List,_Words,V,E):- append(Left,[[tag(V,'C','CC'),'and']|Rest],List),
  arg_group(Left,V1,Out1), arg_group(Rest,V2,Out2), Out3=both(V1,V2,V),
  simplify_tree(Out1&Out2&Out3,E),!.

make_out1([tag(V,'N',NNP),[tag(V,'N',NNP),Noun]],_Words,V,V=nounFn(Noun)).
make_out1([tag(V,'W','WP'),Noun],_Words,V,stringValue(V,Noun)).
make_out1([tag(V,'V',_),Verb],_Words,V,V=verbFn(Verb)):- ignore(V=verbFn(Verb)).

make_out(List,Words,V,E):- make_out1(List,Words,V,E),!.
make_out(List,Words,V,denotedBy(V,List,c80(Phrase))):- any_to_input_layer([what,are,Words,'?'],Phrase).

text_to_tree(I,O):- any_to_input_layer(I,S),input_to_middle(S,MM),!,pre_tree_0(MM,M),normalize_tree(M,O),!.

:- ensure_loaded(library(logicmoo_nlu/parser_link_grammar)).

input_to_middle(I,O):- input_to_middle(I,O,_).

input_to_middle(I,O,lgp):- text_to_lgp_tree(I,O).
input_to_middle(I,O,corenlp):- text_to_corenlp_tree(I,O).
input_to_middle(I,O,best):- text_to_best_tree(I,O).
input_to_middle(I,O,charniak):- text_to_charniak_tree(I,O).
input_to_middle(I,O,ace):- text_to_ace_tree(I,O).
input_to_middle(I,O,ape):- text_to_ape_tree(I,O).
input_to_middle(I,O,chat80):- text_to_chat80_tree(I,O).

learn_middle(B):-learn_middle(e2c,B).
learn_middle(Type,B):- any_to_input_layer(B,I),
  parts_of(Type,I,II),
  findall(O,(input_to_middle(I,O,Named),print_tree_nl(Named=O)),ResL), 
  learn_equivalencies(Type,[II|ResL]).


text_to_all_trees(I):-
  forall(clause(input_to_middle(I,O,Named),B),ignore((call(B),print_tree(Named=O)))).


time_once(G):- notrace(garbage_collect),quietly(locally(set_prolog_flag(gc,true),time(once(show_failure(always,G))))).

any_to_input_layer(I,S):- any_to_str(I,S).
any_to_str(I,S):- \+ string(I), words_of(I,U), any_to_string(U,S),!.
any_to_str(S,S).
  

c82(S):- fail, time_once(process(debug,S)).


c84:-
  Text= "how many rivers are in poland ?",
  c84(Text).


c84(Text):- c84(Text,Post), my_drs_to_fol(Post,FOL),exec_fol(FOL).

c84(Text,Post):-
  text_to_tree(Text,Pre),
  % forall(c80:lf_trained(Pre,_Mid,_Post2),print_tree_nl(pre_db=Pre)),  
 % \+ \+ c80:lf_trained(Pre,_,_),!,
  c80:lf_trained(Pre,Mid,Post),
  %print_tree_nl(pre=Pre),
  maplist(call,Mid),
  ignore((print_tree_nl(using=lf_trained(Pre,Mid,Post)))).
c84(I,O):- observe_system_full(I,O),!.
c84(I,O):- fail,
  any_to_input_layer(I,S),
  add_c82(S),
  mpred_test_mok(text_to_tree(S,UU)),
  print_tree_nl(text_to_tree=UU),
  replace_with_learned_words(chat80,UU,U),
  print_tree_nl(replace_with_learned_words=U),
  parts_of(e2c,U,WordsR),
  print_tree_nl(words_of=WordsR),
  parts_of(e2c,I,WordsI),
  WordsI\==WordsR,
  add_c82(WordsR),!,
  c84(WordsR,RHS),
  replace_with_unlearned_words(chat80,RHS,O),
  should_learn(O).
%c84(I,O):- run_pipeline(I,[clausify80=O]).

observe_system_full(Text):- observe_system_full(Text,Post), my_drs_to_fol(Post,FOL),exec_fol(FOL).

observe_system_full(I,O):- any_to_input_layer(I,M),!,ping_each_system(I,M,O),should_learn(O).

ping_each_system(_,M,O):- c88(M,O),should_learn(O).
ping_each_system(_,M,O):- try_ace_lf(M,O),should_learn(O),!.
ping_each_system(I,_,O):- notrace(words_to_base_forms(I,M)),any_to_input_layer(M,S),try_ace_lf(S,O).

symm_test:- s80([does,iran,border,iraq]).
symm_test2:- cls, symm_test, s80([does,iran,action1,iraq]).
symm_test3:- s80([does,iran,symmetric1,iraq]).

s80(S):- 
 ((mpred_test_mok(into_lexical_segs(S,U)),
 (deepen_pos(sentence80(E,U,[],[],[])),(print_tree_nl(sentence80=E),i_sentence(E,T),print_tree_nl(i_sentence=T))))).

c88(M):- c88(M,O),dmsg(O).
c88(M,O):- process4a(off,M,_,O,_Times).

try_ace_lf(S,O):- ace_to_drs:aceparagraph_to_drs(S,on,off,1,_Sentences,_Trees,_UnresolvedDrs,O,_Messages,_Time),should_learn(O).

exec_fol(FOL):-format('~N'), print_tree_nl(exec_fol=FOL).

my_drs_to_fol(I,O):- on_x_fail(drs_fol(I,O)),!.
my_drs_to_fol(OI,OI).

:- add_history1(c84).

text_to_lf(B,FOL):- text_to_lf1(B,FOL)*->true;text_to_lf2(B,FOL)*->true;text_to_lf3(B,FOL).

text_to_lf1(B,FOL):- 
 print_tree_nl(text_to_lf1=B),
 must_det_l((words_of(B,U),
 any_to_input_layer(U,S),
 text_to_tree(S,LHS),
 print_tree_nl(text_to_tree=LHS))),
 c80:lf_trained(LHS,AprioriHacks,RHS),maplist(ignore,AprioriHacks),my_drs_to_fol(RHS, FOL).

text_to_lf2(U,RHS):- 
  must_det_l((words_of(U,W),  
  process4a(on,W,_,RHS1,_Times),
  replace_with_unlearned_words(chat80,RHS1,RHS))).

text_to_lf3(U,RHS):- %fail,
  transitive([
       pree_tree_call(pre_tree_0),
       pree_tree_call(pre_tree_1),
       pree_tree_call(pre_tree_2),
       pree_tree_call(pre_tree_3),
       pree_tree_call(pre_tree_4)],U,EUQ),  
  transitive(compile_nl_tree,EUQ,E),
  transitive(simplify_tree,E,Simple),
  replace_with_unlearned_words(chat80,Simple,RHS),
  in_color(blue,  print_tree_nl(e=Simple)),
  in_color(yellow,print_tree_nl(s=RHS)).



gp_africa(Result):-
  setOf(Size:City, []^(       
       database80(ti(city,City)),
       database80(trans_pred(thing,contain,africa,City)) ,
       database80(count_pred(thing,population,City,Size)),
       database80(exceeds(Size,_Other))), List),
   database80(aggregate80(max,List,Result)).

%:- add_history1(ensure_loaded(geography/load_kb)).


:- add_history1([test_ext]).
%:- forall(chat80_tests(X),cvt_to_objecteese(X))
%:- forall(chat80_tests(X),cvt_to_objecteese(X))


retrain:-
  % loads 50 phrases for CAHT80
  forall(chat_80_ed(_,B,_),ignore(time_once(cvt_to_objecteese(B)))),
  % loads 4000 phrases from APE
%  forall(training_data(Sent,RHS),learn_full(Type,Sent,RHS)),

  add_c81("how many rivers are in poland ?"),

  add_c80("iran is bordered by iraq?"),
  add_c81("what city in africa has the greatest population?"),
  add_c81("do oceans border any country?"),
  add_c81("is the population of china greater than india's population?"),
  add_c80("what is the total area of nations that can border iraq?"),
  add_c81("how many countries does the danube flow through ?"),
  add_c81("the danube flows through india?"),
  add_c81("the danube flows to a seamass?"),
  add_c81("where does the danube rise?"),
  add_c81("where does the danube drain?"),
  add_c80("what is the total area of nations that should border iraq?"),
  add_c80("what is the total area of nations that are bordered by iraq?"),
  add_c81("what ocean does not border any country ?"),
  add_history1(test_chat80),
  add_c81("what oceans should border any country?"),
  add_c81("is china a country?"),
  add_c80("are china and japan a country?"),
  add_c80("is china and japan a country?"),
  add_c80("are china and japan countries?"),
  add_c81("china is a country?"),
  add_c80("does John like cars?"),
  add_c80("does John drink water?"),
  add_c80("does afganistan border china?"),
  add_c80("Which rivers are not in asia?"),
  retrain_2.

retrain_2:-
  add_c81("If there is an animal X then X barks and it is false that there is a cat."),
  add_c81("If there is an animal X then X barks and it is false that X is a cat."),
  add_c81("If there is a dog X then X barks and it is false that there is a cat."),
  add_c81("If there is a dog then the dog barks and it is false that there is a cat."),
  add_c81("It is false that a man who waits or who eats runs or sleeps ,and barks."),
  add_c81("The man who talks or who walks or who sleeps eats."),
  add_history1((ape_test(_,X),input_to_middle(X))).

:- add_history1(s81).


s81:- make,s811(show_c80),s811(p1(cvt_to_objecteese)).

p1(P2,X):- atom(P2), \+ current_predicate(P2/1),  current_predicate(P2/2),!, 
  any_to_string(X,S),nl,dmsg(?-p1(P2,S)),call(P2,S,Y),wdmsg(Y),nl,!.
p1(P1,X):- any_to_string(X,S),append_term(P1,S,G),nl,dmsg(?-G),call(G),nl,!.
s81(P):- s811(p1(P)).
s811(P):-
  %forall(ape_test(_,X),call(P,X)),
  forall(training_data(X,_),call(P,X)),
  forall(test_e2c(X,_),call(P,X)),
  forall(chat80_all(X,_,_),call(P,X)).
  
%:- add_c80("does joe eat cake?").


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
  



:- if(\+ prolog_load_context(reloading, true)).
%:- forall(chat80_test(X),cvt_to_objecteese(X)).
%:- s81.
:- endif.

:- fixup_exports.

:- if(\+ prolog_load_context(reloading, true)).
:- initialization(prolog,main).
:- endif.
:- retractall(wt_replacement(chat80,_,country,_)).
:- retractall(wt_replacement(chat80,_,border,_)).

%:- forall(ti(country,Word),learn_penn_tag('NN',Word)).
%:- listing(wt_replacement/4).

:- add_history1(test_chat80).

%:- test_chat80.
