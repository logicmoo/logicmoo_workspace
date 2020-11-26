:- module(portray_vars, [debug_var/2,maybe_debug_var/2,pretty_numbervars/2,guess_pretty/1,
  into_symbol_name/2,
  prologcase_name/2,
  may_debug_var/2,
  maybe_debug_var/2,
  guess_varnames/1,
  guess_varnames/2,
  toCamelAtom0/2,
  simpler_textname/2,simpler_textname/3]).
:- set_module(class(library)).
:- use_module(util_varnames,[get_var_name/2]).

:- use_module(library(occurs)).
:- use_module(library(gensym)).
:- use_module(library(when)).

:- use_module(library(occurs)).
:- use_module(library(gensym)).
:- use_module(library(when)).


:- use_module(library(backcomp)).
:- use_module(library(codesio)).
:- use_module(library(charsio)).
:- use_module(library(debug)).
:- use_module(library(check)).


:- use_module(library(edinburgh)).
:- use_module(library(debug)).
:- use_module(library(prolog_stack)).
:- use_module(library(make)).


% :- use_module(library(gui_tracer)).
:- use_module(library(system)).
:- use_module(library(socket)).
:- use_module(library(readutil)).
:- abolish(system:time/1).
:- use_module(library(statistics)).
:- use_module(library(ssl)).
:- use_module(library(prolog_codewalk)).
:- use_module(library(prolog_source)).
:- use_module(library(date)).
%:- use_module(library(editline)).
:- use_module(library(listing)).

/*  Logicmoo Debug Tools
% ===================================================================
% File 'logicmoo_util_varnames.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'logicmoo_util_varnames.pl' 1.0.0
% Revision: $Revision: 1.1 $
% Revised At:  $Date: 2002/07/11 21:57:28 $
% ===================================================================
*/

% debug_var(_A,_Var):-!.
debug_var(X,Y):-  quietly(must_or_rtrace(debug_var0(X,Y))).
debug_var(Sufix,X,Y):- quietly((flatten([X,Sufix],XS),debug_var(XS,Y))).
maybe_debug_var(X,Y):- quietly(must_or_rtrace(may_debug_var(X,Y))).

p_n_atom(Cmpd,UPO):- p_n_atom1(Cmpd,UP),toPropercase(UP,UPO),!.
p_n_atom1(Cmpd,UP):- compound(Cmpd), sub_term(Atom,Cmpd),nonvar(Atom),\+ number(Atom), Atom\==[], catch(p_n_atom0(Atom,UP),_,fail),!.
p_n_atom1(Cmpd,UP):- Cmpd=='', UP='',!.
p_n_atom1(Cmpd,UP):- number(Cmpd),!,format(atom(UP),"_Num~w_",[Cmpd]).
p_n_atom1(Cmpd,UP):- term_to_atom(Cmpd,Atom),p_n_atom0(Atom,UP),!.

filter_var_chars([58|X],[107, 119, 95|Y]):- filter_var_chars_trim_95(X,Y).
filter_var_chars([95|X],[95|Y]):- !, filter_var_chars_trim_95(X,Y).
filter_var_chars(X,Y):- filter_var_chars_trim_95(X,Y).



filter_var_chars_trim_95(X,Y):- filter_var_chars0(X,M),trim_95(M,Y),!.

trim_95([X],[X]).
trim_95([95|M],Y):-!, trim_95(M,Y).
trim_95([X|L],[100,X|Y]):- char_type(X,digit), trim_96(L,Y).
trim_95([X|L],[97,X|Y]):- \+ char_type(X,alpha), trim_96(L,Y).
trim_95(X,Y):- trim_96(X,Y).

trim_96([95],[]).
trim_96([],[]).
trim_96([95,95|M],Y):- trim_96([95|M],Y).
trim_96([X|M],[X|Y]):- trim_96(M,Y).



filter_var_chars0([],[]).


% WATN WHEN MAKING SYMBOLs...  `_` -> `__`

%  `-` -> `c45`
filter_var_chars0(`-`,`c45`):-!.
%  `*` -> `_xx_`
filter_var_chars0([42|T],[95,120,120,95|Rest]):-!,filter_var_chars0(T,Rest).
%  `%` -> `_pf_`
filter_var_chars0([37|T],[95,112, 102, 95| Rest]):-!,filter_var_chars0(T,Rest).
%  `'` -> ``
filter_var_chars0([39|T], Rest):- !,filter_var_chars0(T,Rest).
%  `-` -> `_`
filter_var_chars0([45|T],[95|Rest]):-!,filter_var_chars0(T,Rest).
%  `:` -> `_`
filter_var_chars0([42|T],[95,120,95|Rest]):-!,filter_var_chars0(T,Rest).
filter_var_chars0([H|T],[H|Rest]):-  code_type(H, prolog_identifier_continue),!,filter_var_chars0(T,Rest).
filter_var_chars0([H|T],Rest):- number_codes(H,Codes), filter_var_chars0(T,Mid),append([95, 99|Codes],[95|Mid],Rest).

atom_concat_some_left(L,R,LR):- atom_concat_w_blobs(L,R,LR),atom_length(R,Len),Len>0.
atom_concat_some_left(L,R,LR):- upcase_atom(L,L0),L\==L0,atom_concat_w_blobs(L0,R,LR),atom_length(R,Len),Len>0.
atom_concat_some_left(L,R,LR):- downcase_atom(L,L0),L\==L0,atom_concat_w_blobs(L0,R,LR),atom_length(R,Len),Len>0.


reduce_atomLR(L,L):- \+ atom(L).
reduce_atomLR(L,R):- atom_concat_some_left('Cl_',LL,L),reduce_atomLR(LL,R).
reduce_atomLR(L,R):- atom_concat_some_left('U_',LL,L),reduce_atomLR(LL,R).
reduce_atomLR(L,R):- atom_concat_some_left('F_',LL,L),reduce_atomLR(LL,R).
reduce_atomLR(L,R):- atom_concat_some_left('Pf_',LL,L),reduce_atomLR(LL,R).
reduce_atomLR(L,R):- atom_concat_some_left('Kw_',LL,L),reduce_atomLR(LL,R).
reduce_atomLR(L,R):- atom_concat_some_left('Sys_',LL,L),reduce_atomLR(LL,R).
reduce_atomLR(L,L).

%p_n_atom0(Atom,UP):- simpler_textname(Atom,M),Atom\==M,!,p_n_atom0(M,UP).
p_n_atom0(Atom,UP):- atom(Atom),!,
  reduce_atomLR(Atom,AtomR),
  name(AtomR,[C|Was]),to_upper(C,U),filter_var_chars([U|Was],CS),name(UP,CS).
p_n_atom0(String,UP):- string(String),!,string_to_atom(String,Atom),!,p_n_atom0(Atom,UP).
p_n_atom0([C|S],UP):- !,notrace(catch(atom_codes_w_blobs(Atom,[C|S]),_,fail)),!,p_n_atom0(Atom,UP).

atom_codes_w_blobs(Atom,Codes):-atom(Atom)->atom_codes(Atom,Codes);format(codes(Codes),"~w",[Atom]).

debug_var0(_,NonVar):-nonvar(NonVar),!.
debug_var0(Var,_):- var(Var),!.
debug_var0([C|S],Var):- notrace(catch(atom_codes_w_blobs(Atom,[C|S]),_,fail)),!,afix_varname(Atom,Var).
debug_var0([AtomI|Rest],Var):-!,maplist(p_n_atom,[AtomI|Rest],UPS),atomic_list_concat(UPS,NAME),afix_varname(NAME,Var),!.
debug_var0(Atom,Var):- p_n_atom(Atom,UP),  
  check_varname(UP),
  afix_varname(UP,Var),!.


afix_varname(Suffix,Var):- var(Var), get_var_name(Var,Prev),atomic(Prev),afix_varname_w_prev(Prev,Suffix,Var).
afix_varname(Suffix,Var):- add_var_to_env_trimed(Suffix,Var).

afix_varname_w_prev(Suffix,Prev,Var):- atom_concat_w_blobs('_',NewFix,Suffix),!,afix_ordered_varname(Prev,NewFix,Var).
afix_varname_w_prev(Suffix,Prev,Var):- atom_concat_w_blobs(NewFix,'_',Suffix),!,afix_ordered_varname(NewFix,Prev,Var).
afix_varname_w_prev(Afix,Prev,Var):- atom_concat_w_blobs('_',NewPreFix,Prev),!,afix_ordered_varname(Afix,NewPreFix,Var).
afix_varname_w_prev(Afix,Prev,Var):- atom_concat_w_blobs(NewPreFix,'_',Prev),!,afix_ordered_varname(NewPreFix,Afix,Var).
afix_varname_w_prev(Suffix,Prev,Var):- afix_ordered_varname(Prev,Suffix,Var).
%afix_varname_w_prev(UP,_Prev,Var):- add_var_to_env_trimed(UP,Var).

afix_ordered_varname(Left,Right,_Var):- atom_contains(Left,Right),!.
afix_ordered_varname(Left,Right,_Var):- atom_contains(Right,Left),!.
afix_ordered_varname(Left,Right, Var):- atomic_list_concat([Left,'_',Right],New),
  add_var_to_env_trimed(New,Var).

add_var_to_env_trimed(New,Var):- atom_length(New,Len), Len < 2, !, add_var_to_env(New,Var).
add_var_to_env_trimed(New,Var):- atom_concat_w_blobs(NewNew,'_',New),add_var_to_env_trimed(NewNew,Var).
add_var_to_env_trimed(New,Var):- atom_concat_w_blobs(NewNew,'_v',New),add_var_to_env_trimed(NewNew,Var).
add_var_to_env_trimed(New,Var):- atom_concat_w_blobs('_',NewNew,New),add_var_to_env_trimed(NewNew,Var).
add_var_to_env_trimed(New,Var):- atom_concat_w_blobs('?',NewNew,New),add_var_to_env_trimed(NewNew,Var).
add_var_to_env_trimed(New,Var):- add_var_to_env(New,Var).
%afix_ordered_varname(UP,_Prev,Var):- add_var_to_env_trimed(UP,Var).

check_varname(UP):- name(UP,[C|_]),(char_type(C,digit)->throw(check_varname(UP));true).
                        
to_var_or_atom(L,LL):- var(L)->LL=L;(atom(L)->LL=L ; format(atom(LL),"~w",L)).

atom_concat_w_blobs(L,R,LR):- to_var_or_atom(L,LL),to_var_or_atom(R,RR),to_var_or_atom(LR,LLRR),
  atom_concat(LL,RR,LLRR).

resolve_char_codes('','_').
resolve_char_codes('pf','%').
%resolve_char_codes(C48,C):- notrace(catch((name(C48,[99|Codes]),number_codes(N,Codes),name(C,[N])),_,fail)),!,fail.
resolve_char_codes(C48,_):- notrace(catch((name(C48,[99|Codes]),number_codes(_,Codes)),_,fail)),!,fail.
resolve_char_codes(D1,N):- atom_concat_w_blobs('d',N,D1),notrace(catch(atom_number(N,_),_,fail)),!.
resolve_char_codes(C,CC):- atom_concat_w_blobs(C,'-',CC).

into_symbol_name(Atom,UPPER):- atomic(Atom),atomic_list_concat([Pkg|HC],'_',Atom),!,into_symbol_name([Pkg|HC],UPPER).
into_symbol_name(HC,UPPER):- maplist(resolve_char_codes,HC,RHC),atomics_to_string(RHC,'',STR),
   atom_trim_suffix(STR,'-',Trimed),string_upper(Trimed,UPPER),!.

% *PACKAGE* becomes xx_package_xx
% %MAKE-PACKAGE becomes pf_make_package

prologcase_name(I,O):-notrace(prologcase_name0(I,O)),assertion(O\=='').

prologcase_name0(String,Nonvar):-nonvar(Nonvar),!,prologcase_name(String,ProposedName),!,ProposedName==Nonvar.
prologcase_name0(String,ProposedName):- 
  string_lower(String,In),string_codes(In,Was),!,filter_var_chars(Was,CS),!,name(ProposedName,CS),!.


atom_trim_prefix(Root,Prefix,Result):- atom_concat_w_blobs(Prefix,Result,Root) -> true ; Result=Root.
atom_trim_suffix(Root,Suffix,Result):- atom_concat_w_blobs(Result,Suffix,Root) -> true ; Result=Root.

shrink_naut_vars(I,I).

pretty_numbervars(Term, TermO):- ground(Term), !, TermO=Term.
pretty_numbervars(Term, TermO):-
  shrink_naut_vars(Term,Term1),
  (ground(Term1) 
    -> TermO = Term1 ;
  (guess_pretty(Term1),
   source_variables_lwv(Term1,Vs),
   copy_term(Term+Vs,TermO+Vs2, _),
   implode_varnames_pred(to_var_dollar, Vs2))),!.


ground_variables_as_atoms(_Pred,[],_Vars):-!.
ground_variables_as_atoms(_Pred,_,[]):-!.
ground_variables_as_atoms(Pred,Vs,[N=V|Vars]):-
  ground_variables_as_atoms(Pred,Vs,Vars),
  (member_eq0(V, Vs) -> call(Pred,N,V) ; true).

implode_varnames_as_atoms(Term):-
   nb_current('$variable_names',Vars), 
   term_variables(Term,Vs),!,
   ground_variables_as_atoms(to_var_atom,Vs,Vars).

to_var_dollar(Name,V):- ignore(V='$VAR'(Name)).
to_var_atom(Name,V):- ignore(V=Name).
print_var_nv(N,V):- wdmsg(N=V).

:- meta_predicate(implode_varnames_pred(2,+)).

implode_varnames(Term):- implode_varnames_pred(to_var_dollar,Term).
implode_varnames_pred(P2, V):- var(V),!, ignore((get_var_name(V,Name),call(P2,Name,V))),!.
implode_varnames_pred(_, G):- ground(G),!.
implode_varnames_pred(P2, N=V):- atomic(N),!, ignore(call(P2,N,V)),!.
implode_varnames_pred(P2, [NV|Vars]):- implode_varnames_pred(P2, NV), implode_varnames_pred(P2, Vars).
implode_varnames_pred(P2, G):- term_variables(G,Vs),maplist(implode_varnames_pred(P2),Vs).
  

guess_pretty(H):- pretty_enough(H), !.
guess_pretty(O):- must_or_rtrace((ignore(guess_pretty1(O)),ignore(guess_varnames2(O,_)))).

guess_pretty1(H):- pretty_enough(H), !.
guess_pretty1(O):- must_or_rtrace(( ignore(pretty1(O)),ignore(pretty_two(O)),ignore(pretty_three(O)),ignore(pretty_final(O)))),!.
%make_pretty(I,O):- is_user_output,!,shrink_naut_vars(I,O), pretty1(O),pretty_three(O),pretty_final(O).
%make_pretty(I,O):- I=O, pretty1(O),pretty_three(O),pretty_final(O).

:- export(guess_varnames/1).

guess_varnames(IO):- guess_varnames(IO,_).

guess_varnames(I,O):- guess_pretty1(I), guess_varnames2(I,O).


guess_varnames2(I,O):-guess_varnames2(add_var_to_env_trimed,I,O).

:- meta_predicate guess_varnames2(2,*,*).
guess_varnames2(_Each,G,G):- pretty_enough(G),!.
guess_varnames2(Each, subrelation(V,N), subrelation(V,N)):- var(V), \+ variable_name(V,_), atomic(N),call(Each,N,V),!.
guess_varnames2(Each, isNamed(V,N), isNamed(V,N)):- var(V), \+ variable_name(V,_), atomic(N),call(Each,N,V),!.
guess_varnames2(Each, isNamed(V,H), isNamed(V,H)):- var(V), \+ variable_name(V,_),
   compound(H),functor(H,F,_),
   flag(skolem_count,SKN,SKN+1),
   toCamelcase(F,UF),atom_concat_w_blobs(UF,SKN,UF1),
   call(Each,UF1,V),!.
guess_varnames2(Each,H,H ):- H=..[F,V],var(V),
  \+ variable_name(V,_), 
  \+ atom_concat_w_blobs('sk',_,F), 
  \+ atom_concat_w_blobs(_,'Of',F), 
  \+ atom_concat_w_blobs(_,'Fn',F),
  flag(skolem_count,SKN,SKN+1),
  toCamelcase(F,UF),atom_concat_w_blobs(UF,SKN,UF1),
  call(Each,UF1,V),!.
guess_varnames2(Each,H,HH ):- H=..[F|ARGS],!,must_maplist_det(guess_varnames2(Each),ARGS,ARGSO),!,HH=..[F|ARGSO].
guess_varnames2(_Each, (G), (G)):- guess_pretty1(G),!.


/*
:- export(print_clause_plain/1).
print_clause_plain(I):-
  current_prolog_flag(color_term, Was),
  make_pretty(I,O),
    setup_call_cleanup(set_prolog_flag(color_term, false),
     (nl,lcolormsg1((O))),
     set_prolog_flag(color_term, Was)).
*/

%lcolormsg1(Msg):- mesg_color(Msg,Ctrl),!,ansicall_maybe(Ctrl,fmt9(Msg)).
lcolormsg1(Msg):- fmt9(Msg).

% print_clause_plain(C):- portray_clause_w_vars(O).

is_good_name(IsGood):- \+ atomic(IsGood),!,fail.
is_good_name([]):- !,fail.
is_good_name(IsBad):- atom_contains(IsBad,'_P_'), !, fail.
is_good_name(IsBad):- atom_contains(IsBad,'_Ret'), !, fail.
% is_good_name(IsBad):- atomic_list_concat([_,_,_|_],'_',IsBad), !, fail.
is_good_name(_IsGood).


may_debug_var(_,_,V):- nonvar(V),!.
may_debug_var(L,_,_):- upcase_atom(L,L),!.
may_debug_var(L,R,V):- atom(L),atom_concat_w_blobs('f_',LL,L), may_debug_var(LL,R,V).
may_debug_var(L,R,V):- atom(L),atomic_list_concat([_A1,A2,A3|AS],'_',L),atomic_list_concat([A2,A3|AS],'_',LL),may_debug_var(LL,R,V).
may_debug_var(L,R,V):- debug_var([L,R],V).

may_debug_var_v(R,V):- nonvar(R),var(V),may_debug_var(R,V).

may_debug_var_weak(_,V):- var(V), variable_name(V,_),!.
may_debug_var_weak(R,V):- may_debug_var(R,V),!.

may_debug_var(_,V):- var(V), variable_name(V,IsGood),is_good_name(IsGood),!.
%may_debug_var(R,V):- var(V), variable_name(V,_), atom(R), \+ is_good_name(R).
may_debug_var(R,V):- debug_var(R,V).

pretty_enough(H):- ground(H), !.
pretty_enough(H):- \+ compound(H),!. % may_debug_var(F,'_Call',H).
pretty_enough(H):- compound_name_arity(H,_,0), !.

name_one(V,R):- var(V), ground(R), debug_var(R,V).
name_one(R,V):- var(V), ground(R), debug_var(R,V).
name_one(_,_).

pretty1(H):- pretty_enough(H),!.
pretty1(as_rest(Name, Rest, _)):- may_debug_var_v(Name,Rest).
pretty1(get_var(Env, Name, Val)):- may_debug_var('GEnv',Env),may_debug_var(Name,Val).
pretty1(deflexical(Env,_Op, Name, Val)):- may_debug_var('SEnv',Env),may_debug_var(Name,Val).
pretty1(set_var(Env,Name, Val)):- may_debug_var('SEnv',Env),may_debug_var(Name,Val).

pretty1(f_slot_value(_Env, Name, Val)):- may_debug_var(slot,Name,Val),!.
%pretty1(get_kw(ReplEnv, RestNKeys, test, test, f_eql, true, True)
pretty1(Env=RIGHT):- compound(RIGHT),RIGHT=[List|_],compound(List),var(Env),List=[H|_],compound(H),H=bv(_,_), may_debug_var('Env',Env),
  maplist(pretty1,List).
pretty1(Env=List):- compound(List),var(Env),List=[H|_],compound(H),H=bv(_,_), may_debug_var('Env',Env),
  maplist_not_tail(pretty1,List).
%pretty1(P):- compound_name_arguments(P,_,[_|List]),append(_,[Name, Val|_],List),atom(Name),var(Val),may_debug_var(Name,Val).
pretty1(debug_var(R,V)):- may_debug_var(R,V).
pretty1(bv(R,V)):- name_one(V,R).
pretty1(isa(V,R)):- name_one(V,R).
pretty1(iza(V,R)):- name_one(V,R).
pretty1(cg_name(V,R)):- name_one(V,R).
pretty1(cg_type(V,R)):- name_one(V,R).
pretty1(cg_equal(V,R)):- name_one(V,R).
pretty1(cg_quantz(V,R)):- name_one(V,R).
pretty1(frame_var(V,R)):- name_one(V,R).
pretty1(pred(V,See,_,_)):- debug_var(See,V).
pretty1(rel(V,_,On,_)):- debug_var([On,'_'],V).
pretty1(card(V,Num,R)):- ground(Num:R),atomic_list_concat(['_',R,'_',Num],Eq_2),debug_var(Eq_2,V),!.
pretty1(H):-compound_name_arguments(H,_,ARGS),ignore(must_maplist_det(pretty1,ARGS)).

pretty_two(H):- pretty_enough(H),!.
pretty_two(H):- is_list(H), !, maplist(pretty_two,H).
pretty_two(H):- compound_name_arity(H,F,A),compound_name_arguments(H,F,ARGS),
   pretty_two(1,F,A,ARGS), !.

pretty_two(_,_,_,[]).
pretty_two(N,F,A,[E|ARGS]):-  
  Np1 is N + 1,
  ignore(maybe_nameable_arg(F,A,N,E)),
  pretty_two(Np1,F,A,ARGS).

maybe_nameable_arg(F,A,N,E):- compound(E)-> pretty_two(E) ; 
 ((var(E),arg_type_decl_name(F,A,N,T))-> may_debug_var(T,E) ; true).

ec_timed(EC23):- member(EC23,[holds_at,holds,releasedAt,happens]).
arg_type_decl_name(happens,2,2,when).
arg_type_decl_name(EC23,2,2,time_at):- ec_timed(EC23).
arg_type_decl_name(EC23,3,2,time_from):- ec_timed(EC23).
arg_type_decl_name(EC23,3,3,time_until):- ec_timed(EC23).
arg_type_decl_name(at,2,2,tloc).

:- meta_predicate(maplist_not_tail(1,*)).
maplist_not_tail(_,ArgS):- var(ArgS),!.
maplist_not_tail(G,[X|ArgS]):-call(G,X),maplist_not_tail(G,ArgS).


pretty_three(H):- pretty_enough(H),!. 
%pretty_three([H|T]):-!,maplist_not_tail(pretty_three,[H|T]).
pretty_three(H):-  
 ignore(((compound_name_arity(H,F,_), fail,
  nop((wl:init_args(N,F),integer(N),
   A is N + 1,   
   arg(A,H,R),may_debug_var_weak('KeysNRest',R)))),
   compound_name_arguments(H,F,[P1|ARGS]),  
   must_maplist_det(pretty_three,[P1|ARGS]))),!. 

pretty_final(H):- pretty_enough(H),!.
pretty_final([H | B]):- pretty_final(H),pretty_final(B),may_debug_var_weak('CAR',H),may_debug_var_weak('CDR',B).
pretty_final(H):- compound_name_arity(H,F,A),compound_name_arguments(H,F,[P1|ARGS]), pretty_final(H,F,A,P1,ARGS).

pretty_final(H,F,A,P1,ARGS):- atom_codes_w_blobs(F,[_,49|Rest]),atom_codes_w_blobs(F0,Rest),!,pretty_final(H,F0,A,P1,ARGS).
pretty_final(H,F,A,P1,ARGS):- atom_codes_w_blobs(F,[T|Rest]),\+ char_type(T, alpha), !,atom_codes_w_blobs(F0,Rest),!,pretty_final(H,F0,A,P1,ARGS).
pretty_final(_H,'',_A,P1,ARGS):- must_maplist_det(guess_varnames,[P1|ARGS]),!.
pretty_final(H,F,A,P1,ARGS):- 
   must_maplist_det(guess_varnames,[P1|ARGS]),
   arg(A,H,R),may_debug_var_weak([F,'_'],R),
   ignore((A>2, may_debug_var_weak([F,'_P_',A,'_v'],P1))),   
   !. 

atom_concat_or_rtrace_priv(X,Y,Z):- tracing->atom_concat_w_blobs(X,Y,Z);catch(atom_concat_w_blobs(X,Y,Z),_,(writeq(atom_concat_or_rtrace_priv(X,Y,Z)),break)).


:- export(i_name_lc/2).

%= 	 	 

%% i_name_lc( ?OType, ?IType) is semidet.
%
% Instance Name Not Loop Checked.
%
i_name_lc(OType,IType):-i_name(OType,IOType),!,string_equal_ci(IOType,IType).



%= 	 	 

%% to_iname( ?T, ?T) is semidet.
%
% Converted To Iname.
%
to_iname(T,TT):- var(T),!,freeze(T,to_iname(T,TT)).
to_iname(T,TT):- not(current_predicate(i_name/3)),!,T=TT.
%to_iname(T,TT):- (not_log_op(T),i_name(t,T,TT))->true;TT=T.



%= 	 	 

%% toUpperCamelcase( ?Type, ?TypeUC) is semidet.
%
% Converted To Upper Camelcase.
%
toUpperCamelcase(Type,TypeUC):-toCamelcase(Type,TypeUC). % ,toPropercase(TypeC,TypeUC),!.


icn_tcn(I,IC):-atom(I),i_name('t',I,IC)->I\==IC.

%= 	 	 

%% i_name( ?OType, ?IType) is semidet.
%
% Instance Name.
%
:- export(i_name/2).
i_name(OType,IType):-i_name('',OType,IOType),!,IOType=IType.

%= 	 	 

%% i_name( ?I, ?OType, ?IType) is semidet.
%
% Instance Name.
%
:- export(i_name/3).
i_name(I,OType,IType):- sanity((nonvar(I),nonvar(OType))),!,to_case_break_atoms(OType,[L|List]),!,i_name_4(I,OType,[L|List],IType).
%i_name(I,OType,IType):- typename_to_iname0(I,OType,IOType),!,IOType=IType.

switchable_itypes(L,_I):- downcase_atom(L,L),atom_length(L,LL),LL < 4,!.
i_name_4(I,  OType,[L|_List],IType):- I==L,!,OType=IType.
i_name_4(I, _OType,[L| List],IType):- switchable_itypes(L,I),atomic_list_concat([I|List],IType),!.
i_name_4(I, _OType,[L| List],IType):- toPropercase(L,U),atomic_list_concat([I,U|List],IType),!.




ti_name(I,OType,IType):- i_name(I,OType,IType).
:- export(ti_name/3).

%= 	 	 

%% typename_to_iname0( ?I, ?OType, ?IType) is semidet.
%
% Typename Converted To Iname Primary Helper.
%
%:- export(typename_to_iname0/3).
%typename_to_iname0(I, [], O):- trace_or_throw(bad_typename_to_iname0(I, [], O)).
%typename_to_iname0(I,OType,IType):- fail, (type_prefix(Prefix,_)),atom_concat_w_blobs(Prefix,Type,OType),capitalized(Type),!,typename_to_iname0(I,Type,IType).
%typename_to_iname0(I,Type,IType):-nonvar(Type),atom_concat_w_blobs(I,_UType,Type),Type=IType.
%typename_to_iname0(I,Type,IType):-nonvar(Type),toUpperCamelcase(Type,UType),atom_concat_w_blobs(I,UType,IType).

%= 	 	 

%% split_name_type( ?Suggest, ?InstName, ?Type) is semidet.
%
% Split Name Type.
%
split_name_type(Suggest,InstName,Type):- 
  maybe_notrace(split_name_type_0(Suggest,NewInstName,NewType)),!,
  must((NewInstName=InstName,NewType=Type)),!.
:- export(split_name_type/3).
:- '$hide'(split_name_type/3).

split_name_type_0(S,P,C):- string(S),!,atom_string(A,S),split_name_type_0(A,P,C),!.
%split_name_type_0(FT,FT,ttExpressionType):-a(ttExpressionType,FT),!,dmsg(trace_or_throw(ttExpressionType(FT))),fail.
split_name_type_0(T,T,C):- compound(T),compound_name_arity(T,C,_),!.
split_name_type_0(T,T,C):- quietly((once(atomic_list_concat_safe([CO,'-'|_],T)),atom_string(C,CO))).
split_name_type_0(T,T,C):- quietly((atom(T),atom_codes_w_blobs(T,AC),last(AC,LC),is_digit(LC),append(Type,Digits,AC),
  catch(number_codes(_,Digits),_,fail),atom_codes_w_blobs(CC,Type),!,i_name(t,CC,C))).
split_name_type_0(C,P,C):- atom(C),var(P),i_name(i,C,I),gensym(I,P),!.





%= 	 	 

%% toCamelAtom0( :TermA, ?O) is semidet.
%
% Converted To Camel Atom Primary Helper.
%
toCamelAtom0([A],O):-nonvar(A),!,toPropercase(A,O),!.
toCamelAtom0([A|List],O):-!,toPropercase(A,AO),toCamelAtom0(List,LO),atom_concat_w_blobs(AO,LO,O).
toCamelAtom0(A,O):-toPropercase(A,O),!.



%= 	 	 

%% to_prefixed( ?Prefix, ?I, ?O) is semidet.
%
% Converted To Prefixed.
%
to_prefixed(Prefix,I,O):-to_atomic_name(I,i_name(Prefix),O).

:- meta_predicate to_atomic_name(?,2,?).

%= 	 	 

%% to_atomic_name( ?I, :PRED2Pred, ?O) is semidet.
%
% Converted To Atomic Name.
%
to_atomic_name(I,Pred,O):-is_list(I),toCamelAtom0(I,A),!,to_atomic_name(A,Pred,O).
to_atomic_name(I,Pred,O):-string(I),!,string_to_atom(I,A),!,to_atomic_name(A,Pred,O).
%to_atomic_name(Name,Pred,O):-atomic(Name),ereq(mudKeyword(W,KW)),string_equal_ci(Name,KW),!,to_atomic_name(W,Pred,O).
to_atomic_name(Name,Pred,_):- not(atom(Name)),!,trace_or_throw(todo(not_atom_to_atomic_name(Name,Pred))).
to_atomic_name(Name,Pred,O):- call(Pred,Name,O).


simpler_textname(Name,Text):- simpler_textname(Name,'',Text).
simpler_textname(Name,Sep,Text):-atomic(Name),to_case_breaks(Name,ListN),to_case_breaks_trimed(Name,ListN,Sep,Text),!.

to_case_breaks_trimed(Name,[xti(TextL,ClassL),xti(TextR,ClassR)|ListN],Sep,Text):-  ClassL==ClassR,!,
    maplist(to_descriptive_name(Name),[xti(TextL,ClassL),xti(TextR,ClassR)|ListN],Desc),
    (string(Sep) -> atomics_to_string(Desc,Sep,Text) ; atomic_list_concat(Desc,Sep,Text)).

to_case_breaks_trimed(Name,[xti(_,lower),xti(TextR,ClassR)|ListN],Sep,Text):-
    maplist(to_descriptive_name(Name),[xti(TextR,ClassR)|ListN],Desc),
    (string(Sep) -> atomics_to_string(Desc,Sep,Text) ; atomic_list_concat(Desc,Sep,Text)).

to_case_breaks_trimed(Name,ListN,Sep,Text):- is_list(ListN),!,
    maplist(to_descriptive_name(Name),ListN,Desc),
    (string(Sep) -> atomics_to_string(Desc,Sep,Text) ; atomic_list_concat(Desc,Sep,Text)).



:- fixup_exports.

%to_descriptive_name(For,Desc,Atom):- type_descriptive_name(Type,Desc,Atom),isa(For,Type),!.
%to_descriptive_name(_For,Pefix,Desc):- (type_prefix(Pefix,TypeName)), simpler_textname(TypeName,Desc).
%to_descriptive_name(For,xti(Pefix,lower),Desc):-!,to_descriptive_name(For,Pefix,Desc).
to_descriptive_name(For,xti(Pefix,_),Desc):-!,to_descriptive_name(For,Pefix,Desc).
to_descriptive_name(_For,X,X).

:- multifile(user:portray/1).
:- dynamic(user:portray/1).
:- discontiguous(user:portray/1).

user:portray(Term):- fail,
  \+ ground(Term),
  pretty_numbervars(Term,PrettyVarTerm),
  Term \=@= PrettyVarTerm,
  prolog_pretty_print:print_term(PrettyVarTerm, [output(current_output)]).

:- nb_setval('$variable_names',[]).
