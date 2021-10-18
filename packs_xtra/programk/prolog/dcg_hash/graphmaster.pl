% ===================================================================
% File 'graphmaster.pl'
% Purpose: An Implementation in SWI-Prolog of Graphmaster Index
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'graphmaster.pl' 1.0.0
% Revision: $Revision: 1.7 $
% Revised At: $Date: 2002/07/11 21:57:28 $
% ===================================================================


:- use_module(library(dictoo_lib)).
:- use_module(library(globals_api)).
%:- set_prolog_flag(generate_debug_info, false).
%:- cls.
% :- use_module(library(wam_cl/init)).

:- include(hashmap_oo).

% ===================================================================
% ===================================================================
track_now(Graph):- track_now(Graph, inst).
track_now(Graph, _Type):- hashtable_get(Graph, track_id, _), !.
track_now(Graph, Type):- gensym(Type, I), oo_set(Graph, track_id, I).

%%isStar0(Word1):- member(Word1, [*, '_']).
isStar0(X):-var(X), !, throw(isStar0(X)).
isStar0('*').
isStar0('_').

into_path(List, NList):- notrace((is_list(List), !, maplist(into_path, List, NList))), !.
into_path(List, NList):- atom(List), !, upcase_atom(List, NList).
into_path(List, NList):- compound(List), !, =(List, NList).
into_path(List, NList):- throw(into_path(List, NList)).

sameWords(Word1, Word2):-atom(Word1), atom(Word2), atoms_match0(Word1, Word2).
 atoms_match0(Word1, Word2):- (isStar0(Word1);isStar0(Word2)), !, fail.
 atoms_match0(Word1, Word1):-!.
 atoms_match0(Word1, Word2):-into_path(Word1, WordO), into_path(Word2, WordO), !.

into_name(Graph, Name):- atom(Graph), !, ignore((Graph=Name)).
into_name(Graph, Name):- is_hashtable(Graph), !, ignore((hashtable_get(Graph, name, Name))).

into_named_map(RB, Name, Graph, _ElseCall):- oo_get(RB, Name, Graph), !.
into_named_map(RB, Name, Graph, ElseCall):- hashtable_new(Graph), 
   call(ElseCall, Graph), oo_set(Graph, name, Name), track_now(Graph), oo_set(RB, Name, Graph).


:- nb_current('$graphs', _) -> true ; (hashtable_new( RB), nb_setval('$graphs', RB)).
into_graph(Name):- atom(Name), into_graph(Name, _O).
into_graph(Graph):- into_graph(_, Graph).
into_graph(Name, Graph):-  is_hashtable(Graph), !, ignore((hashtable_get(Graph, name, Name))).
into_graph(Name, Graph):- 
 ignore(Name=graphmaster), 
 into_name(Name, GName), 
 nb_getval('$graphs', RB), 
 into_named_map(RB, GName, Graph, make_graph).

make_graph(Graph):- hashtable_set(Graph, type, graph).

:- nb_current('$states', _) -> true ; (hashtable_new( RB), nb_setval('$states', RB)).
into_state(Name):- atom(Name), into_state(Name, _O).
into_state(State):- into_state(_, State).
into_state(Name, Graph):-  is_hashtable(Graph), !, ignore((hashtable_get(Graph, name, Name))).
into_state(Name, State):- 
 ignore(Name=statemaster),
 into_name(Name, GName), 
 nb_getval('$states', RB),
 into_named_map(RB, GName, State, make_state()).

make_state(State):- reset_state(State).
reset_state(State):- hashtable_set(State, star_name, star), hashtable_set(State, star_num, 1).

into_props(NState, Props, NPropsO):-
 must(cate_states(NState, NCate)),
 must(into_pairs(Props, Pairs)),
 must(append(NCate, Pairs, NProps)),
 flatten(NProps, NPropsO).

cate_states(NState, NCate):-into_pairs(NState, Pairs),
   include(cate_state, Pairs, NCate).

cate_state(N=_):- cate_prop(N).
cate_prop(pattern).
cate_prop(template).



% ===================================================================
% ===================================================================
set_template(Path, Template, Graph):- into_state(State),
  dmsg("adding..."),fmt(set_template(Path, Template)),
  set_pathprops( State, Path, template = (Template), Graph).

get_template(Path, Template, Graph):- into_state(State), get_pathprops( State, Path, template = (Template), Graph).

clear_graph(Graph):- notrace((into_graph(Graph, NGraph), hashtable_clear(NGraph))).

% ===================================================================
% ===================================================================
set_pathprops(Path, Props, Graph):- set_pathprops(_State, Path, Props, Graph).

set_pathprops(State, Path, Props, Graph):- 
 must(notrace((into_state(State, NState), 
          into_path(Path, NPath), 
          into_props([pattern=Path|NState], Props, NProps),
          into_graph(Graph, NGraph)))), 
 with_name_value(NState, star_num, 1,
    set_pathprop_now(NState, NPath, NProps, NGraph)).
 
set_pathprop_now(_State, [], Props, Graph):- !, 
 must(compound(Props)), 
 hashtable_set_props(Graph, Props),
 hashtable_set(Graph, [], Props).


set_pathprop_now(State, Path, Props, Graph):-
  \+ ground(Path), 
  make_path_props_v(Path, Props, PathV, PropsV), !,
  must(ground(PathV)),
  set_pathprop_now(State, PathV, PropsV, Graph).


set_pathprop_now(State, [W0|More], Props, Graph):- 
 path_expand(State, W0, W1, More),
 functor(W1, Index, _), !, 
 ( hashtable_get(Graph, Index, Next) 
   *-> set_pathprop_now( State, More, Props, Next)
    ; (hashtable_new(NewNode),       
       set_pathprop_now( State, More, Props, NewNode),
       (Index==W1 -> NewNodeTerm = NewNode ; w(W1, NewNode) = NewNodeTerm ),
       hashtable_set(Graph, Index, NewNodeTerm))).


make_path_props_v(Path, Props, PathV, PropsV):-
  term_variables(Path, PathVars),
  make_path_props_v(PathVars, Path, Props, PathV, PropsV).
make_path_props_v([], Path, Props, Path, Props):-!.
make_path_props_v([V|PathVars], Path, Props, PathV, PropsV):-
  gensym('PVAR_', PV),
  subst(Path, V, '$VAR'(PV), PathM),
  subst(Props, V, '$VAR'(PV), PropsM),
  make_path_props_v(PathVars, PathM, PropsM, PathV, PropsV).


 revarify(State, Call, GraphMid, CallV, GraphMidV):-
   sub_term(Sub, Call), compound(Sub),
   Sub='$VAR'(_), !,
   subst(Call, Sub, NewVar, CallM),
   subst(GraphMid, Sub, NewVar, GraphMidM),
   revarify(State, CallM, GraphMidM, CallV, GraphMidV).
 revarify(State, Call, GraphMid, CallV, GraphMidV):-
   sub_term(Sub, Call), compound(Sub),
   Sub='$'(NAME), unbound_get(State, NAME, NewVar), !,
   subst(Call, Sub, NewVar, CallM),
   subst(GraphMid, Sub, NewVar, GraphMidM),
   revarify(State, CallM, GraphMidM, CallV, GraphMidV).
revarify(_State, Call, GraphMid, Call, GraphMid).

  

% ===================================================================
% ===================================================================
get_pathprops(Path, Props, Graph):- get_pathprops(_State, Path, Props, Graph), !.

get_pathprops(_State, Path, Props, Graph):- is_hashtable(Graph), Path==[], !, hashtable_get_props(Graph, Props).
get_pathprops( State, Path, Props, Graph):-
 term_variables(Props, PropsV),
 notrace((into_state(State, NState), 
          into_path(Path, NPath),
          into_props([pattern=Path|NState], Props, NProps),          
          into_graph(Graph, NGraph))), 
 get_pathprops_now(NState, NPath, NProps, NGraph), !,
 ignore((PropsV==[Props], flatten(NProps, Props))).

get_pathprops_now( State, [W1|More], Props, Graph):- !, 
 hashtable_get(Graph, W1, Next), 
 get_pathprops_now( State, More, Props, Next).
get_pathprops_now(_State, _, Props, Graph):-                       
 hashtable_get_props(Graph, Props).


% ===================================================================
% ===================================================================
path_match(Path, Result):- path_match(_State, Path, _Graph, Result).

path_match(State, Path, Graph, Result):-
 must(notrace((into_state(State, NState), 
          =(Path, NPath), 
          into_graph(Graph, NGraph), 
          copy_term(Result, Result0),
          reset_state(NState)))),
 path_match_now(NState, NPath, NGraph, Result0),
 notrace((duplicate_term(Result0, Result),
 set_result_vars(NState, Result))), !.


set_result_vars(S, X):- 
  ignore((
     compound(X),
     forall(arg(N, X, E),
           (compound(E),
            ((E=get(A), hashtable_get(S, A, V))
             *-> nb_setarg(N, X, V)
             ; set_result_vars(S, E)))))).


call_with_filler(NewCall):- call(NewCall).


path_match_now(State, Path, Graph, Result):- 
  get_pathprops( State, Path, template = (Result), Graph).

/*
Matching Priorities

HELLO #
HELLO _
HELLO THERE
<set>greetings</set> = @greetings
HELLO ^
HELLO *
*/

% {Call}
path_match_now(State, InputList, Graph, Result):- 
 hashtable_get(Graph, '{}', Found),
 must(w('{}'(Call), GraphMid)=Found),
 revarify(State, Call, GraphMid, CallV, GraphMidV),
 call_with_filler(CallV),
 path_match_now(State, InputList, GraphMidV, Result).

path_match_now(_State, [], Graph, Result):- !,
 hashtable_get(Graph, '[]', Result). 
 

% Call_Star match #,_
path_match_now(State, InputList, Graph, Result):- 
 star_n(N, CStar, _), N < 3,
 atom_concat(call_star_,CStar, CS),
 hashtable_get(Graph, CS, Found),
 NEW =.. [CS, Star, Call],
 must(w(NEW, GraphMid)=Found),
 star_n(_, Star, Min), 
 subst(Call, Star, Left, NewCall),
 complex_match(State, Min, InputList, Left, _Right, call_with_filler(NewCall), GraphMid, Result).

% exact match
path_match_now(State, [Input|List], Graph, Result):- 
 into_path(Input, InputM),
 hashtable_get(Graph, InputM, GraphMid), 
 path_match_now(State, List, GraphMid, Result).

% @DCG
path_match_now(State, InputList, Graph, Result):- 
 hashtable_get(Graph, '@', Found),
 must(w('@'(DCG), GraphMid)=Found),  
 gm_phrase(DCG, InputList, Rest),
 path_match_now(State, Rest, GraphMid, Result).

% *DCG
path_match_now(State, InputList, Graph, Result):- fail,
 hashtable_get(Graph, '*', Found), \+ is_hashtable(Found),
 must(w('*'(DCG), GraphMid)=Found),  
 gm_phrase(DCG, InputList, Rest),
 append(Left,Rest,InputList),
 set_next_star(State, Left, 
 path_match_now(State, Rest, GraphMid, Result)).

% $VAR
path_match_now(State, InputList, Graph, Result):- 
 hashtable_get(Graph, '$', Found),
 must(w('$'(NAME), GraphMid)=Found),
 (unbound_get(State, NAME, RequiredValue)
   -> gm_phrase(req(RequiredValue), InputList, Rest) 
   ;  gm_phrase(NAME, InputList, Rest)), 
 append(Left,Rest,InputList),
 set_next_star(State, Left, 
 ((atom(NAME) -> hashtable_set(State, NAME, Left) ; true),
 path_match_now(State, Rest, GraphMid, Result))).

% Call_Star match ^,*
path_match_now(State, InputList, Graph, Result):- 
 star_n(N, CStar, _), N > 3,
 atom_concat(call_star_,CStar, CS),
 hashtable_get(Graph, CS, Found),
 NEW =.. [CS, Star, Call],
 must(w(NEW, GraphMid)=Found),
 star_n(_, Star, Min), 
 subst(Call, Star, Left, NewCall),
 complex_match(State, Min, InputList, Left, _Right, call_with_filler(NewCall), GraphMid, Result).


% Star match
path_match_now(State, InputList, Graph, Result):-
 star_n(_, Star, Min),
 hashtable_get(Graph, Star, GraphMid),   
 complex_match(State, Min, InputList, _Left, _Right, true, GraphMid, Result).


complex_match(State, Min, InputList, Left, Right, NewCall, GraphMid, Result):- 
 member(NextWord, InputList), 
 into_path(NextWord, NextWordU),
 hashtable_get(GraphMid, NextWordU, GraphNext), 
 length(Right, _),
 append(Left, [NextWord|Right], InputList), 
 length(Left, LL), LL>=Min, 
 set_next_star(State, Left,
 (call(NewCall),
 path_match_now(State, Right, GraphNext, Result))).

complex_match(State, Min, InputList, Left, Right, NewCall, GraphMid, Result):- 
 length(InputList, Max),
 length(Right, RMax), 
 (RMax > Max 
  -> (!,fail) 
  ; (append(Left, Right, InputList), 
     length(Left, LL), LL>=Min,
     set_next_star(State, Left,
     (call(NewCall),
     path_match_now(State, Right, GraphMid, Result))))).


gm_phrase( \+ DCG, InputList, Rest):- nonvar(DCG), !, \+ gm_phrase(DCG, InputList, Rest).
gm_phrase(DCG, InputList, Rest):- phrase(DCG, InputList, Rest).



set_next_star(State, Left, Goal):-
 hashtable_get(State, star_num, StarNum),
 hashtable_get(State, star_name, StarName),
 atom_concat(StarName, StarNum, StarVar),
 hashtable_set(State, StarVar, Left), !,
 StarNum2 is StarNum + 1,
 with_name_value(State, star_num, StarNum2, Goal).


with_name_value(State, Name, Value, Goal):-
 hashtable_get(State, Name, Was),
 hashtable_set(State, Name, Value),
  (Goal 
   *-> hashtable_set(State, Name, Was) 
    ; (hashtable_set(State, Name, Was), fail)).

unbound_get(State, NAME, RequiredValue):- hashtable_get(State, NAME, RequiredValue), \+ is_unbound(RequiredValue).
  
is_unbound(RequiredValue):- \+ is_list(RequiredValue).
 
%%REAL-UNUSED set_matchit1(StarName, Pattern, Matcher, OnBind):- length(Pattern, MaxLen0), MaxLen is MaxLen0 + 2, 
%%REAL-UNUSED set_matchit2(StarName, Pattern, Matcher, MaxLen, OnBind).

match_ci(H,W):- atom(H),atom(W),upcase_atom(H,U),upcase_atom(W,U).

req([]) --> [].
req([H|T]) --> [W],{match_ci(H,W)},req(T).

some([]) --> [].
some([H|T]) --> [H],some(T).

cd --> [c, d].
color --> [red].
color --> [blue].
color --> [green].


star_n(1, '#', 0).
star_n(2, '_', 1).
% star_n('phrase', 1).
% star_n('@', 1).
star_n(5,'^', 0).
star_n(6,'*', 1).

cmp_star(Star, Stuff, NEW):- atom_concat('call_star_',Star,CS), NEW =.. [CS, Star, Stuff].

path_expand(_State, call_star(Star,Stuff), NEW, _More):- cmp_star(Star, Stuff, NEW).
path_expand(_State, CMP, NEW, _More):- compound(CMP), functor(CMP, Star,1), star_n(_, Star,_), % Star\=='*', 
   arg(1, CMP, Stuff), cmp_star(Star,phrase(Stuff,Star,[]), NEW).
path_expand(_State, Star, NEW, [OStar| _More]):- star_n(_, Star,_),star_n(_, OStar,_), cmp_star(Star,phrase([_],Star,[]), NEW).
path_expand(_State, Star, NEW, _More):- star_n(_, Star,_), cmp_star(Star,phrase(some(_),Star,[]), NEW).
path_expand(_State, W, W, _More).

% ======================================================================
%   TEST EXPANSIONS
% ======================================================================

add_test_term_expansion( (:- (add_test(G,R), More)), TEST):- 
  nonvar(G), TEST = (path_match(G,R), More).
  
add_test_term_expansion( (:- (add_test(G,R))), TEST):- 
  (nonvar(R) 
   -> TEST = (path_match(G,R0), dmsg(R0), R0 = R)
    ; TEST = (path_match(G,R), dmsg(R))).


term_expansion(I,(:- assertz(test_call(TEST)), do_test(TEST))):- 
  add_test_term_expansion(I,TEST),!.
  
do_test(Test):- 
  format(user_error, '~N~n',[]),
  dmsg("==="),format(user_error, '% TEST: ~q.~n',[Test]),!,
  with_output_to(user_error,
    ((call(Test)->(ansi_format([fg(green)],'~w~n~n',[pass]),dmsg("==="));(ansi_format([fg(red)],'~w~n~n',[fail]),dmsg("==="),fail)))).

% ======================================================================
%   TESTS
% ======================================================================
:- into_graph(_, _).

%:- rtrace(set_template([a, b1, c], template_a_b1_c, _)).
%:- set_template([a, b2, c], template_a_b2_c, _).
%:- set_pathprops([a, b, c2, d, e], pattern([a, b, c2, d, e]), _).
%:- set_pathprops([a, b, c2, d, e], [a=aaaa, b=bbbb], _).

% ======================================================================
:- set_template([a, b, c, d, e], abcde, _).
:- add_test([a, b, c, d, e], abcde).

% ======================================================================
:- set_template([a, b, c2, d, e], abccde, _).
:- set_template([a, b, c2, d, e], abc2de, _).
:- add_test([a, b, c2, d, e], abc2de).

% ======================================================================
:- set_template([a, b, *, e], c3_fail(get(star1)), _).
:- set_template([a, b, '_'], c3_pass(get(star1)), _).
:- add_test([a, b, c3, d, e], c3_pass([c3, d, e])).

% ======================================================================
:- set_template([a, b2, *, d, e], b2_fail(get(star1)), _).
:- set_template([a, b2, '_', e], b2_pass(get(star1)), _).
:- add_test([a, b2, c4, d, e], b2_pass([c4, d])).

% ======================================================================
:- set_template([a, call_star(*, (member(*, [[b3]]))), c, d, e], b3(get(star1)), _).
:- add_test([a, b3, c, d, e], _).

% ======================================================================
:- set_template([a, {X=1}, b4, c, d, e], b4(X), _).
:- add_test([a, b4, c, d, e], b4(1)).

% ======================================================================
:- set_template([a, b5, @([c, d]), e], b5, _).
:- add_test([a, b5, c, d, e], b5).

% ======================================================================
:- set_template([a, b6, @(cd), e], b6, _).
:- add_test([a, b6, c, d, e], b6).

% ======================================================================
:- set_template([a, b7, '*'(color), d, e], b7(get(star1)), _).
:- add_test([a, b7, green, d, e],  b7([green])).

% ======================================================================
:- set_template([a, b8, '$'(color), d, e], b8(get(star1), get(color)), _).
:- add_test([a, b8, red, d, e], b8([red], [red])).

% ======================================================================
:- set_template([a, b9, '$'(color), '$'(color), e], b9(get(star1), get(color)), _).
:- add_test([a, b9, red, red, e], b9([red], [red])).

% ======================================================================
:- set_template([a, b10, @([c1];[c2]), d, e], b10, _).
:- add_test([a, b10, c2, d, e], _).
:- \+ path_match([a, b10, c3, d, e], _).

% ======================================================================
:- set_template([a, b11, '*'([c11]), d, e], b11(get(star1)), _).
:- add_test([a, b11, c11, d, e], _).

% ======================================================================
:- set_template([a, b12, '*'([c11,c12]), d, e], b12(get(star1)), _).
:- add_test([a, b12, c11, c12, d, e], _).

% ======================================================================
:- set_template([a, b13, '_'([c11,c12]), d, e], b13_pass(get(star1)), _).
:- set_template([a, b13, '*'([c11,c12]), d, e], b13_fail(get(star1)), _).
:- add_test([a, b13, c11, c12, d, e], _).

% ======================================================================
:- set_template([a, b14, *, *, e], b14_pass(get(star1),get(star2)), _).
:- add_test([a, b14, s1, s2, e], _).

% ======================================================================
%   RUN TESTS
% ======================================================================
:- show_name_values.

:- forall(test_call(Test),ignore(do_test(Test))).


% :- clear_graph(graphmaster).

