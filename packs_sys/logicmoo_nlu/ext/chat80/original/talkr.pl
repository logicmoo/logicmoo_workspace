/*
 _________________________________________________________________________
|       Copyright (C) 1982                                                |
|                                                                         |
|       David Warren,                                                     |
|               SRI International, 333 Ravenswood Ave., Menlo Park,       |
|               California 94025, USA;                                    |
|                                                                         |
|       Fernando Pereira,                                                 |
|               Dept. of Architecture, University of Edinburgh,           |
|               20 Chambers St., Edinburgh EH1 1JZ, Scotland              |
|                                                                         |
|       This program may Be used, copied, altered or included in other    |
|       programs only for academic purposes and provided that the         |
|       authorship of the initial program is aknowledged.                 |
|       Use for commercial purposes without the previous written          |
|       agreement of the authors is forbidden.                            |
|_________________________________________________________________________|

*/

/* Simplifying and executing the logical form of a NL query. */

:-public write_tree/1, answer80/1.
:-op(500,xfy,--).

write_tree(T):-
   numbervars80(T,0,_),
   wt(T,0),
   fail.
write_tree(_).

wt(T,_L) :- as_is_old(T),fmt(T),!.
wt((P:-Q),L) :- !, L1 is L+3,
   write(P), tab(1), write((:-)), nl,
   tab(L1), wt(Q,L1).
wt((P,Q),L) :- !, L1 is L-2,
   wt(P,L), nl,
   tab(L1), put("&"), tab(1), wt(Q,L).
wt({P},L) :- complex(P), !, L1 is L+2,
   put("{"), tab(1), wt(P,L1), tab(1), put("}").
wt(E,L) :- decomp(E,H,P), !, L1 is L+2,
   header80(H), nl,
   tab(L1), wt(P,L1).
wt(E,_) :- write(E).

header80([]).
header80([X|H]) :- reply(X), tab(1), header80(H).

decomp(setOf(X,P,S),[S,=,setOf,X],P).
decomp(\+(P),[\+],P) :- complex(P).
decomp(numberof(X,P,N),[N,=,numberof,X],P).
decomp(X^P,[exists,X|XX],P1) :- othervars(P,XX,P1).

othervars(X^P,[X|XX],P1) :- !, othervars(P,XX,P1).
othervars(P,[],P).

complex((_,_)).
complex({_}).
complex(setOf(_,_,_)).
complex(numberof(_,_,_)).
complex(_^_).
complex(\+P) :- complex(P).

% Query execution.

respond(true) :- reply('Yes.').
respond(false) :- reply('No.').
respond([]) :- reply('Nothing satisfies your question.'), nl.
respond([A|L]) :- !, reply(A), replies(L).
respond(A) :- reply(A).

answer80(S1):- answer8o2_g(S1,G,S),call(G),respond(S).

/*
answer802((answer80([]):-E),[B]) :- !, holds_truthvalue(E,B).
answer802((answer80([X]):-E),S) :- !, seto(X,E,S).
answer802((answer80(X):-E),S) :- seto(X,E,S).
*/
answer8o2_g((S1,S2),(G1,G2),S):- !, answer8o2_g(S1,G1,S), answer8o2_g(S2,G2,S).
answer8o2_g(X,G,Y):- answer803(X,Y,G0),expand_setos(G0,G).

answer802((S1,S2),(G1,G2)):- !, answer802(S1,G1), answer802(S2,G2).
answer802(X,Y):- answer803(X,Y,G),!,call802(G).

answer803((answer80([]):-E), [B], holds_truthvalue(E,B)).
answer803((answer80([X]):-E),S, seto(X,E,S)).
answer803((answer80(X):-E), S, seto(X,E,S)).

call802(holds_truthvalue(E,B)):- !, holds_truthvalue(E,B).
call802(seto(X,E,S)):- !, seto(X,E,S).
call802(G):- call(G).

/*
seto1(X,E,S) :- ground(X),
%	portray_clause(({X} :- E)),
	phrase(satisfy80(E,G),Vars),
	pprint_ecp_cmt(yellow,((X+Vars):-G)),!,
	seto2(X,Vars,G,S).

seto2(X,[],G,S):- !, (setof(X,G,S) -> ignore( S = [X] ) ;  S = []).
seto2(X,Vars,G,S):- setof(X,Vars^G,S) -> ignore( S = [X]) ;  S = [].
*/

expand_setos(O,O):- \+ compound(O),!.
expand_setos(holds_truthvalue(E,S),O):-
	phrase(satisfy80(E,G),_Vars),
	%pprint_ecp_cmt(yellow,((X+Vars):-G)),!,
	O=(G->S=true;S=false),!.
expand_setos(seto(X,E,S),O):-
	phrase(satisfy80(E,G),Vars),
	%pprint_ecp_cmt(yellow,((X+Vars):-G)),!,
	O=setof_oR_nil(X,Vars^G,S).
expand_setos(I, O):- phrase(satisfy80(I,O),_Vars),!. 
expand_setos(I, O):-
  compound_name_arguments(I, F, ARGS), 
  expand_setos(qvar_to_vvar, ARGS, ArgsO), 
  compound_name_arguments(O, F, ArgsO).

named_eq(X,X).

setof_oR_nil(X,G,S):- setof(X,G,S)*->true;S=[].

:- system:import(parser_chat80:setof_oR_nil/3).

seto(X,E,S) :-
%	portray_clause(({X} :- E)),
	phrase(satisfy80(E,G),Vars),
	%pprint_ecp_cmt(yellow,((X+Vars):-G)),!,
	(   setof(X,Vars^G,S) 
	*->  true
	;   S = []
	).



holds_truthvalue(E,True) :-
	phrase(satisfy80(E, G), _),
	(   %pprint_ecp_cmt(yellow,G),
      call(G)
	->  True = true
	;   True = false
	).
	
replies(Nil) :- Nil == [],!, reply('.').
replies([A]) :- !, reply(' and '), reply(A), reply('.').
replies([A|X]) :- is_list(X),!, reply(', '), reply(A), replies(X).
replies(A):- reply(A).

reply(N--U) :- !, write(N), write(' '), write(U).
reply(X) :- write(X).

%%	satisfy80(+Term, -Goal)//
%
%	Originally, Term was meta-interpreted. If we   dont want every
%	^/2-term to act as an existential quantification, this no longer
%	works. Hence, we now compile the term   into  a goal and compute
%	the existentially quantified variables.

%numberof(X,Vars^P,N):- !, setOf(X,Vars^P,S),length(S,N).
numberof(X,P,N):- setOf(X,P,S),length(S,N).

%satisfy(X,Y):- satisfy80(X,Y).

satisfy80((P0,Q0), (P,Q)) --> !, satisfy80(P0, P), satisfy80(Q0, Q).
satisfy80({P0}, (P->true)) --> !, satisfy80(P0, P).
satisfy80(X^P0, P) --> !, satisfy80(P0, P), [X].
satisfy80(\+P0, \+P) --> !, satisfy80(P0, P).
satisfy80(numberof(X,P0,N), Out) --> !,
	{ phrase(satisfy80(P0,P),Vars) },
	 Vars,			% S is an internal variable!
  {Out = (numberof(X,Vars^P,N))}.
satisfy80(numberof(X,P0,N), Out) --> !,
	{ phrase(satisfy80(P0,P),Vars) },
	[S], Vars,			% S is an internal variable!
  {Out = (setOf(X,Vars^P,S),length(S,N))}.
satisfy80(setOf(X,P0,S), setOf(X,Vars^P,S)) --> !,
	{ phrase(satisfy80(P0,P),Vars) },
	Vars.
satisfy80(+P0, \+ exceptionto(P)) --> !,
	satisfy80(P0, P).
satisfy80(X<Y, X<Y) --> !.
satisfy80(X=<Y, X=<Y) --> !.
satisfy80(X>=Y, X>=Y) --> !.
satisfy80(X>Y, X>Y) --> !.
satisfy80(database80(P), database80(P)) --> !.
satisfy80(P, database80(P)) --> [].

exceptionto(P) :-
   functor(P,F,N), functor(P1,F,N),
   pickargs(N,P,P1),
   exception80(P1).

exception80(P) :- database80(P), !, fail.
exception80(_P).

pickargs(0,_,_) :- !.
pickargs(N,P,P1) :- N1 is N-1,
   arg(N,P,S),
   pick(S,X),
   arg(N,P1,X),
   pickargs(N1,P,P1).

pick([X|_S],X).
pick([_|S],X) :- !, pick(S,X).
pick([],_) :- !, fail.
pick(X,X).




% Logical Rules
% ------------------
qualifiedBy(V,_,np_head(_Var,det(A),_Adjs,Ti)):- !, debug_var0([A,Ti],V).
qualifiedBy(_,_,_).
intrans_pred_prep(thing,_Thing1,Wait,_X,For,Y):- 
 debug_var([prep,For],Y),
 debug_var([intrans],Wait).

symmetric_pred(Spatial,B,X,C):- nonvar(X),nonvar(C),!,symmetric_direct(Spatial,B,X,C),!.
symmetric_pred(Spatial,B,X,C):- symmetric_direct(Spatial,B,X,C).

symmetric_direct(Spatial,B,X,C) :- direct_ss(Spatial,B,X,C).
symmetric_direct(Spatial,B,X,C) :- direct_ss(Spatial,B,C,X).


measure_pred(Spatial,Heads,C,Total):- is_list(C),maplist(measure_pred(Spatial,Heads),C,Setof), u_total(Setof, Total).

measure_pred(Spatial,Area,Where,Total) :- not_where(Where), 
 % ti(continent,Where),
 setOf(Value:[Country],
               []^(database80(measure_pred(Spatial, Area, Country, Value)), 
               %database80(ti(country, Country)), 
               database80(trans_pred(Spatial,contain,Where,Country))),
               Setof),
         database80(aggregate80(total, Setof, Total)).



bE(_,X,X).
named(X,X).

ti(noun_thing,_).

ti(NewType,X) :- agentitive_symmetric_type(Pred,SuperType), fail,
  % dont loop
  NewType\==SuperType, NewType\==SuperType, 
  % get the type names
  ti(SuperType,NewType), 
  % find the instances 
  symmetric_pred(thing,Pred,NewType,X),
  % dont find instances already of the super type
  \+ ti(SuperType,X).

ti(SC,X) :- ti_subclass(C,SC),ti(C,X).



% if X is contained in africa then X is african.
ti(An,X) :- agentitive_trans(Contains,Af,An), (trans_pred(thing,Contains,Af,X);Af=X).

agentitive_trans(Contains,Af,An):- agentitive_trans_80(Contains,Af,An).

intrans_pred_slots(Spatial,_Type,P,X,[slot_i(dirO,Y)]):- 
  generic_pred(_VV,Spatial,P,X,Y).

intrans_pred_slots(thing,Type,_Flow,X,[slot_i(prep(through),Origin)]):-
  path_pred_linkage(direct(_PathSystem),Type,X,Origin,_Dest).

intrans_pred_slots(thing,Type,_Flow,X,[slot_i(prep(into),Dest)]):-
  path_pred_linkage(direct(_PathSystem),Type,X,_Origin,Dest).

ti(Type,Inst) :- 
 type_specific_bte(Type, PathSystem,_Start,_Continue, _Stop),
 path_nodes(PathSystem,Type,Inst,_L).

path_pred(begins(PathSystem),Type,R,C) :-
 %type_specific_bte(Type, PathSystem, Start,_Continue,_Stop),
 path_nodes(PathSystem,Type,R,L), last_node(L,C).

path_pred(ends(PathSystem),Type,R,S) :- 
 %type_specific_bte(Type, PathSystem,_Start, _Continue, Stop),
 path_nodes(PathSystem,Type,R,L), first_node(L,S).

path_pred(thru_from(PathSystem),Type,R,C) :-
 %type_specific_bte(Type, PathSystem,_Start, _Continue, _Stop),
 path_pred_linkage(direct(PathSystem),Type,R,C,_).

path_pred_linkage(direct(PathSystem),Type,R,C1,C2) :- 
 %type_specific_bte(Type, PathSystem,_Start, _PathSystem, _Stop),
 path_nodes(PathSystem,Type,R,L), node_pairs_direct(L,C2,C1).

path_pred_linkage(indirect(PathSystem),Type,R,C1,C2) :- 
 %type_specific_bte(Type, PathSystem,_Start, _PathSystem, _Stop),
 path_nodes(PathSystem,Type,R,L), node_pairs_indirect(L,C2,C1).

% Logical Rule Helpers
% ------------------
first_node([X|_],X).

last_node([X],X).
last_node([_|L],X) :- last_node(L,X).

node_pairs_direct([X1,X2|_],X1,X2).
node_pairs_direct([_|L],X1,X2) :- node_pairs_direct(L,X1,X2).

node_pairs_indirect(L,C2,C1):- 
  node_pairs_direct(L,C2,CM),
  (CM=C1;node_pairs_indirect(L,CM,C1)).


freeze80(Var,Goal):- (nonvar(Var);(term_variables(Goal, Vars),Vars==[],trace)),!,call(Goal).
freeze80(Var,Goal):- freeze(Var,freeze80(Var,Goal)).

aux(have,_,_).
have(_,_).

%contain(X,Y) :- trans_direct(thing,contain,X,Y).
%contain(X,Y) :- trans_direct(thing,contain,X,W), contain(W,Y).
trans_pred(Spatial,Have,X,Y) :- Have == have, trans_pred(Spatial,contain,X,Y).

trans_pred(Spatial,Contain,X,Y) :- trans_rel(=,trans_direct(Spatial,Contain),X,Y).
%contain(X,X).


:- multifile(trans_direct/4).
:- dynamic(trans_direct/4).




count_pred(Spatial,Heads,C,Total):- is_list(C),maplist(count_pred(Spatial,Heads),C,Setof), u_total(Setof, Total).



:- style_check(-discontiguous).

:- discontiguous unit_format/2. 


% Interface.
% ---------

database801(aggregate80(X,Y,Z)) :- aggregate80(X,Y,Z).
database801(one_of(X,Y)) :- one_of(X,Y).
database801(ratio(X,Y,Z)) :- ratio(X,Y,Z).
database801(card(X,Y)) :- card(X,Y).
%database80(circle_of_latitude(X)) :- circle_of_latitude(X).
%database80(continent(X)) :- continent(X).
database801(exceeds(X,Y)) :- exceeds(X,Y).
database801(ti(Place,X)) :- ti(Place,X).
database801(X=Y) :- X=Y.
%database80(person(X)) :- person(X).	% JW: person is not defined


database801(unit_format(P,X)) :- unit_format(P,X).  % square miles
database801(measure_pred(_Type,P,X,Y)) :- measure_pred(_Type2,P,X,Y). % area of
database801(count_pred(Type,P,X,Y)) :- count_pred(Type,P,X,Y). % population of 
database801(position_pred(Type,P,X,Y)) :- position_pred(Type,P,X,Y). % latitude of
database801(ordering_pred(Type,P,X,Y)) :- ordering_pred(Type,P,X,Y). % south of
database801(symmetric_pred(Type,P,X,Y)) :- symmetric_pred(Type,P,X,Y). % border
database801(specific_pred(Type,P,X,Y)) :- specific_pred(Type,P,X,Y). % capital 
%database801(generic_pred(VV,thing,any,_X,_Y)):- true.
database801(generic_pred(VV,Type,P,X,Y)) :- generic_pred(VV,Type,P,X,Y). % capital 
database801(trans_pred(Type,P,X,Y)) :- trans_pred(Type,P,X,Y). % contain 

database801(modalized(_,X)):- database80(X).
database801(ought(X)):- database80(X).
database801(can(X)):- database80(X).
database801(will(X)):- database80(X).
database801(past(X)):- database80(X).
database801(not(X)):-  \+ database80(X).
database801(there(X)):- database80(X).

%database80(path_pred(begins(Flow),rises,river,X,Y)) :- path_pred(begins(Flow),rises,river,X,Y).
%database80(path_pred(ends(Flow),drains,river,X,Y)) :- path_pred(ends(Flow),drains,river,X,Y).
database801(path_pred(PathSystemPart,ObjType,X,Y)) :- path_pred(PathSystemPart,ObjType,X,Y).
database801(path_pred_linkage(DirectPathSystem,ObjType,X,Y,Z)) :- path_pred_linkage(DirectPathSystem,ObjType,X,Y,Z).

database80((A,B)):- nonvar(A),!,database80(A),database80(B).
database80(G):-  clause(database801(G),B), !, call(B).
database80(G):-  current_predicate(_,G), !, call(G).
database80(G):- dmsg(missing(database80(G))),fail.

:- style_check(+singleton).

remove_each_eq(Ys,[],Ys).
remove_each_eq(Ys,[X|Xs],Es):- exclude(==(X),Ys,Zs),remove_each_eq(Zs,Xs,Es).

setOf1(X,Y,Z):- term_variables(Y,Ys),term_variables(X,Xs),remove_each_eq(Ys,Xs,Es),!,(setof(X,Es^Y,Z)*->true;Z=[]).
setOf(X,Y,Z):- setof(X,Y,Z).

%exceeds(X--U,Y--U) :- !, X > Y.
%exceeds(X1--U1,X2--U2) :- ratio(U1,U2,M1,M2), X1*M1 > X2*M2.
exceeds(X,Y):- number(Y),!,X>Y.
exceeds(X,Y):- (var(Y)),!,X=Y.
exceeds(X,Y):- term_variables(X-Y,Vars),freeze_until(Vars,exceeds0(X,Y)),!.
  freeze_until([],Goal):-!, term_variables(Goal, Vars),(Vars==[] -> call(Goal) ; freeze_until(Vars,Goal)).
  freeze_until([V|Vars],Goal):- freeze(V,freeze_until(Vars,Goal)),!.  
  exceeds0('--'(X,U),'--'(Y,U)) :- !, X > Y.
  exceeds0('--'(X1,U1),'--'(X2,U2)) :- once((ratio(U1,U2,M1,M2), X1*M1 > X2*M2)).



ordering_pred(thing,cp(east,of),X1,X2) :- type_measure_pred( _Region,position(x),Longitude,_), position_pred(thing,Longitude,X1,L1), position_pred(thing,Longitude,X2,L2), exceeds(L2,L1).
ordering_pred(thing,cp(north,of),X1,X2) :- type_measure_pred(_Region,position(y),Latitude,_ ), position_pred(thing,Latitude,X1,L1), position_pred(thing,Latitude,X2,L2), exceeds(L1,L2).
ordering_pred(thing,cp(south,of),X1,X2) :- type_measure_pred(_Region,position(y),Latitude,_ ), position_pred(thing,Latitude,X1,L1), position_pred(thing,Latitude,X2,L2), exceeds(L2,L1).
ordering_pred(thing,cp(west,of),X1,X2) :- type_measure_pred( _Region,position(x),Longitude,_), position_pred(thing,Longitude,X1,L1), position_pred(thing,Longitude,X2,L2), exceeds(L1,L2).


generic_pred(VV,Type,mg(P),X,Y):- nonvar(P),!,generic_pred(VV,Type,P,X,Y).
generic_pred(VV,Type,prep(Of),X,Y):- Of == of, generic_pred0(VV,Type,_,Y,X).
generic_pred(VV,Type,P,X,Y):- P==in,!, generic_pred0(VV,Type,contain,Y,X).
generic_pred(VV,Type,P,X,Y) :- P == any,!, generic_pred0(VV,Type,_,X,Y).
generic_pred(VV,Type,P,X,Y) :- generic_pred0(VV,Type,P,X,Y)*->true;generic_pred1(Type,P,X,Y).

trans_pred_type(Type,P):- tmp80:trans_rel_cache_created(=, trans_direct(Type,P)).
trans_pred_type(thing,contain).

generic_pred0(_VV,Type,P,X,Y) :- trans_pred_type(Type,P), nonvar(P),trans_pred(Type,P,X,Y). % contain 
generic_pred0(_VV,Type,P,X,Y) :- nonvar(P), measure_pred(Type,P,X,Y). % area of
generic_pred0(_VV,Type,P,X,Y) :- nonvar(P), count_pred(Type,P,X,Y). % population of 
generic_pred0(_VV,Type,P,X,Y) :- nonvar(P), position_pred(Type,P,X,Y). % latitude of
generic_pred0(_VV,Type,P,X,Y) :- nonvar(P), ordering_pred(Type,P,X,Y). % south of
generic_pred0(_VV,Type,P,X,Y) :- nonvar(P), symmetric_pred(Type,P,X,Y). % border
generic_pred0(_VV,Type,P,X,Y) :- specific_pred(Type,P,X,Y). % capital 

generic_pred1(Type,P,X,Y) :- var(Type), nop(generic_pred1(Type,P,X,Y)).

lazy_pred(Type,Verb,TypeS,S,AllSlots):- dmsg(lazy_pred(Type,Verb,TypeS,S,AllSlots)).

lazy_pred_LF(Type,Verb,TypeS,S,AllSlots,P):-  
   %if_search_expanded(0),
   %dmsg(lazy_pred_LF(Type,Verb,TypeS,S,AllSlots)),!,
   % nonvar(AllSlots), 
   P = lazy_pred(Type,Verb,TypeS,S,AllSlots).


