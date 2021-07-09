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
|       This program may be used, copied, altered or included in other    |
|       programs only for academic purposes and provided that the         |
|       authorship of the initial program is aknowledged.                 |
|       Use for commercial purposes without the previous written          |
|       agreement of the authors is forbidden.                            |
|_________________________________________________________________________|

*/

% Data for the World Database.
% ---------------------------

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
database801(measure_pred(Type,P,X,Y)) :- measure_pred(Type,P,X,Y). % area of
database801(count_pred(Type,P,X,Y)) :- count_pred(Type,P,X,Y). % population of 
database801(position_pred(Type,P,X,Y)) :- position_pred(Type,P,X,Y). % latitude of
database801(ordering_pred(Type,P,X,Y)) :- ordering_pred(Type,P,X,Y). % south of
database801(symmetric_pred(Type,P,X,Y)) :- symmetric_pred(Type,P,X,Y). % border
database801(specific_pred(Type,P,X,Y)) :- specific_pred(Type,P,X,Y). % capital 
database801(trans_pred(Type,P,X,Y)) :- trans_pred(Type,P,X,Y). % contain 


%database80(path_pred(begins(Flow),rises,river,X,Y)) :- path_pred(begins(Flow),rises,river,X,Y).
%database80(path_pred(ends(Flow),drains,river,X,Y)) :- path_pred(ends(Flow),drains,river,X,Y).
database801(path_pred(PathSystemPart,ObjType,X,Y)) :- path_pred(PathSystemPart,ObjType,X,Y).
database801(path_pred_linkage(DirectPathSystem,ObjType,X,Y,Z)) :- path_pred_linkage(DirectPathSystem,ObjType,X,Y,Z).

database80((A,B)):- nonvar(A),!,database80(A),database80(B).
database80(G):- \+ \+ clause(database801(G),G), !, database801(G).
database80(G):-  must(current_predicate(_,G)), call(G).


:- style_check(+singleton).


%exceeds(X--U,Y--U) :- !, X > Y.
%exceeds(X1--U1,X2--U2) :- ratio(U1,U2,M1,M2), X1*M1 > X2*M2.
exceeds(X,Y):- term_variables(X-Y,Vars),freeze_until(Vars,exceeds0(X,Y)),!.

freeze_until([],Goal):-!, term_variables(Goal, Vars),(Vars==[] -> Goal ; freeze_until(Vars,Goal)).
freeze_until([V|Vars],Goal):- freeze(V,freeze_until(Vars,Goal)),!.

exceeds0(X--U,Y--U) :- !, X > Y.
exceeds0(X1--U1,X2--U2) :- once((ratio(U1,U2,M1,M2), X1*M1 > X2*M2)).

ratio(thousand,million,1,1000).
ratio(million,thousand,1000,1).

unit_format(population,_X--million).
unit_format(population,_X--thousand).

unit_format(area,_X--ksqmiles).

ratio(ksqmiles,sqmiles,1000,1).
ratio(sqmiles,ksqmiles,1,1000).


measure_pred(Spatial,Heads,C,Total):- is_list(C),maplist(measure_pred(Spatial,Heads),C,Setof), u_total(Setof, Total).


ti(NewType,X) :- agentitive_symmetric_type(Pred,SuperType), fail,
  % dont loop
  NewType\==SuperType, NewType\==SuperType, 
  % get the type names
  ti(SuperType,NewType), 
  % find the instances 
  symmetric_pred(spatial,Pred,NewType,X),
  % dont find instances already of the super type
  \+ ti(SuperType,X).

%ti(Sea,X) :- Sea\==seamass,Sea\==ocean,Sea\==sea, agentitive_symmetric_type(Borders,Sea), (symmetric_pred(spatial,Borders,Sea,X)).
%agentitive_symmetric_type(border,Baltic):- ti(seamass,Baltic).
% allows "baltic country" "pacific countries"   
agentitive_symmetric_type(border,seamass).

ti(SC,X) :- ti_subclass(C,SC),ti(C,X).

place_lex(place).

ti_subclass(continent,place).
ti_subclass(region,place).
ti_subclass(seamass,place).
ti_subclass(country,place).

% if X is contained in africa then X is african.
ti(An,X) :- agentitive_trans(Contains,Af,An), (trans_pred(spatial,Contains,Af,X);Af=X).

agentitive_trans(Contains,Af,An):- agentitive_trans_80(Contains,Af,An).

agentitive_trans_80(contain,africa,african).
agentitive_trans_80(contain,america,american).
agentitive_trans_80(contain,asia,asian).
agentitive_trans_80(contain,europe,european).

ordering_pred(spatial,cp(east,of),X1,X2) :- position_pred(spatial,longitude,X1,L1), position_pred(spatial,longitude,X2,L2), exceeds(L2,L1).
ordering_pred(spatial,cp(north,of),X1,X2) :- position_pred(spatial,latitude,X1,L1), position_pred(spatial,latitude,X2,L2), exceeds(L1,L2).
ordering_pred(spatial,cp(south,of),X1,X2) :- position_pred(spatial,latitude,X1,L1), position_pred(spatial,latitude,X2,L2), exceeds(L2,L1).
ordering_pred(spatial,cp(west,of),X1,X2) :- position_pred(spatial,longitude,X1,L1), position_pred(spatial,longitude,X2,L2), exceeds(L1,L2).

unit_format(latitude,_X--degrees).
unit_format(longitude,_X--degrees).

ti(circle_of_latitude,C):- circle_latitude(C,_).
position_pred(spatial,latitude,C,L):- circle_latitude(C,L).
circle_latitude(equator,0--degrees).
circle_latitude(tropic_of_cancer,23--degrees).
circle_latitude(tropic_of_capricorn,(-23)--degrees).
circle_latitude(arctic_circle,67--degrees).
circle_latitude(antarctic_circle,(-67)--degrees).

ti_subclass(ocean,seamass).
ti_subclass(sea,seamass).
ti(ocean,arctic_ocean).
ti(ocean,atlantic).
ti(ocean,indian_ocean).
ti(ocean,pacific).
ti(ocean,southern_ocean).
ti(sea,baltic).
ti(sea,black_sea).
ti(sea,caspian_sea).
ti(sea,mediterranean).
ti(sea,persian_gulf).
ti(sea,red_sea).
% @TODO ti(sea,caribian).




