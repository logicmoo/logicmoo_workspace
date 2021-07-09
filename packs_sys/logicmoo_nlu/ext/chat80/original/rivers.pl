
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
% NLU Logical Forms
% ------------------

% X rises in Begin
% % chat80("where does the rhine rise?") -> [switzerland]
% chat80("the rhine rises in switzerland ?      ").
% @TODO: X begins from Begin
intrans_LF(Start,SpatialType,X, LF,
   [slot(prep(In),Spatial&_,Begin,_,free)],_):- 
 type_begins_thru_ends(Type, PathSystem, Start, _Continue, _Stop),
 bfeature_path(Spatial,Type,SpatialType),member(In,[in,from,at]),
 LF = path_pred(begins(PathSystem),Type,X,Begin).

% X drains into End
intrans_LF(Stop,SpatialType,X, LF, 
   [slot(prep(Into),Spatial&_,End,_,free)],_):- 
 type_begins_thru_ends(Type, PathSystem, _Start, _Continue, Stop),
 bfeature_path(Spatial,Type,SpatialType),member(Into,[into,in,to,at]),
 LF = path_pred(ends(PathSystem),Type,X,End).

% X flows into Dest from Origin
chat80("the rhine flows into west germany?", true).
chat80("the rhine flows to west germany?", true).
chat80("the rhine flows in west germany?", true).
chat80("the rhine flows from west germany?", true).
chat80("the rhine flows at west germany?", true).
chat80("the rhine flows through west germany?", true).
chat80("the rhine flows into switzerland?", false).
chat80("the rhine flows to switzerland?", false).
chat80("the rhine flows in switzerland?", true).
chat80("the rhine flows from switzerland?", true).
chat80("the rhine flows at switzerland?", true).
chat80("the rhine flows through switzerland?", true).
intrans_LF(Continue,SpatialType,X,LF,
   [slot(prep(Into),Spatial&_,Dest,_,free),
    slot(prep(From),Spatial&_,Origin,_,free)],_):- 
 type_begins_thru_ends(Type, PathSystem, _Start, Continue, _Stop),
 bfeature_path(Spatial,Type,SpatialType),
 member(Into,[into,to,through,in,at]),
 member(From,[from,through,in,at]), 
 dif(From,Into),
 LF = path_pred_linkage(direct(PathSystem),Type,X,Origin,Dest).

% X flows through Begin
/*
intrans_LF(Continue,SpatialType,X,LF,
   [slot(prep(Through),Spatial&_,Link,_,free)],_):- 
 type_begins_thru_ends(Type, PathSystem, _Start, Continue, _Stop),
 bfeature_path(Spatial,Type,SpatialType),member(Through,[through,in]),
 LF = path_pred(thru_from(PathSystem),Type,X,Link).
*/

% Logical Rules
% ------------------
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

% Lexical Data
% ------------------
verb_type_db(chat80,rise,main+iv).
verb_type_db(chat80,flow,main+iv).
verb_type_db(chat80,drain,main+iv).

verb_type_db(chat80,LessSpecific,main+iv):-
  less_specific(_, LessSpecific).

less_specific(Rise,  begin):-    type_specific_bte(_Type, _PathSystem,Rise,_,_).
less_specific(begin, start).
less_specific(Flow,  link):-     type_specific_bte(_Type, _PathSystem,_,Flow,_).
less_specific(link,  continue).
less_specific(Drain, end):-      type_specific_bte(_Type, _PathSystem,_,_,Drain).
less_specific(end,   stop).


type_begins_thru_ends(Type, PathSystem, Begin, Continue, Stop):- 
  type_specific_bte(Type, PathSystem, Rise, Flow, Drain),
  maybe_less_specific(Rise,  Begin),
  maybe_less_specific(Flow,  Continue),
  maybe_less_specific(Drain, Stop).

maybe_less_specific(Drain, Drain).
maybe_less_specific(Drain, Stop):-
 less_specific(Drain, End),
 maybe_less_specific(End, Stop).

% superceeded verb_root_db(chat80,rise).
% superceeded verb_root_db(chat80,flow).
% superceeded verb_root_db(chat80,drain).

% superceeded regular_pres_db(chat80,rise).
% superceeded regular_pres_db(chat80,flow).
% superceeded regular_pres_db(chat80,drain).

% superceeded regular_past_db(chat80,flowed,flow).
% superceeded regular_past_db(chat80,drained,drain).

/*
% superceeded 
verb_form_db(chat80,rose,rise,past+fin,_).
verb_form_db(chat80,rises,rise,pres+fin,3+sg).
verb_form_db(chat80,risen,rise,past+part,_).
verb_form_db(chat80,flows,flow,pres+fin,3+sg).
verb_form_db(chat80,flowing,flow,pres+part,_).
verb_form_db(chat80,drains,drain,pres+fin,3+sg).
verb_form_db(chat80,draining,drain,pres+part,_).
*/

% Logical Interface to facts about rivers.
% ------------------

type_specific_bte(river,river_flows,rise,flow,drain).
path_nodes(river_flows,river,River,NodeList):- river_flows(River,NodeList).

% Facts about rivers.
% ------------------
river_flows(amazon,[atlantic,brazil,peru]).
river_flows(amu_darya,[aral_sea,soviet_union,afghanistan]).
river_flows(amur,[pacific,soviet_union,china,mongolia]).
river_flows(brahmaputra,[indian_ocean,bangladesh,china]).
river_flows(colorado,[pacific,mexico,united_states]).
river_flows(congo_river,[atlantic,zaire,zambia]).
river_flows(cubango,[botswana,south_africa,angola]).
river_flows(danube,[black_sea,romania,yugoslavia,hungary,czechoslovakia,austria,
              west_germany]).
river_flows(don,[black_sea,soviet_union]).
river_flows(elbe,[atlantic,west_germany,east_germany,czechoslovakia]).
river_flows(euphrates,[persian_gulf,iraq,syria,turkey]).
river_flows(ganges,[indian_ocean,india,china]).
river_flows(hwang_ho,[pacific,china]).
river_flows(indus,[indian_ocean,pakistan,india,china]).
river_flows(irrawaddy,[indian_ocean,burma]).
river_flows(lena,[arctic_ocean,soviet_union]).
river_flows(limpopo,[indian_ocean,mozambique,south_africa]).
river_flows(mackenzie,[arctic_ocean,canada]).
river_flows(mekong,[pacific,vietnam,cambodia,laos,china]).
river_flows(mississippi,[atlantic,united_states]).
river_flows(murray,[indian_ocean,australia]).
river_flows(niger_river,[atlantic,nigeria,niger,mali,guinea]).
river_flows(nile,[mediterranean,egypt,sudan,uganda]).
river_flows(ob,[arctic_ocean,soviet_union]).
river_flows(oder,[baltic,poland,czechoslovakia]).
river_flows(orange,[atlantic,south_africa,lesotho]).
river_flows(orinoco,[atlantic,venezuela,colombia]).
river_flows(parana,[atlantic,argentina,paraguay,brazil]).
river_flows(rhine,[atlantic,netherlands,west_germany,switzerland]).
river_flows(rhone,[mediterranean,france,switzerland]).
river_flows(rio_grande,[atlantic,mexico,united_states]).
river_flows(salween,[indian_ocean,burma,china]).
river_flows(senegal_river,[atlantic,senegal,mali,guinea]).
river_flows(seine,[atlantic,france]).
river_flows(tagus,[atlantic,portugal,spain]).
river_flows(vistula,[baltic,poland]).
river_flows(volga,[black_sea,soviet_union]).
river_flows(volta,[atlantic,ghana,upper_volta]).
river_flows(yangtze,[pacific,china]).
river_flows(yenisei,[arctic_ocean,soviet_union,mongolia]).
river_flows(yukon,[pacific,united_states,canada]).
river_flows(zambesi,[indian_ocean,mozambique,zambia,angola]).

