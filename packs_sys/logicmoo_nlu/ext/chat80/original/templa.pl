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

:- op(400,xfy,&).

thing_LF(person,_,X,ti(person,X),[],_).
trans_LF(contain,Spatial&_,X,Spatial&_,Y, trans_pred(Spatial,contain,X,Y),[],_,_).
trans_LF(have,Spatial&_,X,Spatial&_,Y, trans_pred(Spatial,have,X,Y),[],_,_).


thing_LF_access(Continent,Spatial&Geo&Continent,X,ti(Continent,X),[],_):- like_type(Geo,continent,Continent), spatial(Spatial).
thing_LF(OceanOrSea,Path,X,ti(OceanOrSea,X),Nil,Any):- ti_subclass(OceanOrSea,Seamass), like_type(_Geo,seamass,Seamass), Seamass=seamass,
   thing_LF(Seamass,Path,X,ti(Seamass,X),Nil,Any).

thing_LF(City,SpatialCity,X,ti(City,X),[],_):- like_type(geo,city,City), spatial(Spatial), bfeature_path(Spatial,City,SpatialCity).
thing_LF(Seamass,Spatial&Geo&Seamass,X,ti(Seamass,X),[],_):- spatial(Spatial),like_type(Geo,seamass,Seamass).

name_template_lf0(X,SpatialCity) :-like_type(geo,city,City), ti(City,X), spatial(Spatial), bfeature_path(Spatial,City,SpatialCity).
name_template_lf0(X,Spatial&Geo&Continent) :- like_type(Geo,continent,Continent), spatial(Spatial), ti(Continent,X).
name_template_lf0(X,Spatial&Geo&Country) :- like_type(Geo,country,Country), spatial(Spatial), ti(Country,X).
name_template_lf0(X,Spatial&Circle_of_latitude) :- like_type(geo,circle_of_latitude,Circle_of_latitude), ti(Circle_of_latitude,X), spatial(Spatial).
name_template_lf0(X,Spatial&Geo&Seamass) :- like_type(Geo,seamass,Seamass),spatial(Spatial), ti(Seamass,X).






thing_LF(river,SpatialRiver,X,ti(River,X),[],_):- like_type(geo,river,River),spatial(Spatial), bfeature_path(Spatial,River,SpatialRiver).
name_template_lf0(X,SpatialRiver) :- like_type(_Geo,river,River), ti(River,X), spatial(Spatial), bfeature_path(Spatial,River,SpatialRiver).
name_template_lf0(X,Spatial&_) :- like_type(_Geo,region,Region),spatial(Spatial), ti(Region,X).




thing_LF(Place,  Spatial&_,          X,ti(Place,X),  [],_):- spatial(Spatial), place_lex(Place).
thing_LF(Region, Spatial&_,          X,ti(Region,X), [],_):- spatial(Spatial),like_type(_Geo,region,Region).
thing_LF(Country,Spatial&Geo&Country,X,ti(Country,X),[],_):- spatial(Spatial),like_type(Geo,country,Country).
thing_LF(Capital,SpatialCity,X,ti(Capital_city,X),[],_):- 
   unique_of_obj(_Geo,Spatial,_Country,_Govern,Capital,City,Capital_city,_Nation_capital),
   spatial(Spatial), bfeature_path(Spatial,City,SpatialCity).

trans_LF(Govern,SpatialCity,X,Spatial&Geo&Country,Y,specific_pred(Spatial,Nation_capital,Y,X),[],_,_):-
  unique_of_obj(Geo,Spatial,Country,Govern,_Capital,City,_Capital_city,Nation_capital),
  spatial(Spatial), bfeature_path(Spatial,City,SpatialCity).


ttribute_LF(populous,Spatial&_,X,measure&population/*citizens*/,Y,count_pred(Spatial,population/*citizens*/,Y,X)).

attribute_LF(large,Spatial&_,X,measure&Area,Y,measure_pred(Spatial,Area,X,Y)):- type_measure_pred(_,size,Area,_).
attribute_LF(small,Spatial&_,X,measure&Area,Y,measure_pred(Spatial,Area,X,Y)):- type_measure_pred(_,size,Area,_).
attribute_LF(great,measure&Type,X,measure&Type,Y,exceeds(X,Y)).


/* Measure */


unit_format(Population,X--million):- unit_format(Population,X--thousand).
unit_format(Population,_X--thousand):- type_measure_pred(_City,sizeP,Population,countV).
unit_format(Latitude,_X--Degrees):- type_measure_pred(_Region, position(_),Latitude,Degrees).
%unit_format(Longitude,_X--Degrees):- measure_pred(_Region,position(x),Longitude,Degrees).
unit_format(Area,_X--Ksqmiles):- type_measure_pred(_Region,size,Area,Ksqmiles).

measure_LF(Unit,measure&Size,[],Units):- type_measure_pred(_City,sizeP,Size,Units), atom_concat(Unit,'s',Units).
measure_LF(sqmile,measure&Size,[],sqmiles):- measure_LF(ksqmile,measure&Size,[],ksqmiles).
measure_LF(sqmile,measure&area,[],sqmiles).
measure_LF(Degree,measure&position,[],Degrees):- type_measure_pred(_Region, position(_),_Latitude,Degrees), atom_concat(Degree,'s',Degrees).
measure_LF(thousand,measure&Population/*citizens*/,[],thousand):-  type_measure_pred(_City,sizeP,Population,countV).
measure_LF(million,measure&Population/*citizens*/,[],million):-  type_measure_pred(_City,sizeP,Population,countV).

verb_type_db(chat80,border,main+tv).
symmetric_verb(Spatial,border):- spatial(Spatial).


ordering_pred(spatial,cp(east,of),X1,X2) :- type_measure_pred(_Region,position(x),Longitude,_), position_pred(spatial,Longitude,X1,L1), position_pred(spatial,Longitude,X2,L2), exceeds(L2,L1).
ordering_pred(spatial,cp(north,of),X1,X2) :- type_measure_pred(_Region,position(y),Latitude,_),  position_pred(spatial,Latitude,X1,L1), position_pred(spatial,Latitude,X2,L2), exceeds(L1,L2).
ordering_pred(spatial,cp(south,of),X1,X2) :- type_measure_pred(_Region,position(y),Latitude,_),  position_pred(spatial,Latitude,X1,L1), position_pred(spatial,Latitude,X2,L2), exceeds(L2,L1).
ordering_pred(spatial,cp(west,of),X1,X2) :- type_measure_pred(_Region,position(x),Longitude,_), position_pred(spatial,Longitude,X1,L1), position_pred(spatial,Longitude,X2,L2), exceeds(L1,L2).


/* Nouns */

property_LF(Capital,  SpatialCity,    X,Spatial&Geo&Country,Y,  
 specific_pred(Spatial,Nation_capital,Y,X),[],_,_):- 
   unique_of_obj(Geo,Spatial,Country,_Govern,Capital,City,_Capital_city,Nation_capital),
   feature_path1(Spatial,City,SpatialCity).

property_LF(Area,     measure&Area,    X,Spatial&_,Y,  measure_pred(Spatial,Area,Y,X),[],_,_):- type_measure_pred(_,size,Area,_).
property_LF(Latitude, measure&position,X,Spatial&_,Y, position_pred(Spatial,Latitude,Y,X),[],_,_):- type_measure_pred(_Region,position(y),Latitude,_).
property_LF(Longitude,measure&position,X,Spatial&_,Y, position_pred(Spatial,Longitude,Y,X),[],_,_):- type_measure_pred(_Region,position(x),Longitude,_).
property_LF(Population, measure&Population/*citizens*/, X,Spatial&_,Y,    count_pred(Spatial,Population/*citizens*/,Y,X),[],_,_):-
  type_measure_pred(_City,sizeP,Population,countV).

synonymous_thing(nation,country).

thing_LF_access(Area,measure&area,X,unit_format(Area,X),[],_):- type_measure_pred(_,size,Area,_).

thing_LF_access(Latitude,measure&position,X,unit_format(Latitude,X),[],_):- type_measure_pred(_Region,position(_Y),Latitude,_).

%thing_LF_access(Longitude,measure&position,X,unit_format(Longitude,X),[],_):- type_measure_pred(_Region,position(x),Longitude,_).
thing_LF_access(Population,measure&Population/*citizens*/,X,unit_format(Population,X),[],_):- type_measure_pred(_,sizeP,Population,_).


/* Prepositions */

adjunction_LF(in,Spatial&_-X,Spatial&_-Y,trans_pred(Spatial,contain,Y,X)).
adjunction_LF(cp(East,Of),Spatial&_-X,Spatial&_-Y,ordering_pred(Spatial,cp(East,Of),X,Y)).
/*
adjunction_LF(cp(east,of),Spatial&_-X,Spatial&_-Y,ordering_pred(Spatial,cp(east,of),X,Y)).
adjunction_LF(cp(west,of),Spatial&_-X,Spatial&_-Y,ordering_pred(Spatial,cp(west,of),X,Y)).
adjunction_LF(cp(north,of),Spatial&_-X,Spatial&_-Y,ordering_pred(Spatial,cp(north,of),X,Y)).
adjunction_LF(cp(south,of),Spatial&_-X,Spatial&_-Y,ordering_pred(Spatial,cp(south,of),X,Y)).
*/







/* Proper nouns */

name_template_LF(X,Type2):- name_template_lf0(X,Type1), type_conversion(Type1,Type2).



aggr_noun(average,_,_,average).
aggr_noun(sum,_,_,total).
aggr_noun(total,_,_,total).

meta_noun_LF(number,of,_,V,Spatial&_,X,P,numberof(X,P,V)):- spatial(Spatial).

spatial(spatial).
%:- if(false).
:- if(true).
feature_path1(Spatial,CR,Spatial&CR):- spatial(Spatial).
:- else.
feature_path1(Spatial,CR,Spatial&geo&CR):- spatial(Spatial).
:- endif.

%bfeature_path(Spatial,CR,CVT):-  feature_path1(Spatial,CR,TYPE), btype_conversion(TYPE,CVT).
bfeature_path(Spatial,_CR,_&_):-spatial(Spatial).


% thing_LF(geo,Spatial&_,X,ti(geo,X),[],_):- spatial(Spatial).

thing_LF(Nation,Path,X,LF,Slots,Other):- synonymous_thing(Nation,Country), thing_LF(Country,Path,X,LF,Slots,Other).


thing_LF_access(Noun,Type2,X,P,Slots,_):-
  thing_LF(Noun,Type1,X,P,Slots,_),
  btype_conversion(Type1,Type2).

btype_conversion(_,_).
type_conversion(Type1,Type2):- !, Type1=Type2.



/* Verbs */

/*
%verb_root_db(chat80,border).
trans_LF(border,Spatial&Geo&_,X,Spatial&Geo&_,Y,symmetric_pred(Spatial,borders,X,Y),[],_,_).
%regular_past_db(chat80,bordered,border).
%% superceeded regular_pres_db(chat80,border).
verb_form_db(chat80,bordering,border,pres+part,_):- .
verb_form_db(chat80,borders,border,pres+fin,3+sg).
verb_form_db(chat80,border,border,pres+fin,_+pl). %:- verb_root_db(chat80,border)
% ... because [which,countries,border,france,?] was not properly parsed (the singular form was)
verb_form_db(chat80,border,border,inf,_). %:- verb_root_db(chat80,border)
% ... because [does,france,border,belgium,?] was not properly parsed
verb_form_db(chat80,bordered,border,past+part,_). % :- regular_past_db(chat80,bordered,border).
*/

:- style_check(+singleton).

trans_LF(Border,Spatial&Geo&_,X,Spatial&Geo&_,Y,symmetric_pred(Spatial,Border,X,Y),[],_,_):- 
   verb_type_lex(Border,main+tv),
   symmetric_verb(Spatial, Border).


%use_lexicon_80(_):- !, true.
use_lexicon_80(chat80).
use_lexicon_80(chat80_extra).
use_lexicon_80(talkdb_verb(X)):- verb_type_db(chat80,X,_).
% use_lexicon_80(_):- fail.

:- import(talkdb:talk_db/6).
%                         nonfinite,  pres+fin, past+fin,  pres+part    past+part,
talkdb_talk_db(transitive,   border,  borders,  bordered,  bordering,  bordered).
talkdb_talk_db(  Transitive, Write,   Writes,   Wrote,     Writing,    Written):- 
  talkdb:talk_db(Transitive, Write,   Writes,   Wrote,     Writing,    Written).

%verb_root_lex(Write):-            talkdb_talk_db(_Transitive,Write,_Writes,_Wrote,_Writing,_Written).
verb_type_db(talkdb,Write,main+tv):-   \+ avoided_verb(Write), talkdb_talk_db( transitive,Write,_Writes,_Wrote,_Writing,_Written).
verb_type_db(talkdb,Write,main+iv):-  \+ avoided_verb(Write),   talkdb_talk_db( intransitive,Write,_Writes,_Wrote,_Writing,_Written).
%regular_past_lex(Wrote,Write):-   talkdb_talk_db(_Transitive,Write,_Writes, Wrote,_Writing,_Written).
% superceeded regular_pres_lex(Write):-         talkdb_talk_db(_Transitive,Write,_Writes,_Wrote,_Writing,_Written).

verb_form_db(chat80,A,B,C,D):- verb_form_db(talkdb,A,B,C,D).
% verb_form_db(chat80,A,B,C,D):- verb_form_db(talkdb,A,B,C,D).

verb_form_db(talkdb,Written,Write,past+part,_):-   talkdb_talk_db(_Transitive,Write,_Writes,_Wrote,_Writing, Written).
verb_form_db(talkdb,Writing,Write,pres+part,_):-   talkdb_talk_db(_Transitive,Write,_Writes,_Wrote, Writing,_Written).
verb_form_db(talkdb, Writes,Write,pres+fin,3+sg):- talkdb_talk_db(_Transitive,Write, Writes,_Wrote,_Writing,_Written).
verb_form_db(talkdb, Writes,Write,pres+fin,_):-    talkdb_talk_db(_Transitive,Write, Writes,_Wrote,_Writing,_Written).
verb_form_db(talkdb,  Write,Write,      inf,_):-   talkdb_talk_db(_Transitive,Write,_Writes,_Wrote,_Writing,_Written).
verb_form_db(talkdb,  Wrote,Write,past+fin,_):-    talkdb_talk_db(_Transitive,Write,_Writes, Wrote,_Writing,_Written).

verb_form_db(talkdb,A,B,C,D):- verb_form_db(clex,A,B,C,D).

:- import(clex_iface:clex_verb/4).
clex_verb80(Looked,Look,VerbType,Form):- clex_iface:clex_verb(Looked,Look,VerbType,Form).
%% superceeded regular_pres_lex(Look):- no_loop_check(verb_root_lex(Look)).
%verb_form_lex(Looking,Look,pres+part,_):- (atom(Looking)->atom_concat(Look,'ing',Looking);var(Looking)),
%  no_loop_check(verb_root_lex(Look)),atom(Look),atom_concat(Look,'ing',Looking).
% NEW TRY verb_root_lex(Look):- clex_verb80(_Formed,Look,_Iv,_Finsg).
% regular_past_lex(Looked,Look):- clex_verb80(Looked,Look,_Iv,prep_phrase).
verb_form_db(clex,Looks,Look,pres+fin,3+sg):-  clex_verb80(Looks,Look,_,finsg).
verb_form_db(clex,LookPL,Look,pres+fin,3+pl):- clex_verb80(LookPL,Look,_,infpl).
verb_type_db(clex,Look,main+ITDV):- clex_verb80(_Formed,Look,ITDV,_Finsg).

trans_LF(Assign,feature&_,X,dbase_t(Assign,X,Y), [slot(prep(To),feature&_,Y,_,free)],_):- clex_verb80(_Assigned, Assign, dv(To),_).


%trans_LF(Look,feature&_,X,dbase_t(Look,X,Y), [slot(prep(At),feature&_,Y,_,free)],_):- (tv_infpl(S,S);tv_finsg(S,S)), atomic_list_concat([Look,At],'-',S).

trans_LF(exceed,measure&Type,X,measure&Type,Y,exceeds(X,Y),[],_,_).


/* Adjectives */

restriction_LF(African,Spatial&_,X,ti(African,X)):- adj_lex(African,restr), spatial(Spatial).
%restriction_LF(american,Spatial&_,X,ti(american,X)).
%restriction_LF(asian,Spatial&_,X,ti(asian,X)).
%restriction_LF(european,Spatial&_,X,ti(european,X)).

aggr_adj(average,_,_,average).
aggr_adj(total,_,_,total).
aggr_adj(minimum,_,_,minimum).
aggr_adj(maximum,_,_,maximum).

/* Measure */

units_db(large,measure&_).
units_db(small,measure&_).

adj_sign_db(large,+).
adj_sign_db(small,-).
adj_sign_db(great,+).

/* Proportions and the like */

comparator_LF(proportion,_,V,[],proportion(V)).
comparator_LF(percentage,_,V,[],proportion(V)).




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
%database80(G):- \+ \+ clause(database801(G),G), !, database801(G).
database80(G):-  must(current_predicate(_,G)), call(G).


:- style_check(+singleton).


measure_pred(Spatial,Area,Where,Total) :- not_where(Where), 
 % ti(continent,Where),
 setof(Value:[Country],
               []^(database80(measure_pred(Spatial, Area, Country, Value)), 
               %database80(ti(country, Country)), 
               database80(trans_pred(Spatial,contain,Where,Country))),
               Setof),
         database80(aggregate80(total, Setof, Total)).


%exceeds(X--U,Y--U) :- !, X > Y.
%exceeds(X1--U1,X2--U2) :- ratio(U1,U2,M1,M2), X1*M1 > X2*M2.
exceeds(X,Y):- term_variables(X-Y,Vars),freeze_until(Vars,exceeds0(X,Y)),!.

freeze_until([],Goal):-!, term_variables(Goal, Vars),(Vars==[] -> Goal ; freeze_until(Vars,Goal)).
freeze_until([V|Vars],Goal):- freeze(V,freeze_until(Vars,Goal)),!.

exceeds0(X--U,Y--U) :- !, X > Y.
exceeds0(X1--U1,X2--U2) :- once((ratio(U1,U2,M1,M2), X1*M1 > X2*M2)).

ratio(thousand,million,1,1000).
ratio(million,thousand,1000,1).

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

ti(SC,X) :- ti_subclass(C,SC),ti(C,X).



% if X is contained in africa then X is african.
ti(An,X) :- agentitive_trans(Contains,Af,An), (trans_pred(spatial,Contains,Af,X);Af=X).

agentitive_trans(Contains,Af,An):- agentitive_trans_80(Contains,Af,An).



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




less_specific(Rise,  begin):-    type_specific_bte(_Type, _PathSystem,Rise,_,_).
less_specific(begin, start).
less_specific(Flow,  link):-     type_specific_bte(_Type, _PathSystem,_,Flow,_).
less_specific(link,  continue).
less_specific(Drain, end):-      type_specific_bte(_Type, _PathSystem,_,_,Drain).
less_specific(end,   stop).

% Lexical Data
% ------------------
%verb_type_db(chat80,Rise,main+iv):-  less_specific(Rise,  begin).
%verb_type_db(chat80,Flow,main+iv):-  less_specific(Flow,  link).
%verb_type_db(chat80,Drain,main+iv):- less_specific(Drain, end).
verb_type_db(chat80,LessSpecific,main+iv):- less_specific(_, LessSpecific).
verb_type_db(chat80,MoreSpecific,main+iv):- less_specific(MoreSpecific, _).

type_begins_thru_ends(Type, PathSystem, Begin, Continue, Stop):- 
  type_specific_bte(Type, PathSystem, Rise, Flow, Drain),
  maybe_less_specific(Rise,  Begin),
  maybe_less_specific(Flow,  Continue),
  maybe_less_specific(Drain, Stop).

maybe_less_specific(Drain, Drain).
maybe_less_specific(Drain, Stop):-
 less_specific(Drain, End),
 maybe_less_specific(End, Stop).




symmetric_pred(Spatial,B,X,C):- nonvar(X),nonvar(C),!,symmetric_direct(Spatial,B,X,C),!.
symmetric_pred(Spatial,B,X,C):- symmetric_direct(Spatial,B,X,C).

symmetric_direct(Spatial,B,X,C) :- direct_ss(Spatial,B,X,C).
symmetric_direct(Spatial,B,X,C) :- direct_ss(Spatial,B,C,X).

add_ss(Spatial,B,X,C):- X @> C, !, add_ss(Spatial,B,C,X).
add_ss(Spatial,B,X,C):- direct_ss(Spatial,B, X,C), !.
add_ss(Spatial,B,X,C):- assertz(direct_ss(Spatial,B, X,C)), !.
%:- add_ss(spatial,border,Albania,Greece).

:- abolish(tmp80:trans_rel_cache_creating,2).
:- abolish(tmp80:trans_rel_cache_created,2).
:- abolish(tmp80:trans_rel_cache_insts,3).
:- abolish(tmp80:trans_rel_cache,4).

:- dynamic(tmp80:trans_rel_cache_creating/2).
:- dynamic(tmp80:trans_rel_cache_created/2).
:- dynamic(tmp80:trans_rel_cache_insts/3).
:- dynamic(tmp80:trans_rel_cache/4).

trans_rel(Spatial,Contain,X,Y):- \+ compound(Contain),!,
  trace_or_throw(wrong(trans_rel(Spatial,Contain,X,Y))),
  trans_rel(=,trans_direct(Spatial,Contain),X,Y).

trans_rel(P1,P2,X,Y) :- trans_rel_cache_create(P1,P2),!, tmp80:trans_rel_cache(P1,P2,X,Y).
trans_rel(P1,P2,X,Y):- trans_rel_nc(P1,P2,X,Y).

trans_rel_nc(P1,P2,X,Y) :- var(X),!, no_repeats(X, trans_rel_rl(P1,P2,X,Y)).
trans_rel_nc(P1,P2,X,Y) :- nonvar(Y), !, trans_rel_lr(P1,P2,X,Y), !.
trans_rel_nc(P1,P2,X,Y) :- no_repeats(Y, trans_rel_lr(P1,P2,X,Y)).

trans_rel_lr(P1,P2,X,Y) :- call(P2,X,W), ( call(P1,W,Y) ; trans_rel_lr(P1,P2,W,Y) ).
trans_rel_rl(P1,P2,X,Y) :- call(P2,W,Y), ( call(P1,W,X) ; trans_rel_rl(P1,P2,X,W) ).



trans_rel_cache_create(P1,P2):- must_be(ground,(P1,P2)),
                                tmp80:trans_rel_cache_created(P1,P2),!.
trans_rel_cache_create(P1,P2):- tmp80:trans_rel_cache_creating(P1,P2),dmsg(looped(trans_rel_cache_create(P1,P2))),fail.
trans_rel_cache_create(P1,P2):-
  asserta((tmp80:trans_rel_cache_creating(P1,P2)),Ref),
  dmsg(trans_rel_cache_creating(P1,P2)),
  forall(call(P2,XX,YY),
     (assert_if_new(tmp80:trans_rel_cache_insts(P1,P2,XX)),
      assert_if_new(tmp80:trans_rel_cache_insts(P1,P2,YY)))),
  forall(tmp80:trans_rel_cache_insts(P1,P2,E),
        (forall(trans_rel_nc(P1,P2,E,Y),assert_if_new(tmp80:trans_rel_cache(P1,P2,E,Y))),
         forall(trans_rel_nc(P1,P2,Y,E),assert_if_new(tmp80:trans_rel_cache(P1,P2,Y,E))))),
  dmsg(trans_rel_cache_created(P1,P2)),
  asserta((tmp80:trans_rel_cache_created(P1,P2))),!,
  erase(Ref),
  %listing(tmp80:trans_rel_cache_insts(P1,P2,_Instances)),
  %listing(tmp80:trans_rel_cache(P1,P2,_,_)),
  !.


%contain(X,Y) :- trans_direct(spatial,contain,X,Y).
%contain(X,Y) :- trans_direct(spatial,contain,X,W), contain(W,Y).


trans_pred(Spatial,Contain,X,Y) :- trans_rel(=,trans_direct(Spatial,Contain),X,Y).
%contain(X,X).


:- multifile(trans_direct/4).
:- dynamic(trans_direct/4).


count_pred(Spatial,Heads,C,Total):- is_list(C),maplist(count_pred(Spatial,Heads),C,Setof), u_total(Setof, Total).

