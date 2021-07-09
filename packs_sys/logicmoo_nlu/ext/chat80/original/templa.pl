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

/* Nouns */

spatial(spatial).
%:- if(false).
:- if(true).
feature_path1(Spatial,CR,Spatial&CR):- spatial(Spatial).
:- else.
feature_path1(Spatial,CR,Spatial&geo&CR):- spatial(Spatial).
:- endif.

bfeature_path(Spatial,CR,CVT):-  feature_path1(Spatial,CR,TYPE), btype_conversion(TYPE,CVT).

property_LF(capital,  SpatialCity,    X,Spatial&geo&country,Y,  
 specific_pred(Spatial,nation_capital,Y,X),[],_,_):- feature_path1(Spatial,city,SpatialCity).

property_LF(area,     measure&area,    X,Spatial&_,Y,  measure_pred(Spatial,area,Y,X),[],_,_).
property_LF(latitude, measure&position,X,Spatial&_,Y, position_pred(Spatial,latitude,Y,X),[],_,_).
property_LF(longitude,measure&position,X,Spatial&_,Y, position_pred(Spatial,longitude,Y,X),[],_,_).
property_LF(population, measure&population/*citizens*/, X,Spatial&_,Y,    count_pred(Spatial,population/*citizens*/,Y,X),[],_,_).

% thing_LF(geo,Spatial&_,X,ti(geo,X),[],_):- spatial(Spatial).



thing_LF(Nation,Path,X,LF,Slots,Other):- synonymous_thing(Nation,Country), thing_LF(Country,Path,X,LF,Slots,Other).

synonymous_thing(nation,country).

thing_LF_access(area,measure&area,X,unit_format(area,X),[],_).
thing_LF_access(latitude,measure&position,X,unit_format(latitude,X),[],_).
thing_LF_access(longitude,measure&position,X,unit_format(longitude,X),[],_).
thing_LF_access(population,measure&population/*citizens*/,X,unit_format(population,X),[],_).
thing_LF_access(continent,Spatial&geo&continent,X,ti(continent,X),[],_):- spatial(Spatial).

thing_LF_access(Noun,Type2,X,P,Slots,_):-
  thing_LF(Noun,Type1,X,P,Slots,_),
  btype_conversion(Type1,Type2).

btype_conversion(_,_).
type_conversion(Type1,Type2):- !, Type1=Type2.


thing_LF(Place,  Spatial&_,          X,ti(Place,X),  [],_):- spatial(Spatial), place_lex(Place).
thing_LF(region, Spatial&_,          X,ti(region,X), [],_):- spatial(Spatial).
thing_LF(country,Spatial&geo&country,X,ti(country,X),[],_):- spatial(Spatial).

thing_LF(OceanOrSea,Path,X,ti(OceanOrSea,X),Nil,Any):- ti_subclass(OceanOrSea,Seamass), Seamass=seamass,
   thing_LF(Seamass,Path,X,ti(Seamass,X),Nil,Any).

thing_LF(seamass,Spatial&geo&seamass,X,ti(seamass,X),[],_):- spatial(Spatial).

thing_LF(person,_,X,ti(person,X),[],_).

thing_LF(capital,SpatialCity,X,ti(capital_city,X),[],_):- spatial(Spatial), bfeature_path(Spatial,city,SpatialCity).
thing_LF(city,SpatialCity,X,ti(city,X),[],_):- spatial(Spatial), bfeature_path(Spatial,city,SpatialCity).
thing_LF(river,SpatialRiver,X,ti(river,X),[],_):- spatial(Spatial), bfeature_path(Spatial,river,SpatialRiver).


aggr_noun(average,_,_,average).
aggr_noun(sum,_,_,total).
aggr_noun(total,_,_,total).

meta_noun_LF(number,of,_,V,Spatial&_,X,P,numberof(X,P,V)):- spatial(Spatial).

/* Proper nouns */

name_template_LF(X,Type2):- name_template_lf0(X,Type1), type_conversion(Type1,Type2).

name_template_lf0(X,Spatial&circle) :-  ti(circle_of_latitude,X), spatial(Spatial).
name_template_lf0(X,SpatialCity) :- ti(city,X), spatial(Spatial), bfeature_path(Spatial,city,SpatialCity).
name_template_lf0(X,Spatial&geo&continent) :- spatial(Spatial), ti(continent,X).
name_template_lf0(X,Spatial&geo&country) :- spatial(Spatial), ti(country,X).
name_template_lf0(X,Spatial&_) :- spatial(Spatial), ti(region,X).
name_template_lf0(X,SpatialRiver) :- ti(river,X), spatial(Spatial), bfeature_path(Spatial,river,SpatialRiver).
name_template_lf0(X,Spatial&geo&seamass) :- spatial(Spatial), ti(seamass,X).

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

verb_type_db(chat80,border,main+tv).
symmetric_verb(Spatial,border):- spatial(Spatial).

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


trans_LF(contain,Spatial&_,X,Spatial&_,Y, trans_pred(Spatial,contain,X,Y),[],_,_).
trans_LF(govern,SpatialCity,X,Spatial&geo&country,Y,specific_pred(Spatial,nation_capital,Y,X),[],_,_):-
  spatial(Spatial),
  bfeature_path(Spatial,city,SpatialCity).

trans_LF(exceed,measure&Type,X,measure&Type,Y,exceeds(X,Y),[],_,_).





/* Adjectives */

restriction_LF(African,Spatial&_,X,ti(African,X)):- adj_lex(African,restr), spatial(Spatial).
%restriction_LF(american,Spatial&_,X,ti(american,X)).
%restriction_LF(asian,Spatial&_,X,ti(asian,X)).
%restriction_LF(european,Spatial&_,X,ti(european,X)).

attribute_LF(large,Spatial&_,X,measure&area,Y,measure_pred(Spatial,area,X,Y)).
attribute_LF(small,Spatial&_,X,measure&area,Y,measure_pred(Spatial,area,X,Y)).
attribute_LF(great,measure&Type,X,measure&Type,Y,exceeds(X,Y)).
attribute_LF(populous,Spatial&_,X,measure&population/*citizens*/,Y,count_pred(Spatial,population/*citizens*/,Y,X)).

aggr_adj(average,_,_,average).
aggr_adj(total,_,_,total).
aggr_adj(minimum,_,_,minimum).
aggr_adj(maximum,_,_,maximum).

/* Prepositions */

adjunction_LF(in,Spatial&_-X,Spatial&_-Y,trans_pred(Spatial,contain,Y,X)).
adjunction_LF(cp(East,Of),Spatial&_-X,Spatial&_-Y,ordering_pred(Spatial,cp(East,Of),X,Y)).
/*
adjunction_LF(cp(east,of),Spatial&_-X,Spatial&_-Y,ordering_pred(Spatial,cp(east,of),X,Y)).
adjunction_LF(cp(west,of),Spatial&_-X,Spatial&_-Y,ordering_pred(Spatial,cp(west,of),X,Y)).
adjunction_LF(cp(north,of),Spatial&_-X,Spatial&_-Y,ordering_pred(Spatial,cp(north,of),X,Y)).
adjunction_LF(cp(south,of),Spatial&_-X,Spatial&_-Y,ordering_pred(Spatial,cp(south,of),X,Y)).
*/

/* Measure */

measure_LF(ksqmile,measure&area,[],ksqmiles).
measure_LF(sqmile,measure&area,[],sqmiles).
measure_LF(degree,measure&position,[],degrees).
measure_LF(thousand,measure&population/*citizens*/,[],thousand).
measure_LF(million,measure&population/*citizens*/,[],million).

units(large,measure&_).
units(small,measure&_).

chat_sign(large,+).
chat_sign(small,-).
chat_sign(great,+).

/* Proportions and the like */

comparator_LF(proportion,_,V,[],proportion(V)).
comparator_LF(percentage,_,V,[],proportion(V)).
