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

:- op(400,xfy,&).


spatial(thing).
feat(Feat):- dif(Feat,geo). % debug_var(feat,Feat),

thing_LF(person,_,X,ti(person,X),[],_).

trans_LF12(Verb,TypeS,S,TypeD,D,Pred,Slots,SlotD,R):- 
 trans_LF(Verb,TypeS,S,TypeD,D,Pred,Slots,SlotD,R)
 *->true
 ;trans_LF1(Verb,TypeS,S,TypeD,D,Pred,Slots,SlotD,R).

trans_LF(contain,Spatial&_,X,Spatial&_,Y, trans_pred(Spatial,contain,X,Y),[],_,_).
trans_LF(have,Spatial&_,X,Spatial&_,Y, trans_pred(Spatial,have,X,Y),[],_,_).
trans_LF(have(MODAL),Spatial&_,X,Spatial&_,Y, trans_pred(Spatial,have,X,Y),[],_,_):- var(MODAL),!.
trans_LF(have(MODAL),Spatial&_,X,Spatial&_,Y, OUT, [],_,_):- append_term(MODAL,trans_pred(Spatial,have,X,Y),OUT).


thing_LF(OceanOrSea,Path,X,ti(OceanOrSea,X),Nil,Any):-  ti_subclass(OceanOrSea,Seamass), 
   like_type(_Geo,seamass,Seamass), %Seamass=seamass,
   thing_LF(Seamass,Path,X,ti(Seamass,X),Nil,Any),
    concrete_type(Seamass).

thing_LF(City,Spatial& Feat& City,X,ti(City,X),[],_):- concrete_type(City), feat(Feat), spatial(Spatial),like_type(geo,city,City).
thing_LF(Seamass,Spatial&Geo& /*_Neo&*/ Seamass,X,ti(Seamass,X),[],_):- concrete_type(Seamass), spatial(Spatial),like_type(Geo,seamass,Seamass).
thing_LF_access(Continent,Spatial&Geo& /*_Neo&*/ Continent,X,ti(Continent,X),[],_):- concrete_type(Continent), like_type(Geo,continent,Continent), spatial(Spatial).

name_template_lf0(X,Spatial& Feat& City) :-like_type(geo,city,City), ti(City,X),feat(Feat), spatial(Spatial).
name_template_lf0(X,Spatial& Feat& Circle_of_latitude) :- like_type(geo,circle_of_latitude,Circle_of_latitude), feat(Feat), ti(Circle_of_latitude,X), spatial(Spatial).

name_template_lf0(X,Spatial&Geo& /*_Neo&*/ Seamass) :- like_type(Geo,seamass,Seamass),spatial(Spatial), ti(Seamass,X).
name_template_lf0(X,Spatial&Geo& /*_Neo&*/ Country) :- like_type(Geo,country,Country), spatial(Spatial), ti(Country,X).
name_template_lf0(X,Spatial&Geo& /*_Neo&*/ Continent) :- like_type(Geo,continent,Continent), spatial(Spatial), ti(Continent,X).

%like_type(geo,River,River):- bind_pos('type',River).



thing_LF(River,Spatial& Feat& River,X,ti(River,X),[],_):- feat(Feat),like_type(geo,river,River),spatial(Spatial).
thing_LF(Type,Spatial& Feat & Type,X,ti(Type,X),[],_):- feat(Feat),bind_pos('type',Type),spatial(Spatial).
thing_LF(Types,Spatial& Feat & Type,X,ti(Type,X),[],_):- feat(Feat),bind_pos('type',Type,'s',Types),spatial(Spatial).
name_template_lf0(X,Spatial& Feat& River) :- feat(Feat),like_type(_Geo,river,River), ti(River,X), spatial(Spatial).
name_template_lf0(X,Spatial&_) :- like_type(_Geo,region,Region),spatial(Spatial), ti(Region,X).




thing_LF(Place,  Spatial&_,          X,ti(Place,X),  [],_):- spatial(Spatial), place_lex(Place).
thing_LF(Region, Spatial&_,          X,ti(Region,X), [],_):- spatial(Spatial),like_type(_Geo,region,Region).
thing_LF(Country,Spatial&Geo& /*_Neo&*/ Country,X,ti(Country,X),[],_):- spatial(Spatial),like_type(Geo,country,Country).


unique_of_obj(geo,thing,Country,Govern,Capital,City,Capital_city,Nation_Capital):-
   maplist(bind_pos, 
       ['type','action','subtype','type','property','property'],
       [Country,Govern,Capital,City,Capital_city,Nation_Capital]).


  

attribute_LF(populous,Spatial&_,X,value&units&population/*citizens*/,Y,count_pred(Spatial,population/*citizens*/,Y,X)).

attribute_LF(large,Spatial&_,X,value&size&Area,Y,measure_pred(Spatial,Area,X,Y)):- spatial(Spatial), type_measure_pred(_,size,Area,_).
attribute_LF(small,Spatial&_,X,value&size&Area,Y,measure_pred(Spatial,Area,X,Y)):- spatial(Spatial), type_measure_pred(_,size,Area,_).
attribute_LF(great,value&Measure&Type,X,value&Measure&Type,Y,exceeds(X,Y)).


/* Measure */

:- op(200,xfx,--).

unit_format(Population,X--million):- unit_format(Population,X--thousand).
unit_format(Population,_X--thousand):- type_measure_pred(_City,units,Population,countV).
unit_format(Latitude,_X--Degrees):- type_measure_pred(_Region, position(_),Latitude,Degrees).
%unit_format(Longitude,_X--Degrees):- measure_pred(_Region,position(x),Longitude,Degrees).
unit_format(Area,_X--Ksqmiles):- type_measure_pred(_Region,size,Area,Ksqmiles).

measure_LF(Unit,value&_&Size,[],Units):- type_measure_pred(_City,_,Size,Units), atom_concat(Unit,'s',Units).
measure_LF(sqmile,Path,[],sqmiles):- measure_LF(ksqmile,Path,[],ksqmiles).
measure_LF(sqmile,value&size&area,[],sqmiles).
measure_LF(Degree,value&position&Axis,[],Degrees):- type_measure_pred(_Region, position(Axis),_Latitude,Degrees), atom_concat(Degree,'s',Degrees).
measure_LF(thousand,value&units&Population/*citizens*/,[],thousand):-  type_measure_pred(_City,units,Population,countV).
measure_LF(million, value&units&Population/*citizens*/,[], million):-  type_measure_pred(_City,units,Population,countV).

verb_type_db(chat80,border,main+tv).
symmetric_verb(Spatial,border):- spatial(Spatial).


ordering_pred(thing,cp(east,of),X1,X2) :- type_measure_pred( _Region,position(x),Longitude,_), position_pred(thing,Longitude,X1,L1), position_pred(thing,Longitude,X2,L2), exceeds(L2,L1).
ordering_pred(thing,cp(north,of),X1,X2) :- type_measure_pred(_Region,position(y),Latitude,_ ), position_pred(thing,Latitude,X1,L1), position_pred(thing,Latitude,X2,L2), exceeds(L1,L2).
ordering_pred(thing,cp(south,of),X1,X2) :- type_measure_pred(_Region,position(y),Latitude,_ ), position_pred(thing,Latitude,X1,L1), position_pred(thing,Latitude,X2,L2), exceeds(L2,L1).
ordering_pred(thing,cp(west,of),X1,X2) :- type_measure_pred( _Region,position(x),Longitude,_), position_pred(thing,Longitude,X1,L1), position_pred(thing,Longitude,X2,L2), exceeds(L1,L2).


/* Nouns */
property_LF(River,Spatial& Feat& River,X,Spatial&Geo& /*_Neo&*/ Country,Y,
 (GP,ti(River,X)),[],_,_):-  
   if_search_expanded(2),
   make_generic_pred(Spatial,any,Y,X,GP),
   feat(Feat),spatial(Spatial),Geo=geo,
   concrete_type(River),
   concrete_type(Country).

concrete_type(Type):- Type==million,!,fail.
concrete_type(dog).
concrete_type(person).
concrete_type(man).
concrete_type(island).
concrete_type(country).
concrete_type(river).
concrete_type(TI):-ti(TI,_),!.

property_LF(Capital,Spatial& Feat& City,X,Spatial&Geo& /*_Neo&*/ Country,Y,specific_pred(Spatial,Nation_capital,Y,X),[],_,_):-  
%   fail,
   feat(Feat),
   unique_of_obj(Geo,Spatial,Country,_Govern,Capital,City,_Capital_city,Nation_capital).

trans_LF(    Govern,Spatial& Feat& City,X,Spatial&Geo& /*_Neo&*/ Country,Y,specific_pred(2,Spatial,Nation_capital,Y,X),[],_,_):-
  feat(Feat),
  unique_of_obj(Geo,Spatial,Country,Govern,_Capital,City,_Capital_city,Nation_capital).
   

thing_LF(Capital,Spatial& Feat& City,X,ti(Capital_city,X),[],_):- 
  feat(Feat),
   unique_of_obj(_Geo,Spatial,_Country,_Govern,Capital,City,Capital_city,_Nation_capital),
   spatial(Spatial).

/*
thing_LF(River,Spatial& Feat& River,X,ti(River,X),[],_):- 
  feat(Feat), concrete_type(River), spatial(Spatial).
*/
  
property_LF(Area,     value&size&Area,    X,Spatial&_,Y,  measure_pred(Spatial,Area,Y,X),[],_,_):- spatial(Spatial), type_measure_pred(_,size,Area,_).
property_LF(Latitude, value&position&XY,X,Spatial&_,Y, position_pred(Spatial,Latitude,Y,X),[],_,_):- type_measure_pred(_Region,position(XY),Latitude,_).
%property_LF(Longitude,value&position&x,X,Spatial&_,Y, position_pred(Spatial,Longitude,Y,X),[],_,_):- type_measure_pred(_Region,position(x),Longitude,_).
property_LF(Population, value&units&Population/*citizens*/, X,Spatial&_,Y,    count_pred(Spatial,Population/*citizens*/,Y,X),[],_,_):-
  type_measure_pred(_City,units,Population,countV).

property_LF(Area,     value&size&Area,    X,Spatial&_,Y, measure_pred(Spatial,Area,Y,X),[],_,_):- spatial(Spatial), clex_attribute(Area).

type_measure_pred(_AnyObjectType,MeasureType,Area,countV):- MeasureType\==size, MeasureType=Area, clex_attribute(Area).

clex_attribute(Area):-  bind_pos('attrib',Area).
%clex_attribute(Area):-  bind_pos('type',Area).

synonymous_spatial(nation,country).

thing_LF_access(Area,value&size&Area,X,unit_format(Area,X),[],_):- type_measure_pred(_,size,Area,_).

thing_LF_access(Latitude,value&position,X,unit_format(Latitude,X),[],_):- type_measure_pred(_Region,position(_Y),Latitude,_).

%thing_LF_access(Longitude,value&position,X,unit_format(Longitude,X),[],_):- type_measure_pred(_Region,position(x),Longitude,_).
thing_LF_access(Population,value&units&Population/*citizens*/,X,unit_format(Population,X),[],_):- type_measure_pred(_,units,Population,_).


/* Prepositions */

adjunction_LF(in,Spatial&_-X,Spatial&_-Y,trans_pred(Spatial,contain,Y,X)).
adjunction_LF(Any,Spatial&_-X,Spatial&_-Y,GP):- if_search_expanded(2),make_generic_pred(Spatial,matches_prep(Any),Y,X,GP).
adjunction_LF(cp(East,Of),Spatial&_-X,Spatial&_-Y,ordering_pred(Spatial,cp(East,Of),X,Y)).


/* Proper nouns */

name_template_LF(X,Type2):- bind_pos('object',X), type_conversion(typeOfFn(X),Type2).
name_template_LF(X,Type2):- bind_pos('type',X), type_conversion(X,Type2).
name_template_LF(X,Type2):- name_template_lf0(X,Type1), type_conversion(Type1,Type2).


aggr_noun_LF(average,_,_,average).
aggr_noun_LF(sum,_,_,total).
aggr_noun_LF(total,_,_,total).

meta_noun_LF(number,of,_,V,Spatial&_,X,P,numberof(X,P,V)):- spatial(Spatial).


% thing_LF(geo,Spatial&_,X,ti(geo,X),[],_):- spatial(Spatial).

thing_LF(Nation,Path,X,LF,Slots,Other):- synonymous_spatial(Nation,Country), thing_LF(Country,Path,X,LF,Slots,Other).


thing_LF_access(Noun,Type2,X,P,Slots,_):-
  thing_LF(Noun,Type1,X,P,Slots,_),
  btype_conversion(Type1,Type2).
thing_LF_access(Person,_,X,ti(Person,X),[],_):-  if_search_expanded(4), concrete_type(Person).

btype_conversion(_,_).
type_conversion(Type1,Type2):- !, Type1=Type2.

check_slots(Slots):- ignore((var(Slots),trace,break)).

/* Verbs */

/*
%verb_root_db(chat80,border).
trans_LF(border,Spatial&Geo& /*_Neo&*/ _,X,Spatial&Geo& /*_Neo&*/ _,Y,symmetric_pred(Spatial,borders,X,Y),[],_,_).
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

trans_LF(Border,Spatial&Super&_,X,Spatial&Super&_,Y,GP,[],_,_):-  make_generic_pred(Spatial,Border,X,Y,GP),
   verb_type_lex(Border,main+tv),
   symmetric_verb(Spatial, Border).

trans_LF(Border,Spatial&Super&_,X,Spatial&Super&_,Y,GP,[],_,_):-  
   (bind_pos('action',Border);bind_pos('attrib',Border)),nop(spatial(Spatial)),
   make_generic_pred(Spatial,Border,X,Y,GP).


bind_pos(Type,Var,Lex,Var2):- nonvar(Var),!,clex:learned_as_type(Type,Var,Lex,Var2).
bind_pos(Type,Var,Lex,Var2):- freeze80(Var2,clex:learned_as_type(Type,Var,Lex,Var2)).
bind_pos(Type,Var):- freeze80(Var,clex:learned_as_type(Type,Var)).

freeze80(Var,Goal):- (nonvar(Var);(term_variables(Goal, Vars),Vars==[],trace)),!,call(Goal).
freeze80(Var,Goal):- freeze(Var,freeze80(Var,Goal)).

verb_form_db(clex,Border,Border,A,B):- verb_form_db_clex(Border,Border,A,B).

verb_form_db_clex(Bordering,Border,pres+part,_):- (bind_pos('action',Border,'ing',Bordering)).
verb_form_db_clex(Bordered,Border,past+part,_):- (bind_pos('action',Border,'ed',Bordered)).
verb_form_db_clex(Borders,Border,pres+fin,_):- (bind_pos('action',Border,'s',Borders)).
verb_form_db_clex(Border,Border,inf,_):- (bind_pos('action',Border,'',Border)).

talkdb_talk_db(transitive,   Border,  Borders,  Bordered,  Bordering,  Bordered):-
  (bind_pos('action',Border,'ing',Bordering), bind_pos('action',Border,'ed',Bordered), bind_pos('action',Border,'s',Borders)).
 
%use_lexicon_80(_):- !, true.
%us e_lexicon_80(cha t80).
%use _lexicon_80(chat 80_extra).
%use_lexicon_80(talkd b_verb(X)):- verb _type_db(chat80,X,_).
% use_lexicon_80(_):- fail.

subsumed_by(X,X).
named(X,X).

:- import(talkdb:talk_db/6).
%                         nonfinite,  pres+fin, past+fin,  pres+part    past+part,
talkdb_talk_db(transitive,   border,  borders,  bordered,  bordering,  bordered).
talkdb_talk_db(  Transitive, Write,   Writes,   Wrote,     Writing,    Written):- 
  talkdb:talk_db(Transitive, Write,   Writes,   Wrote,     Writing,    Written).

%verb_root_lex(Write):-            talkdb_talk_db(_Transitive,Write,_Writes,_Wrote,_Writing,_Written).
verb_type_db(talkdb,Write,main+tv):-   talkdb_talk_db( transitive,Write,_Writes,_Wrote,_Writing,_Written), \+ avoided_verb(Write).
verb_type_db(talkdb,Write,main+iv):-    
   talkdb_talk_db( intransitive,Write,_Writes,_Wrote,_Writing,_Written), 
 % \+ talkdb_talk_db( transitive,Write,_Writes2,_Wrote2,_Writing2,_Written2),
  \+ avoided_verb(Write).
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
clex_verb80(Looked,Look,VerbType,Form):- \+ compound(Looked),\+ compound(Look), 
  clex_iface:clex_verb(Looked,Look,VerbType,Form).
%% superceeded regular_pres_lex(Look):- no_loop_check(verb_root_lex(Look)).
%verb_form_lex(Looking,Look,pres+part,_):- (atom(Looking)->atom_concat(Look,'ing',Looking);var(Looking)),
%  no_loop_check(verb_root_lex(Look)),atom(Look),atom_concat(Look,'ing',Looking).
% NEW TRY verb_root_lex(Look):- clex_verb80(_Formed,Look,_Iv,_Finsg).
% regular_past_lex(Looked,Look):- clex_verb80(Looked,Look,_Iv,prep_phrase).
verb_form_db(clex,Looks,Look,pres+fin,3+sg):- clex_verb80(Looks,Look,_,finsg).
verb_form_db(clex,LookPL,Look,pres+fin,3+pl):-  clex_verb80(LookPL,Look,_,infpl).
verb_type_db(clex,Look,main+ITDV):- clex_verb80(_Formed,Look,ITDV,_Finsg).

intrans_LF(Assign,feature&_,X,dbase_t(Assign,X,Y), [slot(prep(To),feature&_,Y,_,free)],_):-
  clex_verb80(_Assigned, Assign, dv(To),_).


%trans_LF(Look,feature&_,X,dbase_t(Look,X,Y), [slot(prep(At),feature&_,Y,_,free)],_):- (tv_infpl(S,S);tv_finsg(S,S)), atomic_list_concat([Look,At],'-',S).

trans_LF(exceed,value&Measure&Type,X,value&Measure&Type,Y,exceeds(X,Y),[],_,_).
trans_LF1(Trans,_,X,_,Y,P ,[],_,_):- if_search_expanded(4),
  make_generic_pred(Spatial,Trans,X,Y,P),spatial(Spatial).

make_generic_pred(Spatial,matches_prep(AT),X,Y,generic_pred(Spatial,prep(AT),Y,X)):-!.
make_generic_pred(Spatial,(AT),X,Y,generic_pred(Spatial,(AT),X,Y)):-!.

% qualifiedBy
qualifiedBy_LF(_FType, X,Base&Thing,np_head(wh_det(Kind,Kind-_23246),[],Type),(ti(Thing,X),ti(Base,X),ti(Type,X))).
qualifiedBy_LF(FType, X, BaseAndThing,np_head(det(the(sg)),Adjs,Table),Head):- qualifiedBy_LF(FType, X, BaseAndThing,np_head(det(a),Adjs,Table),Head),!.
qualifiedBy_LF(_FType, X,_,np_head(det(a),[],Table),ti(Table,X)). 
qualifiedBy_LF(_FType,Name,_Type,pronoun(_,1+sg),isa(Name,vTheVarFn("I"))).
qualifiedBy_LF(_FType,Name,_Type,pronoun(_,1+pl),isa(Name,vTheVarFn("US"))).
qualifiedBy_LF(FType,Name,Type,Else,P):- nop(qualifiedBy_LF(FType,Name,Type,Else,P)),fail.

qualifiedBy_LF(_FType, X,Type,np_head(det(a),Adjs,Table),Pred):- %fail,
  must(i_adjs(Adjs,Type-X,Type-X,_,Head,Head,Pred,ti(Table,X))).


/* Adjectives */

restriction_LF(Word,Spatial&_,X,ti_adj(Type,X)):- adj_db_clex(Type,Word,restr), spatial(Spatial).
restriction_LF(African,Spatial&_,X,ti(African,X)):- adj_lex(African,restr), spatial(Spatial).
restriction_LF(Word,_,X,Out):- compound(Word),subst(Word,self,X,Out), Word\==Out.



%restriction_LF(american,Spatial&_,X,ti(american,X)).
%restriction_LF(asian,Spatial&_,X,ti(asian,X)).
%restriction_LF(european,Spatial&_,X,ti(european,X)).

aggr_adj_LF(average,_,_,average).
aggr_adj_LF(total,_,_,total).
aggr_adj_LF(minimum,_,_,minimum).
aggr_adj_LF(maximum,_,_,maximum).

/* Measure */

units_db(large,_Measure&_).
units_db(small,value&_&_).

adj_sign_LF(large,+).
adj_sign_LF(small,-).
adj_sign_LF(great,+).

/* Proportions and the like */

comparator_LF(proportion,_,V,[],proportion(V)).
comparator_LF(percentage,_,V,[],proportion(V)).


measure_pred(Spatial,Area,Where,Total) :- not_where(Where), 
 % ti(continent,Where),
 setOf(Value:[Country],
               []^(database80(measure_pred(Spatial, Area, Country, Value)), 
               %database80(ti(country, Country)), 
               database80(trans_pred(Spatial,contain,Where,Country))),
               Setof),
         database80(aggregate80(total, Setof, Total)).



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
  symmetric_pred(thing,Pred,NewType,X),
  % dont find instances already of the super type
  \+ ti(SuperType,X).

ti(SC,X) :- ti_subclass(C,SC),ti(C,X).



% if X is contained in africa then X is african.
ti(An,X) :- agentitive_trans(Contains,Af,An), (trans_pred(thing,Contains,Af,X);Af=X).

agentitive_trans(Contains,Af,An):- agentitive_trans_80(Contains,Af,An).



% NLU Logical Forms
% ------------------

% X rises in Begin
% % chat80("where does the rhine rise?") -> [switzerland]
% chat80("the rhine rises in switzerland ?      ").
% @TODO: X begins from Begin
intrans_LF(Start,Spatial & Feat& Type,X, LF,
   [slot(prep(From),Spatial&_,Begin,_,free)],_):- 
 feat(Feat),
 type_begins_thru_ends(Type, PathSystem, Start, _Continue, _Stop),
 spatial(Spatial),member(From,[in,from,at]),debug_var(From,Begin),
 LF = path_pred(begins(PathSystem),Type,X,Begin).

% X drains into End
intrans_LF(Stop,Spatial & Feat& Type,X, LF, 
   [slot(prep(Into),Spatial&_,End,_,free)],_):- 
 feat(Feat),
 type_begins_thru_ends(Type, PathSystem, _Start, _Continue, Stop),
 spatial(Spatial),member(Into,[into,in,to,at]),debug_var(Into,End),
 LF = path_pred(ends(PathSystem),Type,X,End).


intrans_LF(Continue,Spatial & Feat& Type,X,LF,
   [slot(prep(Into),Spatial&_,Dest,_,free),
    slot(prep(From),Spatial&_,Origin,_,free)],_):- 
 feat(Feat),
 type_begins_thru_ends(Type, PathSystem, _Start, Continue, _Stop),
 spatial(Spatial),
 member(Into,[into,to,through,in,at]),
 member(From,[from,through,in,at]), 
 dif(From,Into),
 debug_var(Into,Dest),
 debug_var(From,Origin),
 LF = path_pred_linkage(direct(PathSystem),Type,X,Origin,Dest).


intrans_LF(Run,Spatial & Feat& Type,X,LF, [],_):- 
 feat(Feat),
 intrans_verb(Run),
 spatial(Spatial),
 LF = intrans_pred(Spatial,Type,Run,X).

intrans_LF(Run,Spatial & Feat& Type,X,LF,
 [slot(prep(Into),Spatial&_,Dest,_,free)],_):- 
 feat(Feat),
 %\+ concrete_type(Run),
 intrans_verb(Run),
 spatial(Spatial),
 LF = intrans_pred_prep(Spatial,Type,Run,X,Into,Dest).
intrans_LF(Continue,Spatial& _Feat& Type,X,LF,
  [slot(prep(dirO(ArgInfo)),Spatial&_,Y,_,free)],_):- 
  if_search_expanded(2),
  intrans_verb(Continue),
  LF = intrans_pred_direct(_Spatial,Continue,Type,X,dirO(ArgInfo),Y).

intrans_LF(Run,Spatial & Feat& Type,X,LF, Slots,_):- 
 feat(Feat), fail,
 %\+ concrete_type(Run),
 intrans_verb(Run),
 if_search_expanded(4),
 spatial(Spatial),
 LF = intrans_pred_slots(Spatial,Type,Run,X,Slots).


intrans_verb(Run):- clex:iv_infpl(Run,_).
intrans_verb(run).
intrans_verb(wait).
intrans_verb(Y):- clex:iv_finsg(_,Y).


% X flows through Begin
/*
intrans_LF(Continue,Spatial & Feat& Type,X,LF,
   [slot(prep(Through),Spatial&_,Link,_,free)],_):- 
 feat(Feat),
 type_begins_thru_ends(Type, PathSystem, _Start, Continue, _Stop),
 spatial(Spatial),member(Through,[through,in]),
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
verb_type_db(chat80,LessSpecific,main+_):-  (bind_pos('action',LessSpecific)).
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
%:- add_ss(thing,border,Albania,Greece).

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


% @TODO Dmiles maybe not cache these?
trans_rel_cache_create(P1,P2):- Both = (P1,P2), \+ ground(Both),numbervars(Both),!,trans_rel_cache_create0(P1,P2).
trans_rel_cache_create(P1,P2):- trans_rel_cache_create0(P1,P2).

trans_rel_cache_create0(P1,P2):- must_be(ground,(P1,P2)),
                                tmp80:trans_rel_cache_created(P1,P2),!.
trans_rel_cache_create0(P1,P2):- tmp80:trans_rel_cache_creating(P1,P2),dmsg(looped(trans_rel_cache_create(P1,P2))),fail.
trans_rel_cache_create0(P1,P2):-
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


%contain(X,Y) :- trans_direct(thing,contain,X,Y).
%contain(X,Y) :- trans_direct(thing,contain,X,W), contain(W,Y).


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
database801(generic_pred(Type,P,X,Y)) :- generic_pred(Type,P,X,Y). % capital 
database801(trans_pred(Type,P,X,Y)) :- trans_pred(Type,P,X,Y). % contain 

database801(should(X)):- database80(X).
database801(can(X)):- database80(X).


%database80(path_pred(begins(Flow),rises,river,X,Y)) :- path_pred(begins(Flow),rises,river,X,Y).
%database80(path_pred(ends(Flow),drains,river,X,Y)) :- path_pred(ends(Flow),drains,river,X,Y).
database801(path_pred(PathSystemPart,ObjType,X,Y)) :- path_pred(PathSystemPart,ObjType,X,Y).
database801(path_pred_linkage(DirectPathSystem,ObjType,X,Y,Z)) :- path_pred_linkage(DirectPathSystem,ObjType,X,Y,Z).

database80((A,B)):- nonvar(A),!,database80(A),database80(B).
database80(G):-  clause(database801(G),B), !, call(B).
database80(G):-  current_predicate(_,G), call(G).

trans_pred_type(Type,P):- tmp80:trans_rel_cache_created(=, trans_direct(Type,P)).
trans_pred_type(thing,contain).
generic_pred(Type,P,X,Y) :- P == any,!, generic_pred0(Type,_,X,Y).
generic_pred(Type,P,X,Y) :- generic_pred0(Type,P,X,Y)*->true;generic_pred1(Type,P,X,Y).

generic_pred0(Type,P,X,Y) :- trans_pred_type(Type,P), ground(Type+P),trans_pred(Type,P,X,Y). % contain 
generic_pred0(Type,P,X,Y) :- measure_pred(Type,P,X,Y). % area of
generic_pred0(Type,P,X,Y) :- count_pred(Type,P,X,Y). % population of 
generic_pred0(Type,P,X,Y) :- position_pred(Type,P,X,Y). % latitude of
generic_pred0(Type,P,X,Y) :- ordering_pred(Type,P,X,Y). % south of
generic_pred0(Type,P,X,Y) :- symmetric_pred(Type,P,X,Y). % border
generic_pred0(Type,P,X,Y) :- specific_pred(Type,P,X,Y). % capital 

generic_pred1(Type,P,X,Y) :- var(Type), nop(generic_pred1(Type,P,X,Y)).

:- style_check(+singleton).

remove_each_eq(Ys,[],Ys).
remove_each_eq(Ys,[X|Xs],Es):- exclude(==(X),Ys,Zs),remove_each_eq(Zs,Xs,Es).

setOf1(X,Y,Z):- term_variables(Y,Ys),term_variables(X,Xs),remove_each_eq(Ys,Xs,Es),!,(setof(X,Es^Y,Z)*->true;Z=[]).
setOf(X,Y,Z):- setof(X,Y,Z).

%exceeds(X--U,Y--U) :- !, X > Y.
%exceeds(X1--U1,X2--U2) :- ratio(U1,U2,M1,M2), X1*M1 > X2*M2.
exceeds(X,Y):- (var(Y)),!,X=Y.
exceeds(X,Y):- term_variables(X-Y,Vars),freeze_until(Vars,exceeds0(X,Y)),!.

freeze_until([],Goal):-!, term_variables(Goal, Vars),(Vars==[] -> call(Goal) ; freeze_until(Vars,Goal)).
freeze_until([V|Vars],Goal):- freeze(V,freeze_until(Vars,Goal)),!.

exceeds0('--'(X,U),'--'(Y,U)) :- !, X > Y.
exceeds0('--'(X1,U1),'--'(X2,U2)) :- once((ratio(U1,U2,M1,M2), X1*M1 > X2*M2)).

:- fixup_exports.
