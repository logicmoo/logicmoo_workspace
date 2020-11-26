% :- if(( ( \+ ((current_prolog_flag(logicmoo_include,Call),Call))) )). 
% :- swi_module(mud_simple_decl_parser, [parserVars/3,parserVars/4,asserta_parserVars/3]).
/* * <module> simple_decl_parser - an example of simple parsing of an inform7 like languages.
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
% :- endif.

:- check_clause_counts.
:- include(prologmud(mud_header)).
:- use_module(library(pfc)).
:- check_clause_counts.

:-discontiguous((translation_spo/6,parserTest/2,parserTest/3,translation_w//
                                                                              1)).
:-dynamic((translation_spo/6,parserTest/2,parserTest/3,translation_w//
                                                                         1)).
:-thread_local(loosePass/0).
:-thread_local(debugPass/0).
:-dynamic(parserVars/4).


glue_words(W):- member(W,[is,a,the,in,carries,'An','A','The',was,of,type]).

toCamelAtom(List,O):-((\+((member(IS,List),glue_words(IS))),toCamelAtom00(List,O))),!.

% toCamelAtom00(I,O):-toCamelAtom0(I,O).
toCamelAtom00(I,O):-toCamelcase(I,O).

 % :- set_prolog_flag(subclause_expansion,true).


vtColor(vRed).

ttValueType(vtColor).

'==>'((isa(X,ttValueType)/(X\==vtValue)),
  (genls(X,vtValue),completelyAssertedCollection(X))).

completelyAssertedCollection(vtValue).

isa(vtValue,ttValueType).


typeGenls(ttValueType,vtValue).


:-must(vtColor(vRed)).
:-must((isa(vRed,REDISA),genls(REDISA,vtValue))).


 % :- set_prolog_flag(subclause_expansion,false).

:- baseKB:ensure_loaded(library(multimodal_dcg)).


asserta_parserVars(N,V,Type):- show_failure(current_agent(A)),asserta(parserVars(A,N,V,Type)).
:-export(parserVars/3).
parserVars(N,V,Type):- show_failure(current_agent(A);A=iCurrentAgentFn),
   (parserVars_local(A,N,V,Type)*->true;parserVars_falback(global,N,V,Type)).

parserVars_local(A,(N1;N2),V,Type):-!,parserVars_local(A,N1,V,Type);parserVars_local(A,N2,V,Type).
parserVars_local(A,N,V,Type):-parserVars(A,N,V,Type).

parserVars_falback(_,N,V,Type):-parserVars_local(global,N,V,Type).

toCol(Txt,I,TCOL):-member(TCOL,[tCol,tObj,tSpatialThing,vtValue,ttTypeType]),show_success(toCol_0,toCol_0(Txt,I,TCOL)),!.

toCol_0(Txt,O,TCOL):-member(Pfx-Sfx- _ISACISA, 
         [
          ''-''-_,
          't'-''-'tCol',
          'tt'-'Type'-'ttTypeType',
          'vt'-''-'ttValueTypeType',
          'v'-''-'vtValue',
          'i'-'7'-'tSpatialThing',
          'i'-'7'-'ttSpatialType',
          't'-'Able'-'ttTypeByAction',
          'tt'-''-'ttTypeType',
          ''-''-_
           ]),atom_concat(Pfx,Txt,I),atom_concat(I,Sfx,O),isa(O,TCOL),!.

is_a --> is_was, [a].
is_a --> is_was.

is_was --> [is].
is_was --> [was].
is_was --> [be].
is_was --> [are].

is_in --> is_was, [in].
is_in --> is_was, [inside,of].
is_in --> is_was, [carried,by].

is_type_of --> is_a, [type,of].
is_type_of --> is_a, [type].

detn(exists) --> [the].
detn(exists) --> ['The'].
detn(exists) --> [some].
detn(all) --> [all].
detn(indef) --> [a].
detn(indef) --> ['A'].
detn(indef) --> [an].
detn(indef) --> ['An'].

collection(I,Col,More)--> detn(_),!,collection(I,Col,More).
collection(I,Col,true)--> collection0(I,Col).
collection(I,Col,More)--> attribute(_Pred,I,_Value,More),collection0(I,Col).
collection(I,Col,More)--> attribute(_Pred,I,_Value,More),{call_u(isa(I,Col))}.


collection0(I,Col)--> [A,B,C],{toCamelAtom([A,B,C],O),collection00(O,I,Col)}.
collection0(I,Col)--> [A,B],{toCamelAtom([A,B],O),collection00(O,I,Col)}.
collection0(I,Col)--> [O],{collection00(O,I,Col)}.

collection00(A,I,Col):-mudKeyword(I,W),string_equal_ci(A,W),toCol(A,I,Col).
collection00(M,I,Col):-toCol(M,I,Col).
collection00(A,I,Col):-toPropercase(A,O),toCol(O,I,Col).

subject(I,More)-->subject(I,_,More).
subject(I,T,true)-->(['This'];['this']),!,{must((parserVars(isThis,I,T);parserVars(_,I,T)))}.
subject(I,T,true)--> [IT],{string_equal_ci(IT,ITLC),parserVars(isParserVar(ITLC),I,T)},!.
subject(I,T,true)--> [IT],{string_equal_ci(IT,ITLC),parserVars((ITLC),I,T)},!.
subject(I,T,More)--> dcgOptional(detn(_)),collection(I,T,More),{(asserta_parserVars(isThis,I,T))}.

object(I,More)-->object(I,_,More).
object(I,T,true)-->([it];['It'];['This'];['this']),!,{must((parserVars(object,I,T);parserVars(_,I,T)))}.
object(I,T,More)--> detn(_),!,collection(I,T,More),{(asserta_parserVars(object,I,T))}.
object(I,T,More)--> collection(I,T,More),{(asserta_parserVars(object,I,T))}.

% big , red , flat, etc
attribute(Pred,I,C,t(Pred,I,C))--> [W],{ \+ glue_words(W),collection00(W,C,vtValue), isa(C,What),\=(What,vtValue),isa(What,ttValueType),argIsa(Pred,2,What)}.


dcgParse213(A1,A2,A3,S,E):-append([L|Left],[MidT|RightT],S),phrase(A2,[MidT|RightT],EE),     ((phrase(A1,[L|Left],[]),phrase(A3,EE,E))).
dcgParse213(A1,A2,A3,S,E):-debugPass,append([L|Left],[MidT|RightT],S),phrase(A2,[MidT|RightT],EE), trace, must((phrase(A1,[L|Left],[]),phrase(A3,EE,E))).

p_predicate(Pred,_Arg1Isa,_Arg2Isa)-->predicate0(Pred),{current_predicate(Pred/_),!}.
p_predicate(Pred,_Arg1Isa,_Arg2Isa)-->{loosePass},predicate0(Pred).


predicate0(mudStowing)-->[carries].
predicate0(mudWielding)-->[wields].
predicate0(mudLikes)-->[likes].
predicate0(mudColor)-->[is,colored].
predicate0(localityOfObject)-->is_in.
predicate0(Pred)-->[has,Color],{i_name(mud,Color,Pred)}.
predicate0(isa)-->is_type_of.
predicate0(Pred)-->[is,the,Color],{i_name(mud,Color,Pred)}.
predicate0(Pred)-->[Likes],{atom_concat(Like,'s',Likes),i_name(mud,Like,Pred)}.
predicate0(Pred)-->[is,Colored],{atom_concat(Color,'ed',Colored),i_name(mud,Color,Pred)}.
predicate0(isa)-->is_a.
predicate0(mudRelates)-->is_was.
predicate0(isa)-->[is].

 % :- set_prolog_flag(subclause_expansion,true).


tCol('tRoom').

% :-ignore(show_call(phrase(collection(I,T,More),[red,room]))).

%TODO "All couches are things."

% assert_text(iWorld7,"couches are hard sometimes").

parserTest(A,B):-parserTest(A,B,_).

:-assertz_if_new(parserTest(iWorld7,"A television is usually in the living room.")).

% :-assert_text_now(iWorld7,"You are in a well kept garden.").


translation_spo(Prolog,localityOfObject,I,C) --> dcgParse213(subject(I,More1),is_in,object(C,More2)),{conjoin(More1,More2,Prolog)}.


% :-assertz_if_new(parserTest(iKitchen7,"This is the red room.")).

:-assertz_if_new(parserTest(iWorld7,"The player carries the sack.")).
translation_spo(Prolog,Pred,I,C) --> dcgParse213(subject(I,Arg1Isa,More1),p_predicate(Pred,Arg1Isa,Arg2Isa),object(C,Arg2Isa,More2)),{conjoin(More1,More2,Prolog)}.

:-assertz_if_new(parserTest(iWorld7,"room is type of tRegion")).
translation_spo(Prolog,isa,I,C) --> dcgParse213(subject(I,tCol,More1),is_type_of,object(C,tCol,More2)),{conjoin(More1,More2,Prolog)}.

:-assertz_if_new(parserTest(iWorld7,"The Living room is a room.")).
tCol('tSack').

:-assertz_if_new(parserTest(iWorld7,"The sack is a container.")).
translation_spo(Prolog,isa,I,C) --> dcgParse213(subject(I,More1),is_a,object(C,tCol,More2)),{conjoin(More1,More2,Prolog)}.


:-assert_if_new(vtSize('vBulky')).

translation_spo(Prolog,isa,I,C) --> dcgParse213(subject(I,More1),is_was,object(C,_,More2)),{conjoin(More1,More2,Prolog)}.
translation_spo(Prolog,Pred,I,C) --> dcgParse213(subject(I,More1),is_was,attribute(Pred,I,C,More2)),{conjoin(More1,More2,Prolog)}.

%:-assertz_if_new(parserTest(iWorld7,"A coffee table is in the living room.")).
%:-assertz_if_new(parserTest(iWorld7,"It is bulky.")).

tCol('tRemoteControl').
:-assertz_if_new(parserTest(iWorld7,"A remote control is in the living room.")).
:-assertz_if_new(parserTest(iWorld7,"A tv guide is a type of item.")).

tCol('tTvGuide').
:-assertz_if_new(parserTest(iWorld7,"A tv guide is in the living room.")).

%:-assertz_if_new(parserTest(iWorld7,"The paper clip is on the coffee table.")).

:-assertz_if_new(parserTest(iWorld7,"A tv guide is a type of book.")).

toplevel_type(InstISA):-member(InstISA,[tWorld,tRegion,tAgent,tItem,tObj,ftSpec,tCol,ftTerm]).
% toplevel_type(InstISA):-ftSpec(InstISA).


get_ctx_isa(InstISA,Inst,InstISA):- toplevel_type(InstISA),must((isa(Inst,InstISA))),!.
get_ctx_isa(Inst,Inst,InstISA):- must(show_call(once(((toplevel_type(InstISA),isa(Inst,InstISA)))))),!.

system:assert_text(InstIn,String):- cwc, get_ctx_isa(InstIn,Inst,InstISA),!,assert_text(Inst,InstISA,String).

assert_text(Inst,InstISA,String):-  cwc, 
       % context changed   and not the tWorld?                
          % v this is for when there was no prior context
  ((parserVars(context,Inst0,_) -> (((Inst0 \==Inst),InstISA\==tWorld) 
   -> (asserta_parserVars(isThis,Inst,InstISA)); true) ; (asserta_parserVars(isThis,Inst,InstISA))), 
    locally(parserVars(context,Inst,InstISA),assert_text_now(Inst,InstISA,String))).

assert_text_now(Inst,InstISA,String):-   
 on_f_log_ignore(( 
  % parse the string to attributed text
 to_word_list(String,WL),!,to_icase_strs(WL,IC),!,   
   ((phrase(translation_dbg_on_fail(Inst,InstISA,PrologO),IC),
   ain(asserted_text(Inst,String,PrologO)),     
     ain(onSpawn(PrologO)))))).

:- kb_shared(asserted_text/3).

tCol(describedTyped).
describedTyped(tRegion).
describedTyped(tObj).
(describedTyped(Col),isa(Inst,Col),mudDescription(Inst,String)/ 
  ( \+asserted_text(Inst,String,_), \+assert_text(Inst,String))) ==> mudDescriptionHarder(Inst,String).

:- export(to_icase_strs/2).
to_icase_strs(WL,IC):-maplist(to_icase_str,WL,IC).


:- export(to_icase_str/2).
% to_icase_str(SL,IC):-string_to_atom(SL,SA),string_to_atom(SS,SA),when(?=(IC,Y),(trace,(Y=SA;Y=SS))).
to_icase_str(SL,IC):-string_to_atom(SL,SA),string_to_atom(SS,SA),when(nonvar(IC);?=(IC,IC),(IC=SA;IC=SS)).


% somethingCanBe(tFountainDrink,[vSmall,vMedium,vLarge]).

translation_for(Room,'tRegion',(isa(Room,'tCorridor'),isa(Room,'tWellLit')),WS,[]):-concat_atom(WS,' ',O),if_defined(tag_pos(O,IO),fail),IO = 
 ('S'('NP'('PRP'('You')),'VP'('VBP'(find),'NP'('PRP'(yourself)),'PP'('IN'(in),'NP'('NP'('DT'(the),'NN'(middle)),'PP'('IN'(of),
  'NP'('NP'('DT'(a),'ADJP'('RB'(well),'JJ'(lit)),'NN'(corridor)),'PP'('IN'(on),'NP'('DT'(the),'NN'('Enterprise')))))))))).

translation_for(_Inst,_InstISA,t(M,Prolog),WS,WE):- once((append(LeftSide,RightSide,WS), modality(M,List,Replace),append(LeftL,List,LeftSide),
  append(LeftL,List,LeftSide),append(LeftL,Replace,Left),
   append(Left,RightSide,NewWS))),
   translation_w(Prolog,NewWS,WE),!.
translation_for(_Inst,_InstISA,Prolog) --> translation_w(Prolog).
translation_for(_Inst,_InstISA,Prolog,WS,WE):-locally(loosePass,translation_w(Prolog,WS,WE)).


translation_dbg_on_fail(Inst,InstISA,Prolog)-->translation_for(Inst,InstISA,Prolog),!.
translation_dbg_on_fail(Inst,InstISA,Prolog,WS,WE):-locally(debugPass,translation_for(Inst,InstISA,Prolog,WS,WE)).

%:-assertz_if_new(parserTest(iWorld7,"Buffy the Labrador retriever is lounging here, shedding hair all over the place.")).
%:-assertz_if_new(parserTest(iWorld7,"You can also see a sugar candy doll house here.")).

mudKeyword(tItem,"thing").
mudKeyword(isSelfRegion,"here").
mudKeyword(tThing,"object").

==>
 type_action_info(tHumanControlled,
   actAddText(isOptional(tTemporalThing,isThis),ftText),
     "Development add some Text to a room.  Usage: addtext a sofa is in here").


a_command(Agent,actAddText(What,StringM)):- ground(What:StringM),
 locally(parserVars(isThis,What,ftTerm),
   locally(parserVars(isSelfAgent,Agent,tAgent),   
       must(assert_text(What,StringM)))).


translation_w(t(M,Prolog),WS,WE):- once((append(LeftSide,RightSide,WS), modality(M,List,Replace),append(LeftL,List,LeftSide),append(LeftL,Replace,Left),
   append(Left,RightSide,NewWS))),translation_w(Prolog,NewWS,WE),!.
translation_w(Prolog) --> translation_spo(More2,P,S,O),!,{conjoin(More2,t(P,S,O),Prolog)}.

:-assertz_if_new(parserTest(iWorld7,"An emitter has a truth state called action keeping silent.",
   relationAllExists(mudActionKeepingSilient,tEmitter,ftBool))).

translation_w(relationAllExists(mudActionKeepingSilient,tEmitter,ftBoolean))
  --> ['An',emitter,has,a,truth,state,called,action,keeping,silent].

:-assertz_if_new(parserTest(iWorld7,"An object has a text called printed name.")).
translation_w(relationAllExists(P,C,DT))  
  --> collection(C),[has,a],datatype(DT),[called],predicate_named(P).

collection(C)-->subject(C,tCol,true).
datatype(ftBoolean)--> dcgOptional(detn(_)),[truth,state].
datatype(ftText)--> dcgOptional(detn(_)),[text].
datatype(ftTerm)--> dcgOptional(detn(_)),[value].

predicate_named(Pred) --> dcgAnd(theText(Text),dcgLenBetween(1,5)),
  {toCamelAtom(Text,O),i_name(mud,O,Pred),ignore(assumed_isa(Pred,tPred))}.

:- dmsg(call(listing(predicate_named//1
              ))).

assumed_isa(I,C):-isa(I,C),!.
assumed_isa(I,C):-loosePass,assert_isa(I,C),!.

:- call(must(dcgAnd(dcgLenBetween(5,1),theText(_Text),[a,b,c],[]))).
:- must_or_rtrace(call(must(predicate_named(_P,[proper,-,named],[])))).


:-assertz_if_new(parserTest(iWorld7,"An object can be proper-named or improper-named.",partitionedInto(tObj,tProperNamed,tImproperNamed))).
translation_w(partitionedInto(C1,C2,C3)) --> collection(C1),[be],collection(C2),[or],collection(C3).


:-assertz_if_new(parserTest(iWorld7,"An object is usually improper-named.",relationMostInstance(isa,tObj,tImproperNamed))).
translation_w(relationMostInstance(isa,C1,C2)) --> collection(C1),[is,usually],collection(C2).  

:-assertz_if_new(parserTest(iWorld7,"A thing can be scenery.", relationSomeInstance(isa,tItem,tScenery))).
translation_w(relationSomeInstance(isa,C1,C2)) --> collection(C1),[be],collection(C2).  

:-assertz_if_new(parserTest(iWorld7,"The outside is a direction.", t(isa,vOutside,vtDirection))).
translation_w(isa(C1,C2)) --> detn(exists),col(v,C1),[is,a],col(vt,C2).  

col(Pfx,C)-->subject(C,_,true),{atom_concat(Pfx,_,C)}.
col(_Pfx,C)-->{loosePass},subject(C,_,true).

% set of small things in the world
tCol(tSmall).  % I dont like doing this with adjectives.. but it cant be argued to be sane
tSmall(X) <==> mudSize(X,vSmall).

% set of green things in the world
tCol(tGreen).
tGreen(X) <==> mudColor(X,vGreen).

:-check_clause_counts.

%:-assertz_if_new(parserTest(iWorld7,"All green books are small.", (tGreen(X),tBook(X))==>tSmall(X))).
%:-assertz_if_new(parserTest(iWorld7,"Most green books are small.", pfc_default((tGreen(X),tBook(X))==>tSmall(X)))).

/*   The litmus

A thing can be lit or unlit. A thing is usually unlit.

Y can be C1 or C2.  
Y is [usually] C2.


An object has a text called printed name.  --> relationAllExists(mudPrintedName,tObj,ftText).
An object has a text called printed plural name.  --> mudPrintedPluralName(tObj,ftText).
An object has a text called an indefinite article.  --> mudIndefinateArticle(tObj,ftText).
An object can be plural-named or singular-named. An object is usually singular-named.  
                                                     --> partitionedInto(tObj,tSingularNamed,tPluralNamed). 
                                                         relationMostInstance(isa,tObj,tSingularNamed).

An object can be proper-named or improper-named. An object is usually improper-named.
                                             --> partitionedInto(tObj,tProperNamed,tImproperNamed).  
                                                 relationMostInstance(isa,tObj,tImproperNamed).

A room can be privately-named or publically-named. A room is usually publically-named.
                                             --> partitionedInto(tRoom,tPrivatelyNamed,tPublicallyNamed). 
                                                 relationMostInstance(isa,tObj,tPublicallyNamed).


A room can be lighted or dark. A room is usually lighted.
A room can be visited or unvisited. A room is usually unvisited.
A room has a text called description.




A thing can be edible or inedible. A thing is usually inedible.
A thing can be fixed in place or portable. A thing is usually portable.

A thing can be scenery.   -->  relationSomeInstance(isa,tItem,tScenery).
A thing can be wearable.  -->  relationSomeInstance(isa,tItem,tWearAble).

A thing can be pushable between rooms.   -->  relationSomeInstance(isa,tItem,tPushAble).  % between rooms?

A direction is a type of value.  -->  isa(vtDirection,ttValueType).
The north is a direction.  -->  isa(vNorth,vtDirection).
The northeast is a direction.  --> ..
The northwest is a direction.
The south is a direction.
The southeast is a direction.
The southwest is a direction.
The east is a direction.
The west is a direction.
The up is a direction.
The down is a direction.
The inside is a direction.
The outside is a direction.

The north has opposite south. Understand "n" as north.   --> mudHasOpposite(vNorth,vSouth).  mudKeyword(vNorth,"n").
The northeast has opposite southwest. Understand "ne" as northeast.
The northwest has opposite southeast. Understand "nw" as northwest.
The south has opposite north. Understand "s" as south.
The southeast has opposite northwest. Understand "se" as southeast.
The southwest has opposite northeast. Understand "sw" as southwest.
The east has opposite west. Understand "e" as east.
The west has opposite east. Understand "w" as west.
Up has opposite down. Understand "u" as up.
Down has opposite up. Understand "d" as down.
Inside has opposite outside. Understand "in" as inside.
Outside has opposite inside. Understand "out" as outside.

??????? TODO ?????????
The inside object translates into I6 as "in_obj".
The outside object translates into I6 as "out_obj".

??????? TODO ?????????
The verb to be above implies the mapping up relation.
The verb to be mapped above implies the mapping up relation.
The verb to be below implies the mapping down relation.
The verb to be mapped below implies the mapping down relatio

A door has an object called other side.
The other side property translates into I6 as "door_to".
Leading-through relates one room (called the other side) to various doors.
The verb to be through implies the leading-through relation.

S33. Containers and supporters. The carrying capacity property is the exception to the remarks above
about the qualitative nature of the world model: here for the first and only time we have a value which can
be meaningfully compared.
Section SR1/6 - Containers
The specification of container is "Represents something into which portable
things can be put, such as a teachest or a handbag. Something with a really
large immobile interior, such as the Albert Hall, had better be a room
instead."
A container can be enterable.
A container can be opaque or transparent. A container is usually opaque.
A container has a number called carrying capacity.
The carrying capacity of a container is usually 100.
Include (- has container, -) when defining a container

The specification of supporter is "Represents a surface on which things can be
placed, such as a table."
A supporter can be enterable.
A supporter has a number called carrying capacity.
The carrying capacity of a supporter is usually 100.
A supporter is usually fixed in place.
Include (-
has transparent supporter
-) when defining a supporte

A door can be open or closed. A door is usually closed.
A door can be openable or unopenable. A door is usually openable.
A container can be open or closed. A container is usually open.
A container can be openable or unopenable. A container is usually unopenable.

Before rules is a rulebook. [20]
Instead rules is a rulebook. [21]
Check rules is a rulebook. [22]
Carry out rules is a rulebook. [23]
After rules is a rulebook. [24]
Report rules is a rulebook. [25]

Action-processing rules is a rulebook. [10]
The action-processing rulebook has a person called the actor.
Setting action variables is a rulebook. [11]
The specific action-processing rules is a rulebook. [12]
The specific action-processing rulebook has a truth state called action in world.
The specific action-processing rulebook has a truth state called action keeping silent.
The specific action-processing rulebook has a rulebook called specific check rulebook.
The specific action-processing rulebook has a rulebook called specific carry out rulebook.
The specific action-processing rulebook has a rulebook called specific report rulebook.
The specific action-processing rulebook has a truth state called within the player''s sight.
The player''s action awareness rules is a rulebook. [13]
S16. The rules on accessibility and visibility, which control whether an action is physically possible, have
named outcomes as a taste of syntactic sugar.
Accessibility rules is a rulebook. [14]
Reaching inside rules is an object-based rulebook. [15]
Reaching inside rules have outcomes allow access (success) and deny access (failure).
Reaching outside rules is an object-based rulebook. [16]
Reaching outside rules have outcomes allow access (success) and deny access (failure).
Visibility rules is a rulebook. [17]
Visibility rules have outcomes there is sufficient light (failure) and there is
insufficient light (success).
S17. Two rulebooks govern the processing of asking other people to carry out actions:
Persuasion rules is a rulebook. [18]
Persuasion rules have outcomes persuasion succeeds (success) and persuasion fails (failure).
Unsuccessful attempt by is a rulebook. [19

*/
:- include(prologmud(mud_footer)).
:- all_source_file_predicates_are_transparent.

