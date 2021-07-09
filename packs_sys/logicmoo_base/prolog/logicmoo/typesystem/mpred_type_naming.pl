% =======================================================
/* 
% This Naming System is mainly used by the mpred_loader but also needed everywhere
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
% =======================================================
% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_type_naming.pl
:- if(( ( \+ ((current_prolog_flag(logicmoo_include,Call),Call))) )).
:- module(mpred_type_naming,
          [ convertOneSpawnArg/4,
            convertSpawnArgs/4,
            convertToInstance/3,
            createByNameMangle/3,
            createByNameMangle0/3,
            createByNameMangle_compound/3,
            create_from_type/3,
            create_meta/4,
            get_source_suffix/2,
            /*
            i_name/2,
            i_name/3,
            i_name_lc/2,*/
            modality/3,
            addSpawn/1,
            addSpawn_modal/2,
            addSpawn_f_args/3,
            spawnOneSpawnArg/4,
            /*split_name_type/3,
            split_name_type_0/3,
            toCamelAtom1/2,
            toUpperCamelcase/2,
            to_atomic_name/3,
            to_iname/2,
            to_prefixed/3,
            typename_to_iname0/3,*/
            mpred_type_naming_file/0
          ]).

%:- include('mpred_header.pi').
:- endif.

:- set_how_virtualize_file(bodies).

:- thread_local(t_l:current_source_suffix/1).


% ================================================
% Naming System
% ================================================
:- was_export(create_meta/4).
% if SuggestedName was 'food666' it'd like the SuggestedClass to be 'food' and the stystem name will remain 'food666'
% if SuggestedName was 'food' it'd like the SuggestedClass to be 'food' and the stystem name will become a gensym like 'food1'

%= 	 	 

%% create_meta( ?SuggestedName, ?SuggestedClass, ?BaseClass, ?SystemName) is semidet.
%
% Create Meta.
%
create_meta(SuggestedName,SuggestedClass,BaseClass,SystemName):-
   must_det(split_name_type(SuggestedName,SystemName,NewSuggestedClass)),
   ignore(SuggestedClass=NewSuggestedClass),   
   assert_subclass_safe(SuggestedClass,BaseClass),
   assert_subclass_safe(NewSuggestedClass,BaseClass),
   assert_isa_safe(SystemName,BaseClass),
   assert_isa_safe(SystemName,NewSuggestedClass),
   assert_isa_safe(SystemName,SuggestedClass).



%= 	 	 

%% createByNameMangle( ?Name, ?IDA, ?InstAO) is semidet.
%
% Create By Name Mangle.
%
createByNameMangle(Name,IDA,InstAO):-must(createByNameMangle0(Name,IDA,InstAO)),!.


%= 	 	 

%% createByNameMangle0( ?S, ?I, ?C) is semidet.
%
% Create By Name Mangle Primary Helper.
%
createByNameMangle0(S,I,C):-is_list(S),toCamelAtom1(S,A),!,createByNameMangle0(A,I,C).
createByNameMangle0(S,I,C):-string(S),!,string_to_atom(S,A),!,createByNameMangle0(A,I,C).
createByNameMangle0(OType,Name,Type):-compound(OType),!,must(createByNameMangle_compound(OType,Name,Type)),!.
createByNameMangle0(Name,_,_Type):- \+ atom(Name),!,trace_or_throw(todo(not_atom_createByNameMangle(Name))).
%createByNameMangle0(OType,Name,Type):- isa_asserted(OType,tCol),!,create_from_type(OType,Name,Type).
createByNameMangle0(OType,Name,Type):-create_from_type(OType,Name,Type),!.
createByNameMangle0(Suggest,Name,Type):- once(split_name_type(Suggest,Name,Type)),(Suggest==Name;Suggest==Type),assert_isa(Name,Type),!.
createByNameMangle0(Name,I,C):-ereq(mudKeyword(W,KW)),string_equal_ci(Name,KW),!,createByNameMangle0(W,I,C).
createByNameMangle0(Name,IDA,Name):- gensym(Name,IDA), englishServerInterface([actCreate,Name,IDA]).


toCamelAtom1([A],O):-nonvar(A),!,toPropercase(A,O),!.
toCamelAtom1([A|List],O):-!,toPropercase(A,AO),toCamelAtom1(List,LO),atom_concat(AO,LO,O).
toCamelAtom1(A,O):-toPropercase(A,O),!.


%= 	 	 

%% createByNameMangle_compound( ?Name, ?Name, ?Type) is semidet.
%
% Create By Name Mangle Compound.
%
createByNameMangle_compound(Name,Name,Type):- Name=..[Type|Props],assert_isa(Name,Type),locally(t_l:deduceArgTypes(_),padd(Name,Props)).
createByNameMangle_compound(Name,Inst,Type):- functor_catch(Name,Type,A),must(A==1),assert_isa(Name,Type),Name=Inst.




%= 	 	 

%% get_source_suffix(NameNeedsSuffix, ?SS) is semidet.
%
% Get Source Suffix.
%
get_source_suffix(_NameNeedsNum,SS):- fail,
  source_location(F,_),!,file_directory_name(F,DN),
  directory_source_sufix(DN,SS),!,
  asserta_if_new(t_l:current_source_suffix(SS)).
get_source_suffix(_NameNeedsNum,SS):- t_l:current_source_suffix(SS),!.
get_source_suffix(_NameNeedsNum,'7').

:- volatile(lmcache:tmp_directory_source_sufix/2).
:- dynamic(lmcache:tmp_directory_source_sufix/2).
directory_source_sufix(DN,SSM):- lmcache:tmp_directory_source_sufix(DN,SSM),!.
directory_source_sufix(DN,SSM):- make_directory_source_sufix(DN,SSM),!,
  asserta(lmcache:tmp_directory_source_sufix(DN,SSM)),!.

make_directory_source_sufix(DN,SSM):- 
  file_base_name(DN,SS),
  atomic_list_concat(REV,'_',SS),reverse(REV,[SS1|_]),
  % gensym(SS1,SSGS),
  atomic_list_concat(['_',SS1,8],SSM),!.


clip_source_suffix(TypeStemNum,TypeStem):- get_source_suffix(TypeStem,SS), atom_concat(TypeStem,SS,TypeStemNum),!.
clip_source_suffix(TypeStem,TypeStem):-!.

%= 	 	 

%% create_from_type( ?OType, ?Name, ?Type) is semidet.
%
% Create Converted From Type.
%
create_from_type(InstOrType,Name,Type):- sanity(var(Name)),
  must_det_l(( 
   guess_type_name(InstOrType,Type),
   guess_inst_name(InstOrType,Type,Name),
   assert_isa(Type,tSet),
   assert_isa(Name,Type))),!.

guess_type_name(InstOrType,InstOrType):- isa_asserted(InstOrType,tCol),!.
guess_type_name(InstOrType,InstOrType):- atom_concat('t',_,InstOrType),!.
guess_type_name(InstOrType,Type):-
   i_name(InstOrType,TypeStemNum),
   clip_source_suffix(TypeStemNum,TypeStem),
   atom_concat('t',TypeStem,Type).

guess_inst_name(InstOrType,Type,InstOrType):- atom_concat('t',Type,InstOrType),!.
guess_inst_name(Type,Type,Name):-
   i_name('i',Type,NameNeedsNum),
   get_source_suffix(NameNeedsNum,SS),
   atom_concat(NameNeedsNum,SS,Name),!.
guess_inst_name(Inst,_Type,Inst).



% ========================================
% Spawn new instances
% ========================================


%= 	 	 

%% modality( ?VALUE1, ?VALUE2, ?VALUE3) is semidet.
%
% Modality.
%
modality(mdefault, [usually],[]).
modality(~ , [cannot],[can]).
modality(mdefault,[sometimes],[]).
modality(can,[can],[be]).
modality(possibly,[either],[]).
modality(~,[not],[]).
modality(~,[never],[]).


%% addSpawn( :TermA) is semidet. 
% 
% Spawn. 

addSpawn((A,B)):- must_be(nonvar,A),!,addSpawn(A),addSpawn(B). 
addSpawn(Class==>Fact):-!,mpred_post(Class==>{addSpawn(Fact)}). 
addSpawn(ClassFact):-   
 % get_startup_uu(UU),ain_expanded(Idea,UU)
   fully_expand(clause(assert,addSpawn),ClassFact,ClassFactO),!,  
   addSpawn_modal(t,ClassFactO).


%= 	 	 

%% addSpawn_modal( ?Modality, ?ClassFact) is semidet.
%
% Whenever spawn  Primary Helper.
%
addSpawn_modal(_Modality,ClassFact):- 
 ClassFact=..[FunctArgType,Name],
 modality(FunctArgType,_,_),!,
 must(addSpawn_modal(FunctArgType,Name)).
   
addSpawn_modal(Modality,ClassFact):-  ClassFact=..[FunctArgType,Name],
 call_u(tSet(FunctArgType)),
 must_det((
 createByNameMangle(Name,Inst,TypeA),
 assert_isa(TypeA,tCol),assert_isa(Inst,FunctArgType),assert_isa(Inst,TypeA),
 fully_expand(clause(assert,addSpawn),t(Modality,genls(TypeA,FunctArgType)),TO),
 add_on_start(TO))).
 

addSpawn_modal(Modality,ClassFact):- ClassFact=..[Funct|InstADeclB],
  must_det(addSpawn_f_args(Modality,Funct,InstADeclB)).


%= 	 	 

%% addSpawn_f_args( ?Modality, ?Funct, ?List) is semidet.
%
% Whenever Spawn Functor Arguments.
%
addSpawn_f_args(Modality,Funct,List):-
 must_det((
   convertSpawnArgs(Funct,1,List,NewList),!,
   Later =.. [Funct|NewList],
   fully_expand(clause(assert,addSpawn),t(Modality,Later),TO),
   add_on_start(TO))),!. 
  % call_after_mpred_load_slow(locally(t_l:deduceArgTypes(Funct), mpred_post(Later))))),!.


definitional(X):- \+ compound(X),!,fail.
definitional(isa(_,_)).
definitional(genls(_,_)).
definitional(t(TO)):- !, definitional(TO).
definitional(Compound):- functor(Compound,F,A), (A=1;if_defined(definitionalProp(F),fail)).

add_on_start(t(TO)):- nonvar(TO),!,add_on_start(TO).
add_on_start(TO):- definitional(TO),!,mpred_post(TO).
add_on_start(TO):- call_u(mpred_post(onStart(TO))).

%= 	 	 

%% convertSpawnArgs( ?Funct, ?N, :TermA, :TermO) is semidet.
%
% Convert Spawn Arguments.
%
convertSpawnArgs(_,_,[],[]).
convertSpawnArgs(Funct,N,[A|List],[O|NewList]):-
 must(convertOneSpawnArg(Funct,N,A,O)),!,
 N2 is N + 1,
 convertSpawnArgs(Funct,N2,List,NewList),!.


%= 	 	 

%% convertOneSpawnArg( ?VALUE1, ?VALUE2, ?O, ?O) is semidet.
%
% Convert One Spawn Argument.
%
convertOneSpawnArg(_,_,O,O):-string(O),!.
convertOneSpawnArg(_,_,O,O):-number(O),!.
convertOneSpawnArg(_,_,nospawn(O),O):-!.
convertOneSpawnArg(Funct,N,isInstFn(A),O):-spawnOneSpawnArg(Funct,N,A,O).
convertOneSpawnArg(Funct,N,A,O):-spawnOneSpawnArg(Funct,N,A,O).


%= 	 	 

%% spawnOneSpawnArg( ?VALUE1, ?VALUE2, ?VALUE3, ?VALUE4) is semidet.
%
% Spawn One Spawn Argument.
%
spawnOneSpawnArg(Funct,N,Name,Inst):- 
    N == 1, call_u(tCol(Funct)),
    must(convertToInstance(Name,Funct,Inst)),!.

spawnOneSpawnArg(Funct,N,Name,Inst):- 
    must(argIsa(Funct,N,FunctArgType)),!,
    must(convertToInstance(Name,FunctArgType,Inst)),!.


%= 	 	 

%% convertToInstance( ?Name, ?FunctArgType, ?Inst) is semidet.
%
% Convert Converted To Instance.
%
convertToInstance(Name,FunctArgType,Inst):- call_u(isa(Name,FunctArgType)),!,Inst=Name.
convertToInstance(Name,COLTHING,TypeA):- a(ttTypeType,COLTHING),createByNameMangle(Name,_,TypeA),assert_isa(TypeA,COLTHING).
convertToInstance(Name,COLTHING,TypeA):- call_u(genls(COLTHING,tCol)),createByNameMangle(Name,_,TypeA),assert_isa(TypeA,COLTHING).
convertToInstance(Name,FunctArgType,Inst):- createByNameMangle(Name,Inst,TypeA),
    assert_isa(Inst,FunctArgType),
    call_u(mpred_post(genls(TypeA,FunctArgType))),!.

:- fixup_exports.

mpred_type_naming_file.
