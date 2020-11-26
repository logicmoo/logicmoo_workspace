%:- if(( ( \+ ((current_prolog_flag(logicmoo_include,Call),Call))) )). 
% :- module(world, []).
%:- endif.
/* * module  
% Common place to reduce redundancy World utility prediates
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
% Special thanks to code written on
% May 18, 1996
% written by John Eikenberry
% interface by Martin Ronsdorf
% general assistance Dr. Donald Nute
%
*/

% mtProlog(world).
:-export((
        % a_unparsed/2,
       % call_agent_action/2,
       get_agent_sessions/2,
            %mud_isa/2,
            isa_any/2,
            put_in_world/1,
            pathBetween_call/3,
            obj_memb/2,
            prop_memb/2,            
            from_dir_target/3,
            create_instance/2,create_instance/3,
            create_agent/1,
            create_agent/2,
            in_world_move/3, check_for_fall/3,
            agent_into_corpse/1, display_stats/1,
            reverse_dir/2,
            
            round_loc/8,
            round_loc_target/8,
            dir_offset/5,
            number_to_dir/3,
            list_agents/1,
            
            agent_list/1,
            check_for_fall/3,
            list_object_dir_sensed/4,
            list_object_dir_near/3,
            num_near_reverse/3,
            asInvoked/2,
            decl_type/1,
            
                       
         init_location_grid/1,
         grid_dist/3,
         to_3d/2,
         is_3d/1,
         in_grid/2,
         loc_to_xy/4,
         grid_size/4,
         doorLocation/5,
         foc_current_agent/1,
         locationToRegion/2,
         init_location_grid/2,
         
         do_act_affect/3,
         %spread/0,
         %growth/0,
         isaOrSame/2,
         current_agent_or_var/1)).


:-discontiguous create_instance_0/3.

:-export((
          create_instance/2,
          create_instance/3,
          create_instance_0/3,
          create_agent/1,
          create_agent/2)).

:- dynamic  agent_list/1.
% :- kb_shared(mudDescription/2).

:- include(prologmud(mud_header)).
% :- register_module_type (utility).

ensure_pfc_loaded(F):-atom_concat(F,'.pfc',FF),ensure_loaded(FF).

:- ensure_pfc_loaded(world_2d).
:- ensure_pfc_loaded(world_text).
:- ensure_pfc_loaded(world_text_output).
:- ensure_pfc_loaded(world_effects).
:- ensure_pfc_loaded(world_events).
:- ensure_pfc_loaded(world_agent).
:- ensure_pfc_loaded(world_npc).

% :- if_file_exists(include(logicmoo('vworld/world_spawning.pl'))).

:-export(isaOrSame/2).
isaOrSame(A,B):-A==B,!.
isaOrSame(A,B):-isa(A,B).

intersect(A,EF,B,LF,Tests,Results):-findall( A-B, ((member(A,EF),member(B,LF),once(Tests))), Results),[A-B|_]=Results.
% is_property(P,_A),PROP=..[P|ARGS],CALL=..[P,Obj|ARGS],req1(CALL).

obj_memb(E,L):-is_list(L)->member(E,L);E=L.

isa_any(E,L):-flatten([E],EE),flatten([L],LL),!,intersect(A,EE,B,LL,isaOrSame(A,B),_Results).

prop_memb(E,L):-flatten([E],EE),flatten([L],LL),!,intersect(A,EE,B,LL,isaOrSame(A,B),_Results).


tCol(tItem).
tCol(tAgent).
tCol(tRegion).
tCol(tItem).
tSet(tItem).
existingThing(O):-tItem(O).
existingThing(O):-tAgent(O).
existingThing(O):-tRegion(O).
anyInst(O):-tCol(O).
anyInst(O):-existingThing(O).

/*


% meta_argtypes(typeGenls(col,metaclass)).

%OLD decl_database_hook(change(assert,_),typeGenls(_,MC)):-assert_isa(MC,ttTypeType).

% deduce_facts(typeGenls(T,MC),deduce_facts(genls(S,T),isa(S,MC))).


*/

%genls(SubType,formattype):-isa(SubType,formattype).

%cached(G):-catch(G,_,fail).


tCol(ttNotSpatialType).

ttNotSpatialType(ftInt).
ttNotSpatialType(ftTerm).

genls(tWearAble,tItem).
genls(tItem,tLookAble).
genls(tRegion,tLookAble).
genls(tObj,tLookAble).
genls(tKnife,tItem).
genls(tFood,tItem).


%ttSpatialType(FT):- nonvar(FT),ttExpressionType(FT),!,fail.
%ttSpatialType(FT):- nonvar(FT),ttNotSpatialType(FT),!,fail.
%ttSpatialType(tItem). %  col, formattype, 
% ttSpatialType(SubType):-member(SubType,[tAgent,tItem,tRegion]).
%ttSpatialType(S):- is_asserted(ttSpatialType(T)), impliedSubClass(S,T).

%createableSubclassType(S,T):-req1(  ttSpatialType(T)),is_asserted(genls(S,T)).
%createableSubclassType(T,tSpatialThing):-req1( ttSpatialType(T)).

create_agent(P):-functor(P,isKappaFn,_),!.
create_agent(P):-create_agent(P,[]).
create_agent(P,List):- must(create_instance(P,tAgent,List)),!.




:-export(create_instance/1).
create_instance(P):- must(call_u((isa(P,What),ttSpatialType(What)))),must(create_instance(P,What,[])),!.
:-export(create_instance/2).
create_instance(Name,Type):-create_instance(Name,Type,[]).
%create_instance(Name,Type):-create_instance(Name,Type,[]).
:-export(create_instance/3).
create_instance(What,Type,Props):- 
  loop_check(time_call(create_instance_now(What,Type,Props)),dmsg(already_create_instance(What,Type,Props))).

create_instance_now(What,Type,Props):-
  must((var(Type);atom_concat('t',_,Type ))),!,
 locally_tl(t_l:agenda_suspend_scans,
  locally_tl(t_l:deduceArgTypes(_),
  locally_hide(t_l:useOnlyExternalDBs,
   locally_hide(t_l:noRandomValues(_),
     locally_hide(t_l:infInstanceOnly(_),   
      locally_hide(t_l:infAssertedOnly(_),
        locally_hide(baseKB:use_cyc_database, 
     ((split_name_type(What,Inst,_WhatType),assert_isa(Inst,Type), (create_instance_0(What,Type,Props)->true)))))))))),!.

:-discontiguous create_instance_0/3.

:-export(is_creating_now/1).
:- dynamic(is_creating_now/1).
:- dynamic(create_instance_0/3).


create_instance_0(What,Type,List):- (var(What);var(Type);var(List)),trace_or_throw((var_create_instance_0(What,Type,List))).
create_instance_0(I,_,_):-is_creating_now(I),!.
create_instance_0(I,_,_):-asserta_if_new(is_creating_now(I)),fail.
create_instance_0(What,FormatType,List):- FormatType\==tCol, ttExpressionType(FormatType),!,trace_or_throw(ttExpressionType(FormatType,create_instance(What,FormatType,List))).
create_instance_0(SubType,tCol,List):-ain(tCol(SubType)),(List==[]->true;padd(SubType,List)).

==>ttSpatialType(tAgent).
==>genls(tActor,tAgent).
==>genls(mobExplorer,tAgent).

==>prologHybrid(predTypeMax/3).
==>prologHybrid(predInstMax/3).

%NEXT TODO predInstMax(I,mudEnergy,NRG):- infSecondOrder, predTypeMax(mudEnergy,AgentType,NRG),isa(I,AgentType).
%predInstMax(I,mudHealth,Dam):- predTypeMax(mudHealth,AgentType,Dam),isa(I,AgentType).

punless(Cond,Action):- once((call(Cond);call(Action))).

create_instance_0(T,tAgent,List):-
  must_det_l((
   retractall(agent_list(_)),
   create_meta(T,_,tAgent,P),
   mreq(isa(P,tAgent)),
   padd(P,List),   
   % punless(mudPossess(P,_),modCreate:rez_to_inventory(P,food,_Food)),
   find_and_call(rez_to_inventory(P,tFood,_Food)),
   %reset_values(P),   
   padd(P, [ predInstMax(mudHealth,500),
                       predInstMax(mudEnergy,200),
                       mudHealth(500),
                       % mudEnergy(90),
                       mudAgentTurnnum(0),
                       mudScore(1)]),   
   % set_stats(P,[]),
   put_in_world(P),
   add_missing_instance_defaults(P))).

add_missing_instance_defaults(P):- ain(tNewlyCreated(P)).
   
/*
reset_values(I):- forall(valueReset(To,From),reset_value(I,To,From)).

reset_value(I,To,From):- prop(I,From,FromV), padd(I,To,FromV),!.
reset_value(I,To,From):- prop(I,From,FromV), padd(I,To,FromV),!.
   
   (contains_var(V,value),get_value(P,V,Result)) -> subst(V,P,isThis)
   argIsa(P,SVArgNum,Type),
   is_term_ft(V,Type),

valueReset(score,0).
valueReset(health,max_health).
valueReset(charge,max_charge).

*/

tCol(tTable).
relationMostInstance(mudColor, tTable, vWhite).

%typeProps(Type,Props),isa(Obj,Type),{flatten([Props],VoProps)} ==> props(Obj,VoProps).
ttSpatialType(tRegion).

create_instance_0(T, tItem, List):-
   isa(T,What),What\=tItem, ttSpatialType(What),!,create_instance_0(T, What, List).

/*
create_instance_0(T,Type,List):-
  createableSubclassType(Type,MetaType),!,
  must_det_l([
   create_meta(T,Type,MetaType,P),
   padd(P,List),
   add_missing_instance_defaults(P)]). 
*/

create_instance_0(T,MetaType,List):-  
  call_u((must_det_l((
   create_meta(T,_Type,MetaType,P),
   padd(P,List),
   add_missing_instance_defaults(P))))),!. 

create_instance_0(What,Type,Props):- leash(+call),wdmsg(assumed_To_HAVE_creted_isnance(What,Type,Props)),!.

%ttSpatialType(col).



% already convered mudPossess(Who,Thing):-genlInverse(W,mudPossess),into_mpred_form(t(W,Thing,Who),Call),req1(Call).
% already convered mudPossess(Who,Thing):-genlPreds(mudPossess,W),into_mpred_form(t(W,Who,Thing),Call),req1(Call).


% :- include(prologmud(mud_footer)).

:- all_source_file_predicates_are_transparent.

