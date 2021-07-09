/*
% NomicMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
% Bits and pieces:
%
% LogicMOO, Inform7, FROLOG, Guncho, PrologMUD and Marty's Prolog Adventure Prototype
%
% Copyright (C) 2004 Marty White under the GNU GPL
% Sept 20, 1999 - Douglas Miles
% July 10, 1996 - John Eikenberry
%
% Logicmoo Project changes:
%
% Main file.
%
*/
:- '$set_source_module'(mu).

:- ensure_loaded('adv_props').

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CODE FILE SECTION
:- nop(ensure_loaded('adv_main_states')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CODE FILE SECTION
:- nop(ensure_loaded('adv_main_commands')).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


save_term_exists(Filename, Term) :-
 absolute_file_name(Filename, AbsFilename), !, Filename \== AbsFilename,
 save_term_exists(AbsFilename, Term), !.

save_term_exists(Filename, Term) :-
 \+ access_file(Filename, exist),
 save_term(Filename, write, Term),
 player_format('Saved to file "~w".~n', [Filename]), !.
save_term_exists(Filename, _) :-
 access_file(Filename, exist),
 player_format('Save FAILED! Does file "~w" already exist?~n', [Filename]).
save_term_exists(Filename, _) :-
 player_format('Failed to open file "~w" for saving.~n', [Filename]).


record_saved(State) :- is_list(State), !,
  sort(State, Term),
  with_mutex(save_advstate, must_mw1(save_term(library('episodic_memory/adv_state_db.pl'), write , Term))), !.
record_saved(State):-
  with_mutex(save_advstate, must_mw1(save_term(library('episodic_memory/adv_state_db.pl'), append , State))).

save_term(Filename, How, Term) :-
  is_list(Term), sort(Term, STerm), Term\==STerm, !,
  save_term(Filename, How, STerm).
save_term(Filename, How, Term) :-
 absolute_file_name(Filename, AbsFilename),
 setup_call_cleanup(open(AbsFilename, How, FH),
  format(FH, '~N~@.~N', [mu:adv_pretty_print(Term)]),
  close(FH)), !.
save_term(Filename, How, _) :-
 player_format('Failed to ~w file "~w" for saving.~n', [How, Filename]), !.



copy_prolog_sim(From,Into):-
 setup_call_cleanup(
 get_state_context(Before),
  must_mw1((
   set_state_context(From),
   get_advstate(InitState),
   set_state_context(Into),
   set_advstate(InitState),
   init_objects)),
 get_state_context(Before)), !.

:- multifile(extra_decl/2).
:- dynamic(extra_decl/2).
:- dynamic(undo/2).
%undO([u, u, u, u, u, u, u, u])).


get_advstate(State):- get_state_context(Ctx), get_advstate(Ctx, State).

get_advstate(Ctx, State):- atom(Ctx), nb_current(Ctx, State), !.
get_advstate(Ctx, State):- var(State), !, findall(State1, call(Ctx,State1), StateL), 
  flatten([structure_label(cur_state)|StateL], State).

get_advstate(Ctx, State):- call(Ctx,State), !.

into_ref(State, State):-!.
into_ref(State, StateRef):- notrace(get_attr(State, refValue, _)->StateRef=State;put_attr(StateRef, refValue, State)).
from_ref(State, State):-!.
from_ref(StateRef, State):- (var(State)->get_attr(StateRef, refValue, State);StateRef=State).

refValue_unify_hook(State, Value):-  State=Value.
refValue:attr_unify_hook(State, Value):- refValue_unify_hook(State, Value).
refValue:attribute_goals(Var)-->
  {get_attr(Var, refValue, Value), gensym(store_at_, Store), nb_setval(Store, Value)},
  [restore_refValue(Var, Store)].

restore_refValue(Var, Store):- nb_getval(Store, Value), put_attr(Var, refValue, Value).


/*add_advstate(State):- from_ref(State, State0),
  with_mutex(get_advstate, must_mw1(add_advstate_db(Ctx,State0))),
  nop(with_mutex(save_advstate, must_mw1(save_term(library('episodic_memory/adv_state_db.pl'), append , State0)))), !.
*/

update_running(StateInfo):-
 % dbug1(update_running(StateInfo)),
  ignore((get_advstate(S0), !, declare(StateInfo, S0, S1), !, set_advstate(S1))), !.
% update_running(_StateInfo).


set_advstate(State):- get_state_context(Ctx), set_advstate(Ctx, State).

set_advstate(Ctx, State):- atom(Ctx), nb_current(Ctx, _), must_be(list,State), !, nb_setval(Ctx, State). 
set_advstate(Ctx, State):- from_ref(Ctx, Ctx0), Ctx0\==Ctx, !, set_advstate(Ctx0, State).
set_advstate(Ctx, State):-  
  append_term(Ctx,_,Retractall),
  retractall(Retractall),
  add_advstate_db(Ctx,State),
  nop(record_saved(Ctx)), !.

odd_state(State):- \+ compound(State),!.
odd_state([istate|_More]).

add_advstate_db(_,Nil):- Nil==[], !.
add_advstate_db(Ctx,State):- odd_state(State), dumpST, dmsg(add_advstate_db(Ctx,State)), break.
add_advstate_db(Ctx,[H|T]):- !, 
   add_advstate_db(Ctx,H),
   add_advstate_db(Ctx,T).

% add_advstate_db(Ctx,type_props(player, [traits(player), sp=nouns])):- dumpST,break.
add_advstate_db(_,structure_label(cur_state)):-!.
add_advstate_db(Ctx,State):- 
   % dmsg(add_advstate_db(Ctx,State)),
   compound_name_arguments(State, Pred, StateL) ,
   % must_be(ground, Object),
   append(WithoutLastArg, [_NewData], StateL),
   append(WithoutLastArg, [_OldData], PreStateL),
   compound_name_arguments(PreState, Pred, PreStateL),
   nop((append_term(Ctx,PreState,Remove),forall(clause(Remove, true, Ref), erase(Ref)))),
   append_term(Ctx,State,Add),asserta(Add), !.

get_advstate_fork(StateC):- get_advstate(State), duplicate_term(State, StateC).

declared_advstate(Fact):- get_advstate(State), declared(Fact, State).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CODE FILE SECTION
:- nop(ensure_loaded(adv_state)).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% -----------------------------------------------------------------------------
% State may be implemented differently in the future (as a binary tree or
% hash table, etc.), but for now is a List. These (backtrackable) predicates
% hide the implementation:
% assert/record/declare/memorize/think/associate/know/retain/affirm/avow/
% insist/maintain/swear/posit/postulate/allege/assure/claim/proclaim
% retract/erase/forget/un-declare/unthink/repress/supress
% retrieve/remember/recall/ask/thought/think-of/reminisc/recognize/review/
% recollect/remind/look-up/research/establish/testify/sustain/attest/certify/
% verify/prove
% simulation: declare/undeclare/declared
% perception:
% memory: memorize/forget/thought

:- meta_predicate(update_agent_model_props(+, *, *, *)).
update_agent_model_props(Agent, Figment, M0, M1) :-  agent_mem(Agent, M0, M1, A0, A1),
  memorize_edit_0(Agent, append, Figment, A0, A1).

:- meta_predicate(memorize_edit_0(+, 3, *, *, *)).
memorize_edit_0(_Agent, Pred3, Figment, M0, M2) :- assertion(\+ is_list(Figment)),
 must_mw1((
   Figment =.. [Name, Value], OldFigment =.. [Name, OldValue],
   (select(OldFigment, M0, M1)
     -> ( call(Pred3, OldValue, Value, NewValue), NewFigment =.. [Name, NewValue])
     ; (NewFigment=Figment, M0=M1)),
    append([NewFigment], M1, M2))).
 
memorize_appending(Agent, Figment0, M0, M1) :-
 must_mw1((agent_mem(Agent, M0, M1, A0, A1),
  listify(Figment0, Figment),
  episodic_mem(Agent, memorize_appending(Figment)),
  memorize_edit_0(Agent, append, Figment, A0, A1))).

% Manipulate memories (M stands for Memories)
memorize_prepending(Agent, Figment0, M0, M1) :- must_mw1((agent_mem(Agent, M0, M1, A0, A1),
  listify(Figment0, Figment),
  episodic_mem(Agent, memorize_prepending(Figment)),
  notrace(append(Figment, A0, A1)))).

replace_thought(Agent, Figment0, M0, M1) :-    must_mw1((agent_mem(Agent, M0, M1, A0, A3),
  var(A3),
  must_unlistify(Figment0, Figment),
  old_figment(Figment, _F, _A, FigmentErased),
  select(FigmentErased, A0, A1),
  % episodic_mem(Agent, replace(FigmentErased, Figment)),
  append([Figment], A1, A3))), !.

thought(Agent, Figment0, M) :-         
 must_mw1(( agent_mem(Agent, M, A),
  must_unlistify(Figment0, Figment))), !,
  (declared(Figment, A) -> episodic_mem(Agent, thought(Figment)); (episodic_mem(Agent, considered(Figment)), fail)), !.

thought_check(Agent, Figment0, M) :-
  must_mw1(( agent_mem(Agent, M, A),
   must_unlistify(Figment0, Figment))), !,
  declared(Figment, A).

/*
memorize(Agent, Figment0, M0, M1):- memorize_prepending(Agent, Figment0, M0, M1), !.


forge t(Agent, Figment0, M0, M1) :-   agent_mem(Agent, M0, M1, A0, A1),
  must_unlistify(Figment0, Figment), !,
  select_from(Figment, A0, A1),
  nop(episodic_mem(Agent, forget(Figment))).

forget_ always(Agent, Figment0, M0, M1) :-   agent_mem(Agent, M0, M1, A0, A1),
  must_unlistify(Figment0, Figment),
  select_always(Figment, A0, A1),
  nop(episodic_mem(Agent, forget(Figment))).
*/





must_unlistify(Figment0, Figment):- is_list(Figment0), !, must_mw1(Figment0=[Figment]).
must_unlistify(Figment, Figment).

in_agent_model(Agent, Fact, State):- in_model(Fact, State)*-> true ; 
  (agent_thought_model(Agent, ModelData, State), in_model(Fact, ModelData)).

in_model(E, L):- in_model0(E, L).

in_model0(E, L):- \+ is_list(L), declared_link(declared, E, L).
in_model0(E, L):- compound(E), E = holds_at(_, _), !, member(E, L).
in_model0(E, L):- member(EE, L), same_h_element(EE, EO),E=EO.

same_h_element(E, E) :- !.
same_h_element(holds_at(E, T), E):- nonvar(T).

agent_mem(_Agent, Mem0, Mem1, AMem0, AMem1):- Mem0=AMem0, Mem1=AMem1.
agent_mem(_Agent, Mem0, AMem0):- Mem0=AMem0.

:- defn_state_getter(agent_thought_model(agent, model)).
agent_thought_model(Agent, ModelData, M0):- agent_thought_model_0(Agent, ModelData, M0).
agent_thought_model_0(Agent, ModelData, M0):- var(M0), get_advstate(State), !, member(memories(Agent, M0), State), agent_thought_model(Agent, ModelData, M0).
agent_thought_model_0(Agent, ModelData, M0):- \+ is_list(M0), !, declared_link(agent_thought_model(Agent), ModelData, M0).
agent_thought_model_0(Agent, ModelData, M0) :- memberchk(propOf(memories, Agent), M0), ModelData = M0, !.
agent_thought_model_0(Agent, ModelData, M0):- declared(memories(Agent, M1), M0), !,
  agent_thought_model_0(Agent, ModelData, M1).


:- export(declare/3).
%:- defn_state_setter(declare(fact)).
select_from(Fact, State, NewState) :- notrace((assertion(var(NewState)), is_list(State))), !,
   notrace(select_from_list(Fact, State, NewState)).
select_from(Fact, inst(Object), inst(Object)):- !,
   get_advstate(State),
   (declared(props(Object, PropList), State);PropList=[]), !,
   select_from_list(Fact, PropList, NewPropList),
   select_always(props(Object, _), State, MidState),
   append([props(Object, NewPropList)], MidState, NewState),
   set_advstate(NewState).
select_from(Fact, type(Type), type(Type)):- !,
   get_advstate(State),
   (declared(type_props(Type, PropList), State);PropList=[]), !,
   select_from_list(Fact, PropList, NewPropList),
   select_always(type_props(Type, _), State, MidState),
   append([type_props(Type, NewPropList)], MidState, NewState),
   set_advstate(NewState).
select_from(Fact, Pred1Name, Pred1Name):- is_pred1_state(Pred1Name), append_term(Pred1Name, State, DBPred), (retract(DBPred);State=[]), !, select_from_list(Fact, State, NewState), DBPredNewState=..[Pred1Name, NewState], asserta(DBPredNewState).
select_from(Fact, VarName, VarName):- atom(VarName), nb_current(VarName, PropList), select_from_list(Fact, PropList, NewPropList), b_setval(VarName, NewPropList).
select_from(Fact, Object, Object):- callable(Fact), !, Fact=..[F|List],
  Call=..[F, NewArg|List],
  current_predicate(_, Call), !,
  ignore( \+ \+ retract(Call)),
  NewArg=Object,
  asserta(Call).

select_from_list(Item, List, ListWithoutItem):- select(Item, List, ListWithoutItem).

% Like select_from, but always succeeds, for use in deleting.
select_always(Item, List, ListWithoutItem) :- select_from(Item, List, ListWithoutItem) -> true; ListWithoutItem=List.

% Like select_from, but with a default value if not found in List..
%select_default(Item, _DefaultItem, List, ListWithoutItem) :-
% select_from(Item, List, ListWithoutItem).
%select_default(DefaultItem, DefaultItem, ListWithoutItem, ListWithoutItem).

% Manipulate simulation state
% declare(Fact, State):- player_local(Fact, Player), !, declare(wishes(Player, Fact), State).
:- export(declare/3).
:- defn_state_setter(declare(fact)).

declare(h(A,B,C), State, NewState):- dbug1(bad_arity_declare(h(A,B,C), State, NewState)),!,dumpST,break.
declare(Fact, State, NewState):- must_mw1(declare_0(Fact, State, NewState)).

declare_0(Fact, State, NewState) :- notrace((assertion(var(NewState)), is_list(State))), !, notrace(declare_list(Fact, State, NewState)).
declare_0(Fact, inst(Object), inst(Object)):- !,
   get_advstate(State),
   (declared(props(Object, PropList), State);PropList=[]), !,
   declare_list(Fact, PropList, NewPropList),
   select_always(props(Object, _), State, MidState),
   append([props(Object, NewPropList)], MidState, NewState),
   set_advstate(NewState).
declare_0(Fact, type(Type), type(Type)):- !,
   get_advstate(State),
   (declared(type_props(Type, PropList), State);PropList=[]), !,
   declare_list(Fact, PropList, NewPropList),
   select_always(type_props(Type, _), State, MidState),
   append([type_props(Type, NewPropList)], MidState, NewState),
   set_advstate(NewState).
declare_0(Fact, Pred1Name, Pred1Name):- is_pred1_state(Pred1Name), append_term(Pred1Name, State, DBPred), (retract(DBPred);State=[]), !,
   declare_list(Fact, State, NewState), append_term(Pred1Name, NewState, DBPredNewState), asserta(DBPredNewState).
declare_0(Fact, VarName, VarName):- atom(VarName), nb_current(VarName, PropList), declare_list(Fact, PropList, NewPropList), b_setval(VarName, NewPropList).
declare_0(Fact, Object, Object):- callable(Fact), !, Fact=..[F|List],
  Call=..[F, NewArg|List],
  current_predicate(_, Call), !,
  ignore( \+ \+ retract(Call)),
  NewArg=Object,
  asserta(Call).


merge_proplists(AddPropList, OldPropList, NewPropList):-
  append(AddPropList, OldPropList, NewPropListL), list_to_set(NewPropListL, NewPropList), !.

declare_list(Fact, State, NewState) :- assertion(compound(Fact)), assertion(var(NewState)), Fact==[], !, NewState = State.
declare_list((Fact1, Fact2), State, NewState) :- !, declare_list(Fact1, State, MidState), declare_list(Fact2, MidState, NewState).
declare_list([Fact1|Fact2], State, NewState) :- !, declare_list(Fact1, State, MidState), declare_list(Fact2, MidState, NewState).
declare_list(HasList, State, [NewFront|NewState]) :-
  HasList=..[F, Object, AddPropList],
  Old=..[F, Object, OldPropList],
  select_from(Old, State, NewState), !,
  assertion(is_list(OldPropList)),
  merge_proplists(AddPropList, OldPropList, NewPropList),
  NewFront=..[F, Object, NewPropList].
declare_list(HasList, State, [NewFront|NewState]) :-
  safe_functor(HasList, F, A), arg(A, HasList, PropList), is_list(PropList),
  safe_functor(Functor, F, A), \+ \+ type_functor(state, Functor),
  arg(1, HasList, Object), arg(1, Functor, Object),
  select_from(Functor, State, NewState), !,
  arg(A, Functor, OldPropList), assertion(is_list(OldPropList)),
  append(PropList, OldPropList, NewPropList),
  assertion(A=2;A=3), NewFront=..[F, Object, NewPropList].
declare_list(Fact, State, NewState) :- append([Fact], State, NewState).


append_toplevel_props(perceptq(Agent, Events), S0, S2):-
 must_mw1((
 pre_redeclare(perceptq(Agent, Queue), S0, S1),
 append(Queue, Events, NewQueue),
 redeclare(perceptq(Agent, NewQueue), S1, S2),
 declared(perceptq(Agent, RQueue), S0, _),
 is_list(RQueue))).


:- defn_state_setter( pre_redeclare( + fact)).
pre_redeclare(Fact, State, NewState):- declared(Fact, State),!, NewState=State.
pre_redeclare(Fact, State, NewState):- must_mw1(functor(Fact,_,Arg)), ignore(arg(Arg,Fact,[])), NewState=[Fact|State],!.
%pre_redeclare(Fact, State, NewState):- undeclare(Fact, State, NewState).

:- defn_state_setter( redeclare( + fact)).
redeclare(Fact, State, NewState):-
 must_mw1((old_figment(Fact, _F, A, Old),
 (undeclare(Old, State, MidState);(State=MidState, ignore(arg(A, Old, [])))),
 nop(episodic_mem($agent, replace(Old, Fact))),
 declare(Fact, MidState, NewState))).

%undeclare(Fact, State):- player_local(Fact, Player), !, undeclare(wishes(Player, Fact), State).
undeclare(Fact, State, NewState):- !, state_prop(undeclare_list,Fact, State, NewState),!.
undeclare(Fact, State, NewState):- notrace(undeclare_list(Fact, State, NewState)).
undeclare_list(Fact, State, NewState) :- assertion(is_list(State)), copy_term(State, Copy), select_from(Fact, State, NewState),
 assertion( \+ member(Copy , NewState)).

%undeclare_always(Fact, State):- player_local(Fact, Player), !, undeclare_always(wishes(Player, Fact), State).
%undeclare_always(Fact, State, NewState):- state_prop(select_always,Fact, State, NewState),!.
undeclare_always(Fact, State, NewState) :- select_always(Fact, State, NewState).

% declared(Fact, State) :- player_local(Fact, Player), !, declared(wishes(Player, Fact), State).

declared(State) :- % dumpST, throw
  declared_advstate(State).

:- export(declared/2).
:- defn_state_getter(declared(fact)).
declared(Fact, State) :- declared_0(Fact, State).

declared_0(Fact, State) :-
  is_list(State)->declared_list(Fact, State);declared_link(declared, Fact, State).

declared_list(Fact, State) :- assertion(is_list(State)), member(Fact, State).
declared_list(Fact, State) :- member(link(VarName), State), nonvar(VarName), declared_link(declared, Fact, VarName).
declared_list(Fact, State) :- member(propOf(_, Object), State), declared_link(declared, Fact, Object).

:- meta_predicate(declared_link(2, ?, *)).
declared_link(Pred2, Fact, VarName):- strip_module(Pred2, _, Var), var(Var), !, declared_link(declared, Fact, VarName).
declared_link(Pred2, Fact, VarName):- atom(VarName), nb_current(VarName, PropList), PropList\==[], !, call(Pred2, Fact, PropList).
declared_link(Pred2, Fact, inst(Obj)):- declared_advstate(props(Obj, PropList)), call(Pred2, Fact, PropList).
declared_link(Pred2, Fact, type(Type)):- declared_advstate(type_props(Type, PropList)), call(Pred2, Fact, PropList).
% declared_link(Pred2, Fact, inst_model(Obj, Type)):- declared_advstate(props(Type, PropList)), call(Pred2, Fact, PropList).
declared_link(Pred2, Fact, Object):- nonvar(Object), extra_decl(Object, PropList), call(Pred2, Fact, PropList).
declared_link(Pred2, Fact, Object):- get_advstate(State), 
   setup_call_cleanup(
         flag('$direct_props2',X,X+1), 
         ((X = 0, direct_props2(Object, PropList, State))),
         flag('$direct_props2',_,X)),
   call(Pred2, Fact, PropList), !.
declared_link(declared, Fact, Object):- callable(Fact), Fact=..[F|List], Call=..[F, Object|List],
  current_predicate(_, Call), call(Call), !.
declared_link(declared, Fact, Object):- callable(Fact), Fact=..[F|List], Call=..[F, Object|List], declared_advstate(Call), !.
declared_link(Pred2, Fact, Object):- var(Object), get_advstate(State), member(Prop, State), arg(1, Prop, Object), arg(2, Prop, PropList),
  call(Pred2, Fact, PropList).
declared_link(declared, Fact, _Object):- declared_advstate(Fact), !.



as_first_arg(Object, Prop, Element):-
  callable(Prop), Prop=..[Name| Value], Element =..[Name, Object| Value].








:- export(is_state_info/1).

is_state_info(StateInfo):- \+ compound(StateInfo), !, fail.
is_state_info(StateInfo):- safe_functor(StateInfo, F, A),
   (functor_arity_state(F, A)->true; (A>2, functor_arity_state(F, 2))).


functor_arity_state(F, A):- is_type_functor(state, F, A).
functor_arity_state(type, 2).
%functor_arity_state(F, A):- is_spatial_rel(F).
%functor_arity_state(F, A):- is_spatial_rel(F).

episodic_mem(x(floyd, _), _Figment) :-!.
episodic_mem(Agent, Figment) :- is_list(Figment), !, forall(member(F, Figment), episodic_mem(Agent, F)).
episodic_mem(Agent, Figment):- compound(Figment), arg(1, Figment, Ag), Ag==Agent, !, note_episodic_mem(Figment).
episodic_mem(Agent, Figment) :- note_episodic_mem(episodic_mem(Agent, Figment)), !.

note_episodic_mem(Figment):-
  nop((notrace((format('~N', []), in_color(pink, print_tree(Figment)), format('~N', []), overwrote_prompt)))).

% for  the "TheSims" bot AI which will make the bots do what TheSims characters do... (they dont use the planner they use a simple priority routine)

state_list_op(OP, Fact, PropList, Unused):- Unused == unused, !, call(OP, Fact, PropList). 
state_list_op(OP, Fact, PropList, NewPropList):- call(OP, Fact, PropList, NewPropList). 

state_prop(OP, Fact, State, NewState):- quietly(state_prop_0(OP, Fact, State, NewState)).

state_prop_0(OP, Fact, State, NewState) :- notrace((assertion(var(NewState)), is_list(State))), !, state_list_op(OP, Fact, State, NewState).
state_prop_0(OP, Fact, State, NewState) :- dmsg(state_prop_0(OP, Fact, State, NewState)), fail.
state_prop_0(OP, Fact, inst(Object), OUT):- !,
   get_advstate(State),
   (declared(props(Object, PropList), State);PropList=[]), !,
   state_list_op(OP, Fact, PropList, NewPropList),
   select_always(props(Object, _), State, MidState),
   append([props(Object, NewPropList)], MidState, NewState),   
   set_advstate(NewState),
   store_op(inst(Object),NewPropList,OUT).
   
state_prop_0(OP, Fact, type(Type), OUT):- !,
   get_advstate(State),
   (declared(type_props(Type, PropList), State);PropList=[]), !,
   state_list_op(OP, Fact, PropList, NewPropList),
   select_always(type_props(Type, _), State, MidState),
   append([type_props(Type, NewPropList)], MidState, NewState),
   set_advstate(NewState),
   store_op(type(Type),NewPropList,OUT).

state_prop_0(OP, Fact, VarName, OUT):- atom(VarName), nb_current(VarName, PropList), 
   state_list_op(OP, Fact, PropList, NewPropList), 
   b_setval(VarName, NewPropList),
   store_op(VarName,NewPropList,OUT).

state_prop_0(OP, Fact, Pred1Name, OUT):- is_pred1_state(Pred1Name), append_term(Pred1Name, State, DBPred), 
  (retract(DBPred);State=[]), !,
   state_list_op(OP, Fact, State, NewState), 
   append_term(Pred1Name, NewState, DBPredNewState), asserta(DBPredNewState),
   store_op(Pred1Name,NewState,OUT).   

state_prop_0(OP, Fact, Object, Object):- dmsg(unsupported_state_prop(OP, Fact, Object, Object)),fail.

state_prop_0(_OP, Fact, Object, Object):- callable(Fact), !, Fact=..[F|List],
  Call=..[F, NewArg|List],
  current_predicate(_, Call), !,
  ignore( \+ \+ retract(Call)),
  NewArg=Object,
  asserta(Call).

store_op(VarName,_NewPropList,OUT):- ignore(VarName=OUT).

:- dynamic(mu:adv_state_context/1).
get_state_context(Ctx):- mu:adv_state_context(Ctx),!.
set_state_context(Ctx):- asserta(mu:adv_state_context(Ctx)), ensure_state_context(Ctx).

:- dynamic(mu:is_pred1_state/1).
ensure_state_context(Ctx):- mu:is_pred1_state(Ctx), !.
ensure_state_context(Ctx):- asserta_new(mu:is_pred1_state(Ctx)), fail.
ensure_state_context(Ctx):- atom(Ctx), nb_setval(Ctx,[structure_label(Ctx) ]),fail.
ensure_state_context(Ctx):- append_term(Ctx,_,P), functor(P,F,A), dynamic(F/A), clear_state_context(Ctx).


clear_state_context(Ctx):- set_advstate(Ctx,[structure_label(Ctx) ]).

notrace_state(G):- call(G).

:- ensure_state_context(advstate_db).
:- ensure_state_context(istate).
:- ensure_state_context(statest).
:- ensure_state_context(parseFrame(_)).

:- fixup_exports.


