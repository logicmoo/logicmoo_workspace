%%%	Society Proof
%%%	Authors: Marco Alberti, Federico Chesani
%%%	Created: 2003-11-27

:- nl, write('sciff_agent.pl: Starts consulting...'), nl.

:- use_module(library(chr)).
:- use_module(quantif).
:- use_module(ccopy).
:- use_module(ics_quant).
:- use_module( library(lists)).
:- use_module(library(terms),
         [term_variables/2]).
:- use_module(domains).
:- use_module(library(jasper)).
:- use_module(proof_util).
:- use_module(library(system)).
:- use_module( library(charsio)).
:- load_files(sciff).

:- nl, write('sciff_agent.pl: consulted all external files...'), nl.




%------------------------------------------------------------------------------
%	MAIN CICLE
%------------------------------------------------------------------------------
run_with_gui:-
	load_ics,
	current_time(0),
	society_goal,
	setReady,
	cicle(-1).

query_the_brain(MsgList) :-
	nl, write('query_the_brain: '), write(MsgList), nl,
	load_ics,
	current_time(0),
	society_goal,
	evalMsg(MsgList),
	get_orders(List),
	send_orders_java(List).


evalMsg([]).
evalMsg([H|T]) :-
	call(H),
	evalMsg(T).


cicle(Last) :-
	fetchNew(NMsg, Last, Actual),
	doContinue(NMsg, Last, Actual).

doContinue(_NMsg, _Last, -100) :-
	!,
	true.
doContinue(close_history, _Last, Actual) :-
	!,
	call(close_history),
	printConstraints(Actual).
doContinue(NMsg, _Last, Actual) :-
	call(NMsg),
	printConstraints(Actual),
	cicle(Actual).






%------------------------------------------------------------------------------
%   FETCH NEW PREDICATE
%------------------------------------------------------------------------------
fetchNew(TMsg, Last, Actual) :-
                clause(scv(OBJ), true),
                jasper_initialize([classpath(['.'])], JVM),
                jasper_call(JVM,
                                method('SocialComplianceVerifier', 'fetchNew', [instance]),
                                fetch_new(+object(''), +integer, [-string]),
                                fetch_new(OBJ, Last, Msg) ),
                atom_to_chars(Msg, LMsg),
                read_from_chars(LMsg, java_event(TMsg, Last, Actual)).


%------------------------------------------------------------------------------
%   SETTING READY
%------------------------------------------------------------------------------
setReady :-
                clause(scv(OBJ), true),
                jasper_initialize([classpath(['.'])], JVM),
                jasper_call(JVM,
                                method('SocialComplianceVerifier', 'setInitialized', [instance]),
                                set_initialized(+object('SocialComplianceVerifier')),
                                set_initialized(OBJ) ).


%------------------------------------------------------------------------------

print_all_state_java(Msg) :-
    clause(scv(OBJ), true),
    !,
    jasper_initialize([classpath(['.'])], JVM),
    jasper_call(JVM,
                    method('SocialComplianceVerifier', 'printState', [instance]),
                    print_state(+object('SocialComplianceVerifier'), +chars),
                    print_state(OBJ, Msg) ).
print_all_state_java(_).

print_all_state_java(Msg, Pos) :-
    clause(scv(OBJ), true),
    !,
    jasper_initialize([classpath(['.'])], JVM),
    jasper_call(JVM,
                    method('SocialComplianceVerifier', 'printState', [instance]),
                    print_state(+object('SocialComplianceVerifier'), +chars, +integer),
                    print_state(OBJ, Msg, Pos) ).
print_all_state_java(_, _).



%------------------------------------------------------------------------------
% PRINTING OF THE STATE THROUGH JAVA
%------------------------------------------------------------------------------
printConstraints :-
    print_all_constraints.
    
printConstraints(Pos) :-
    print_all_constraints(Pos).


%------------------------------------------------------------------------------
print_all_constraints :-
    findall_constraints(_, X),
    crea_lista_chars(X, Lista),
    print_all_state_java(Lista).

print_all_constraints(Pos) :-
    findall_constraints(_, X),
    crea_lista_chars(X, Lista),
    print_all_state_java(Lista, Pos).


%------------------------------------------------------------------------------

get_orders(List) :-
    findall_constraints(_, X),
    crea_lista_chars(X, List).

send_orders_java(Msg) :-
    clause(scv(OBJ), true),
    !,
    jasper_initialize([classpath(['.'])], JVM),
    jasper_call(JVM,
                    method('SCIFFBehaviour', 'sendOrders', [instance]),
                    print_state(+object('SCIFFBehaviour'), +chars),
                    print_state(OBJ, Msg) ).
send_orders_java(_).


%------------------------------------------------------------------------------


user:unknown_predicate_handler(society_goal, _, true) :-
	nl, write('WARNING: the society_goal predicate has not been defined!!!'), nl.


:- nl, write('sciff_agent.pl: consulted everything.'), nl.
