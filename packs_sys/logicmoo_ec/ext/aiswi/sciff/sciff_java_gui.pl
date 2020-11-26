%%%	Society Proof
%%%	Authors: Marco Alberti, Federico Chesani
%%%	Created: 2003-11-27

:- nl, write('sciff_java_gui.pl: Starts consulting...'), nl.

:- use_module(library(chr)).
:- use_module( library(lists)).
:- use_module(library(terms),
         [term_variables/2]).
:- use_module(library(jasper)).
:- use_module(library(system)).
:- use_module( library(charsio)).


:- load_files(sciff).
:- use_module(proof_util).


:- nl, write('sciff_java_gui.pl: consulted all external files.'), nl.






%------------------------------------------------------------------------------
%	MAIN CICLE
%------------------------------------------------------------------------------
run_with_gui:-
	set_option(violation_causes_failure, no),
	load_ics,
	current_time(0),
	society_goal,
	setReady,
	cicle(-1).




cicle(Last) :-
	fetchNew(NMsg, Last, Actual),
	doContinue(NMsg, Last, Actual).

doContinue(_NMsg, _Last, -100) :-
	!,
	true.
doContinue(close_history, _Last, Actual) :-
	!,
	call(close_history),
	(	findall_constraints(viol(_), [_|_])
		->	!, 
				printConstraints,
				fail
		; printConstraints(Actual)
	).
doContinue(NMsg, _Last, Actual) :-
	call(NMsg),
	(	findall_constraints(viol(_), [_|_])
		->	!, 
				printConstraints,
				fail
		; printConstraints(Actual)
	),
	cicle(Actual).




%------------------------------------------------------------------------------
%   MANAGING THE FAILMENT OF E-CONSISTENCY and of NOT-CONSISTENCY
%------------------------------------------------------------------------------
:- prolog_flag(redefine_warnings,_, off).


check_unification(T1,T2,B,_,_):-
    ccopy(p(T1,T2),p(U1,U2)),
    reif_unify(U1,U2,B),!.
check_unification(T1,T2,_,S1,S2):-
    T1=..[_|A1],
    T2=..[_|A2],
    R1=..[S1|A1],
    R2=..[S2|A2],
    inconsistent(R1,R2),
    (	current_predicate(printConstraints/0)
     	->	printConstraints
     	;		true
    ),
    fail.





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




user:unknown_predicate_handler(society_goal, _, true) :-
	nl, write('WARNING: the society_goal predicate has not been defined!!!'), nl.


:- nl, write('sciff_java_gui.pl: consulted everything.'), nl.
