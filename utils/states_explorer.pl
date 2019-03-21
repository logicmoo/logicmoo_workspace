% Experiments finding states and event transitions by simulation (or abstract interpretation wrt time...?),
% when the LPS program is equivalent to a deterministic finite automata
% Assumptions: no time constants, finite states
% TODO: abort if term depth bigger than 2?? More???
:- module(states_explorer,[load_program/1,phb/0,print_phb/0,print_transitions/0]).

:- ensure_loaded('psyntax.P').
:- use_module('../engine/interpreter.P',[flat_sequence/2, action_/1, event_pred/1]).

% Backtrackable assert/retract of state
assert_fluent(X) :- interpreter:uassert(state(X)).
assert_fluent(X) :- interpreter:uretractall(state(X)), fail.
retract_fluent(X) :- 
	\+ \+ interpreter:state(X) -> (interpreter:uretractall(X); interpreter:uassert(X), fail) ; true.


load_program(F) :- golps(F,[dc,initialize_only]).

current_state(State) :- 
	interpreter:setof(S,(state(S), \+ system_fluent(S)),State).

% find possible "relevant" transitions from the current state
transition(Event,- Fl) :- 
	E=happens(Event,_,_),
	phb_tuple(holds(Fl,_)), interpreter:terminated(E,Fl,Cond), phb_tuple(E), 
	check_condition(Cond). 
	% we need to deal with free vars, and have "incoming" events present (extended state...?)
transition(Event, + Fl) :- 
	E=happens(Event,_,_),
	phb_tuple(holds(Fl,_)), interpreter:initiated(E,Fl,Cond), phb_tuple(E), 
	check_condition(Cond). 
transition(Event, [-Fl, + NewFl]) :- 
	E=happens(Event,_,_), phb_tuple(E), 
	interpreter:updated(E,Fl,Old-New,Cond), 
	phb_tuple(holds(Fl,_)), 
	interpreter:replace_term(Fl,Old,New,NewFl), phb_tuple(holds(NewFl,_)),
	check_condition(Cond). 

% TODO: transitions with multiple events; filter with preconditions!!

check_condition(Cond) :-
	flat_sequence(Cond,Flat), positive_abstract_sequence(Flat,Pos),
	bind_with_phb(Pos).

print_transitions :-
	setof((Ev->Fl),states_explorer:transition(Ev,Fl),Trans), !, 
	nl, writeln("Initial fluents state:\n----"),
	current_state(State), forall(member(F,State),writeln(F)),
	nl, writeln("State transitions:\n----"),
	forall(member(T,Trans), writeln(T)).
print_transitions :- 
	writeln("No transitions.").

% lps_literals(-Goal) a "clause"-like metapredicate to enumerate all head/body combinations
lps_literals([E,holds(Fl,_)|Cond]) :- interpreter:terminated(E,Fl,Cond).
lps_literals([E,holds(Fl,_)|Cond]) :- interpreter:initiated(E,Fl,Cond).
lps_literals([E,holds(NewFl,_)|Cond]) :- interpreter:updated(E,Fl,Old-New,Cond), (NewFl=Fl ; interpreter:replace_term(Fl,Old,New,NewFl)).
lps_literals(L) :- interpreter:d_pre(L).
lps_literals(L) :- interpreter:reactive_rule(H,B), append(H,B,L).
lps_literals([H|Body]) :- interpreter:l_int(H,Body); interpreter:l_events(H,Body); interpreter:l_timeless(H,Body).
lps_literals([Pred,Body]) :- user_prolog_clause(Pred,Body). 

user_prolog_clause(Pred,Body) :- 
	interpreter:lps_program_clause_file(Pred,Body,File), File\=asserted, 
	\+ sub_string(File,_,_,_,'/lps_corner/engine/'), 
	\+ sub_string(File,_,_,_,'/lps_corner/utils/'), 
	\+ interpreter:program_predicate(Pred).

lps_fact(H) :- 
	interpreter:l_int(H,[]); interpreter:l_events(H,[]); interpreter:l_timeless(H,[]); user_prolog_clause(H,true).

lps_literal(Literals,Lit) :- 
	interpreter:a_literal(Literals,L), nonvar(L), 
	% We're collecting constants/bindings, skip through negation
	((L = not(L_); L = holds(not(Fl),T), L_=holds(Fl,T)) -> lps_literal([L_],Lit) ; L=Lit).

% neg_free(Literal,JuicyLiteral) Strip negation of simple literals only
neg_free(G,_) :- var(G), !, throw(weird_var_goal).
neg_free(holds(not Fl,T),holds(Fl,T)) :- !.
neg_free(not(G),NF) :- !, neg_free(G,NF).
neg_free(G,G).


% abstract_literal: ground/replace time by $_LPS_TIME
abstract_literal(V,_) :- var(V), !, throw(weird_var_literal).
abstract_literal(holds(Fl,T),holds(Fl,'$_LPS_TIME')) :- !, (T='$_LPS_TIME'->true;true).
abstract_literal(happens(E,T1,T2),happens(E,'$_LPS_TIME','$_LPS_TIME')) :- !,
	(T1='$_LPS_TIME'->true;true), (T2='$_LPS_TIME'->true;true).
abstract_literal(L,L).

% positive_sequence(Sequence,PositivesOnly)  Skips and ignores negated subgoals
positive_abstract_sequence([not(_)|S],P) :- !, positive_abstract_sequence(S,P).
positive_abstract_sequence([holds(not(_),_)|S],P) :- !, positive_abstract_sequence(S,P).
positive_abstract_sequence([L|S],[AL|P]) :- !, abstract_literal(L,AL), positive_abstract_sequence(S,P).
positive_abstract_sequence([],[]).

% bind_with_phb(+Sequence,+AbduceAll)  AbduceAll is true/false: whether ALL goals are abducible (even non actions)
% Assumes no var subgoals
% binds literals in sequence using current tuples in phb
% considers all literals abducible
bind_with_phb([G|S],Ab) :- ground(G), !, 
	(system_literal(G) -> once(G) ; once(phb_tuple(G))),
% Replacing the above by the following commented lines causes too many transitions for goto_with_ifthenelse,
%	(system_literal(G) -> once(G) 
%		; \+ event_pred(G) -> once(phb_tuple(G)) 
%		; true), % abduce events
	bind_with_phb(S,Ab).
bind_with_phb([X=X|S],Ab) :- !, bind_with_phb(S,Ab).
% Somehow uncommenting this leads to no transitions being found...:
% bind_with_phb([G|S],Ab) :- action_(G), !, bind_with_phb(S,Ab). % abduce actions
bind_with_phb([G|S],Ab) :- phb_tuple(G), bind_with_phb(S,Ab).
bind_with_phb([_|S],true) :- bind_with_phb(S,true). % we also abduce events etc
bind_with_phb([],_).

bind_with_phb(S) :- bind_with_phb(S,false).


% TODO: deal properly with non user predicates!!!: 
system_literal(G) :- predicate_property(G,built_in).

thread_local phb_tuple/1. % preliminary Herbrand base

phb :- retractall(phb_tuple(_)), fail.
phb :- current_state(State), member(S,State), assert(phb_tuple(holds(S,'$_LPS_TIME'))), fail.
phb :- lps_fact(F), ground(F), \+ phb_tuple(F), assert(phb_tuple(F)), fail. % WHAT ABOUT non ground facts? should we care?
phb :- phb2.

phb2 :- 
	writeln("Starting a new pass..."),
	Flag=foo(_), 
	(
		% For each clause, considered with its positive literals only...
		lps_literals(L), % (L=[holds(loc(_11528,south),_)|_] -> trace ; true),
		flat_sequence(L,Flat), positive_abstract_sequence(Flat,Pos), 
		% .. try to bind those literals with the preliminary HB found so far...
		% writeln(binding-Pos),
		bind_with_phb(Pos,true), 
		% writeln(bound-Pos),
		member(Lit,Pos), ground(Lit), \+ system_literal(Lit), \+ phb_tuple(Lit),
		% we found a new one, remember it and continue:
		assert(phb_tuple(Lit)), 
		format("Remembering ~w~n",[Lit]),
		nb_setarg(1,Flag,added_at_least_one), fail
	; 
		arg(1,Flag,Arg), Arg==added_at_least_one, !, phb2 % try again with the extra tuples
	).
phb2 :- writeln("Finished preliminary HB").

print_phb :- 
	nl, writeln("Preliminary Herbrand Base:\n----"), setof(T,phb_tuple(T),Tuples), forall(member(T,Tuples),writeln(T)).

%! my_ite(:If,:Then,:Else)  an if-then-else with universal quantifier on the condition
my_ite(If,Then,_Else) :- 
	Flag=foo(_), 
	(If, nb_setarg(1,Flag,made_it), Then ; arg(1,Flag,Arg), nonvar(Arg), !, fail).
my_ite(_If,_Then,Else) :- Else.

% NEXT? 
% bind but evaluate negation and system predicates? delay system and negated literals and just run? negated if-then-else conditions?
% do not abstract time...?

