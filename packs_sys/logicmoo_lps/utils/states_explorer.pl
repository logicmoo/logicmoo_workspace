% WARNING: preliminary incomplete experiment, use with care! 
% Finds "potential Herbrand Base", an Herbrand Base for LPS relaxing time overall and ignoring the filtering effect of negations and preconditions
% Finds potential states and event transitions by abstracting time, and optionally numbers
% Further assumptions: no time constants in heads, finite states
% TODO: abstract recursive structures, e.g. lists; right now... timeout is our kludge, resulting in an incomplete base.
% Example usage:
% /Applications/SWI-Prolog8.1.1.app/Contents/MacOS/swipl -l /Users/mc/git/lps_corner/utils/states_explorer.pl
% and then:
% load_program("/Users/mc/git/lps_corner/examples/CLOUT_workshop/loanAgreementPostConditions.pl"), phb, print_phb, print_transitions.
% load_program("/Users/mc/git/lps_corner/examples/CLOUT_workshop/goto_with_ifthenelse.pl"), phb, print_phb, print_transitions.
% Alternatively use print_transitions(false) to present transitions without abstracting numbers (e.g. showing concrete numbers)
%
% on SWISH, simply use explore (abstracts numbers) and explore_numbers (keeps numbers in the base)

:- module(states_explorer,[load_program/1,explore/2,phb/0,print_phb/0,print_transitions/0,print_phb/1,print_transitions/1]).

:- ensure_loaded('psyntax.P').
:- use_module('../engine/interpreter.P',[flat_sequence/2, action_/1, event_pred/1, abstract_numbers/2, user_prolog_clause/2]).
:- use_module('checker.P',[lps_literals/1]).

% Backtrackable assert/retract of state
assert_fluent(X) :- interpreter:uassert(state(X)).
assert_fluent(X) :- interpreter:uretractall(state(X)), fail.
retract_fluent(X) :- 
	\+ \+ interpreter:state(X) -> (interpreter:uretractall(X); interpreter:uassert(X), fail) ; true.

explore(F,Options) :- 
	(select(abstract_numbers,Options,Options_) -> AN=true; Options=Options_, AN=false),
	(select(phb_limit(TimeLimit),Options_,Options__) -> true; Options__=Options_, TimeLimit=0.5),
	interpreter:go(F,[initialize_only|Options__]),
	phb(TimeLimit), print_phb(AN), print_transitions(AN).

load_program(F) :- golps(F,[dc,initialize_only]).

current_state(State) :- 
	interpreter:setof(S,(state(S), \+ system_fluent(S)),State), !.
current_state([]).

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

print_transitions :- print_transitions(true).

print_transitions(AbstractNumbers) :-
	must_be(boolean,AbstractNumbers),
	setof((Ev_->Fl_),Ev^Fl^(
		states_explorer:transition(Ev,Fl),
		(AbstractNumbers==true -> abstract_numbers(Ev->Fl,Ev_->Fl_); Ev=Ev_, Fl=Fl_)
		),Trans), !, 
	nl, writeln("Initial fluents state:\n----"),
	current_state(State), forall(member(F,State),writeln(F)),
	nl, writeln("Potential state transitions:\n----"),
	forall(member(T,Trans), writeln(T)).
print_transitions(_) :- 
	writeln("No transitions.").

lps_fact(H) :- 
	interpreter:l_int(H,[]); interpreter:l_events(H,[]); interpreter:l_timeless(H,[]); user_prolog_clause(H,true).

lps_literal(Literals,Lit) :- 
	interpreter:a_literal(Literals,L), nonvar(L), 
	% We're collecting constants/bindings, skip through negation
	((L = not(L_); L = holds(not(Fl),T), L_=holds(Fl,T)) -> lps_literal([L_],Lit) ; L=Lit).

% neg_free(Literal,JuicyLiteral) Strip negation of simple literals only
neg_free(G,_) :- var(G), !, throw(weird_var_goal).
neg_free(holds(not(Fl),T),holds(Fl,T)) :- !.
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
bind_with_phb([Y is E|S],Ab) :- ground(E), !, % TODO: other system predicates with limited domains need to be handled too
	Y is E, bind_with_phb(S,Ab). 
bind_with_phb([G|S],Ab) :- ground(G),
	(system_literal(G) -> catch(once(G),_,true) % assume exceptions to be caused by time or number abstraction
		; once(phb_tuple(G))),
% Replacing the above by the following commented lines causes too many transitions for goto_with_ifthenelse,
%	(system_literal(G) -> once(G) 
%		; \+ event_pred(G) -> once(phb_tuple(G)) 
%		; true), % abduce events
	!,
	bind_with_phb(S,Ab).
bind_with_phb([X=X|S],Ab) :- !, bind_with_phb(S,Ab).
% Somehow uncommenting this leads to no transitions being found...:
% bind_with_phb([G|S],Ab) :- action_(G), !, bind_with_phb(S,Ab). % abduce actions
bind_with_phb([G|S],Ab) :- phb_tuple(G), bind_with_phb(S,Ab).
bind_with_phb([_|S],true) :- bind_with_phb(S,true). % abduce 
bind_with_phb([],_).

bind_with_phb(S) :- bind_with_phb(S,false).

% bind_with_time(+Literal,+Time)
bind_with_time(V,_T) :- var(V), !, throw(weird_var_literal).
bind_with_time(holds(_Fl,T),T) :- !. % TODO: deal with time expressions
bind_with_time(holds(_,_),_) :- !.
bind_with_time(happens(_,T1,T2),_T) :- ground(T1-T2), !.
bind_with_time(happens(_,T1,T2),T) :- ground(T1), !, T2=T.
bind_with_time(happens(_,T1,T2),T) :- ground(T2), !, T1=T.
bind_with_time(happens(E,T1,T2),_) :- throw(weird_happenning(happens(E,T1,T2))).
bind_with_time(holds(not(Fl),T),Time) :- !, bind_with_time(holds(Fl,T),Time) .
bind_with_time(not(G),T) :- !, bind_with_time(G,T).
bind_with_time(_,_).

% TODO: deal properly with non user predicates!!!: 
system_literal(G) :- predicate_property(G,built_in).

:- thread_local phb_tuple/1. % preliminary Herbrand base

% time(T) :- between(0,10,T). % time window

phb :- phb(0.05). % default time limit

phb(_) :- retractall(phb_tuple(_)), fail.
% phb :- current_state(State), member(S,State), assert(phb_tuple(holds(S,0))), fail.
phb(_) :- writeln("Remembering facts and initial state..."), current_state(State), member(S,State), assert(phb_tuple(holds(S,'$_LPS_TIME'))), fail.
phb(_) :- lps_fact(F), ground(F), \+ phb_tuple(F), assert(phb_tuple(F)), fail. % WHAT ABOUT non ground facts? should we care?
phb(TimeLimit) :- writeln("Starting bottom-up steps..."), phb2(TimeLimit), !.
phb(_).

phb2(TimeLimit) :- % seconds
	findall(t,phb_tuple(_),Tuples), length(Tuples,N),
	format("~w tuples~n",[N]),
	Flag=foo(_), 
	(
		% For each clause, considered with its positive literals only...
		lps_literals(L), % (L=[holds(loc(_11528,south),_)|_] -> trace ; true),
		% writeln(lps_literals(L)),
		flat_sequence(L,Flat), positive_abstract_sequence(Flat,Pos), 
		% .. try to bind those literals with the preliminary HB found so far...
		% writeln(Pos),
		G = (
				bind_with_phb(Pos,true), 
				%writeln(bound-Pos),
				member(Lit,Pos), ground(Lit), \+ system_literal(Lit), \+ phb_tuple(Lit),
				% we found a new one, remember it and continue:
				assert(phb_tuple(Lit)),
				nb_setarg(1,Flag,added_at_least_one), fail
			),
		%(L=[happens(transfer(_11924,_11926,_11928),_11918,_11920),holds(balance(_11926,_11910),_11906),_11912 is _11910+_11928] -> trace; true),
		% G, % useful for debugging
		catch(call_with_time_limit(TimeLimit,G),Ex, (Ex=time_limit_exceeded->Timeout=true;throw(Ex))),
		% If we succeeded, better be a timeout:
		(Timeout == true -> writeln("Timeout, incomplete base!"), !, fail ; throw(weird_condition))
		
	; 
		arg(1,Flag,Arg), Arg==added_at_least_one, !, phb2(TimeLimit) % try again with the extra tuples
	).
phb2(_) :- writeln("\nFixpoint reached.").

print_phb :- print_phb(true).

print_phb(AbstractNumbers) :- 
	must_be(boolean,AbstractNumbers),
	nl, writeln("----\nPotential Herbrand Base:\n"), 
	setof(T_, T^(phb_tuple(T), (AbstractNumbers==true->abstract_numbers(T,T_);T=T_) ),Tuples), 
	writeln("--Fluents:"),
	forall(member(holds(X,_),Tuples),writeln(X)),
	writeln("--Events and actions:"),
	forall(member(happens(X,_,_),Tuples),writeln(X)),
	writeln("--Timeless:"),
	forall((member(X,Tuples), X\=holds(_,_), X\=happens(_,_,_)),writeln(X)),
	writeln("----").

% my_ite(:If,:Then,:Else)  an if-then-else with universal quantifier on the condition
my_ite(If,Then,_Else) :- 
	Flag=foo(_), 
	(If, nb_setarg(1,Flag,made_it), Then ; arg(1,Flag,Arg), nonvar(Arg), !, fail).
my_ite(_If,_Then,Else) :- Else.

% NEXT? 
% bind but evaluate negation and system predicates? delay system and negated literals and just run? negated if-then-else conditions?
% do not abstract time...?

