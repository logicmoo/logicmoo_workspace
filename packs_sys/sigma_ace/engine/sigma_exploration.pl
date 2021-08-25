:-include('sigma_header.pl').

% on a dirty ctx/kb run remake_positive_cache(KB,Ctx)
% on a dirty ctx/kb run remake_negative_cache(KB,Ctx)



:-dynamic(have_arity/4).

% these hold prototypes of calls
:-dynamic(positive_fact_cache/4).
:-dynamic(positive_rule_cache/4).
:-dynamic(negative_fact_cache/4).
:-dynamic(negative_rule_cache/4).

:-was_indexed(positive_fact_cache(1,1,1,1)).
:-was_indexed(positive_rule_cache(1,1,1,1)).
:-was_indexed(negative_fact_cache(1,1,1,1)).
:-was_indexed(negative_rule_cache(1,1,1,1)).
       
negative_fact_cache(KB,Ctx,+X):-not(positive_fact_cache(KB,Ctx,_,X)).
negative_fact_cache(KB,Ctx,-X):-not(positive_fact_cache(KB,Ctx,_,X)).

negative_rule_cache(KB,Ctx,+X):-not(positive_rule_cache(KB,Ctx,_,X)).
negative_rule_cache(KB,Ctx,-X):-not(positive_rule_cache(KB,Ctx,_,X)).

ambiguous_fact_cache(KB,Ctx,+X):- (positive_fact_cache(KB,Ctx,?,X)).
ambiguous_fact_cache(KB,Ctx,-X):- (positive_fact_cache(KB,Ctx,?,X)).

ambiguous_rule_cache(KB,Ctx,+X):- (positive_rule_cache(KB,Ctx,?,X)).
ambiguous_rule_cache(KB,Ctx,-X):- (positive_rule_cache(KB,Ctx,?,X)).
 

holds_wrapper(KB,Ctx,R,X,Y):-nonvar(R),!,
	P=..[R,X,Y],
	all_predicate_cache(KB,Ctx,+P).

holds_wrapper(KB,Ctx,R,X,Y,Z):-nonvar(R),!,
	P=..[R,X,Y,Z],
	all_predicate_cache(KB,Ctx,+P).

holds_wrapper(KB,Ctx,R,X,Y,Z,Q):-nonvar(R),!,
	P=..[R,X,Y,Z,Q],
	all_predicate_cache(KB,Ctx,+P).

holds_wrapper(KB,Ctx,R,X,Y):-
	all_predicate_cache(KB,Ctx,+P),
	P=..[R,X,Y].

holds_wrapper(KB,Ctx,R,X,Y,Z):-
	all_predicate_cache(KB,Ctx,+P),
	P=..[R,X,Y,Z].

holds_wrapper(KB,Ctx,R,X,Y,Z,Q):-
	all_predicate_cache(KB,Ctx,+P),
	P=..[R,X,Y,Z,Q].

not_holds_wrapper(KB,Ctx,R,X,Y):-nonvar(R),!,
	P=..[R,X,Y],
	all_predicate_cache(KB,Ctx,-P).

not_holds_wrapper(KB,Ctx,R,X,Y,Z):-nonvar(R),!,
	P=..[R,X,Y,Z],
	all_predicate_cache(KB,Ctx,-P).

not_holds_wrapper(KB,Ctx,R,X,Y,Z,Q):-nonvar(R),!,
	P=..[R,X,Y,Z,Q],
	all_predicate_cache(KB,Ctx,-P).

not_holds_wrapper(KB,Ctx,R,X,Y):-
	all_predicate_cache(KB,Ctx,-P),
	P=..[R,X,Y].

not_holds_wrapper(KB,Ctx,R,X,Y,Z):-
	all_predicate_cache(KB,Ctx,-P),
	P=..[R,X,Y,Z].

not_holds_wrapper(KB,Ctx,R,X,Y,Z,Q):-
	all_predicate_cache(KB,Ctx,-P),
	P=..[R,X,Y,Z,Q].       

positive_rule_fact(KB,Ctx,X):-
	positive_fact_cache(KB,Ctx,X),
	positive_rule_cache(KB,Ctx,X).

positive_rule_only(KB,Ctx,X):-
	positive_rule_cache(KB,Ctx,X),
	not(positive_fact_cache(KB,Ctx,X)).

positive_fact_only(KB,Ctx,X):-
	positive_fact_cache(KB,Ctx,X),
	not(positive_rule_cache(KB,Ctx,X)).


all_predicate_cache(KB,Ctx,X):-
	positive_fact_cache(KB,Ctx,X).
all_predicate_cache(KB,Ctx,X):-
	positive_rule_cache(KB,Ctx,X).

positive_fact_cache(KB,Ctx,+X):-positive_fact_cache(KB,Ctx,+,X).
positive_fact_cache(KB,Ctx,-X):-positive_fact_cache(KB,Ctx,-,X).
positive_fact_cache(KB,Ctx,+X):-positive_fact_cache(KB,Ctx,?,X).
positive_fact_cache(KB,Ctx,-X):-positive_fact_cache(KB,Ctx,?,X).
positive_rule_cache(KB,Ctx,+X):-var(X),!,positive_rule_cache(KB,Ctx,+,X).
 % Note that -X is missing from positive rule cache when its a X is a Var
positive_rule_cache(KB,Ctx,+X):-positive_rule_cache(KB,Ctx,+,X).
positive_rule_cache(KB,Ctx,-X):-positive_rule_cache(KB,Ctx,-,X).
positive_rule_cache(KB,Ctx,+X):-positive_rule_cache(KB,Ctx,?,X).
positive_rule_cache(KB,Ctx,-X):-positive_rule_cache(KB,Ctx,?,X).


all_predicate_fast_cache(KB,Ctx,X):-positive_fact_cache(KB,Ctx,X).
all_predicate_fast_cache(KB,Ctx,X):-positive_rule_cache(KB,Ctx,X).

all_predicate_slow_cache(KB,Ctx,X):-positive_fact_cache(KB,Ctx,X).
all_predicate_slow_cache(KB,Ctx,X):-positive_rule_only(KB,Ctx,X).
	
	
:-dynamic(make_positive_cache/2).

	

remake_positive_cache(KB,Ctx):-
	unmake_positive_cache(KB,Ctx),
	make_positive_cache(KB,Ctx),!.

unmake_positive_cache(KB,Ctx):-
	retractall((make_positive_cache(KB,Ctx):-!)).

make_positive_cache(KB,Ctx):-
	retractall(positive_fact_cache(_,_,_,_)),
	retractall(positive_rule_cache(_,_,_,_)),
	retractall(negative_fact_cache(_,_,_,_)),
	retractall(negative_rule_cache(_,_,_,_)),
	fail.

make_positive_cache(KB,Ctx):-
	sigmaCache(R,Cons, Ante, _, L, KB, Ctx, Proof),
	add_positive_rule_cache(KB,Ctx,L,R),
	fail.
	
make_positive_cache(KB,Ctx):-
	sigmaCache(R,Fact, _, L, KB, Ctx, Proof),
	add_positive_fact_cache(KB,Ctx,L,R),
	fail.

make_positive_cache(KB,Ctx):-
	asserta((make_positive_cache(KB,Ctx):-!)),!.

	
add_positive_fact_cache(KB,Ctx,L,R):-
	get_arity_fast(KB,Ctx,R,A),!,
	functor(P,R,A),
	not(positive_fact_cache(KB,Ctx,?,P)),
		((L=true  ->
			((
			retract(positive_fact_cache(KB,Ctx,-,P)) ->
				asserta(positive_fact_cache(KB,Ctx,?,P)) ;
				assert_if_new(positive_fact_cache(KB,Ctx,+,P))
			));
			((
			retract(positive_fact_cache(KB,Ctx,+,P)) -> 
				asserta(positive_fact_cache(KB,Ctx,?,P)) ;
				assert_if_new(positive_fact_cache(KB,Ctx,-,P))
			))      	
		)),!.

add_positive_rule_cache(KB,Ctx,L,R):-
	get_arity_fast(KB,Ctx,R,A),!,
	functor(P,R,A),
	not(positive_rule_cache(KB,Ctx,?,P)),
		((L=true  ->
			((
			retract(positive_rule_cache(KB,Ctx,-,P)) ->
				asserta(positive_rule_cache(KB,Ctx,?,P)) ;
				assert_if_new(positive_rule_cache(KB,Ctx,+,P))
			));
			((
			retract(positive_rule_cache(KB,Ctx,+,P)) -> 
				asserta(positive_rule_cache(KB,Ctx,?,P)) ;
				assert_if_new(positive_rule_cache(KB,Ctx,-,P))
			))      	
		)),!.
	
	
get_arity_fast(KB,Ctx,R,A):-have_arity(R,KB,Ctx,A),!.
get_arity_fast(KB,Ctx,R,A):-
	sigmaCache(R,Cons, _, _, KB, Ctx, Proof),
	functor(Cons,R,A),
	asserta(have_arity(R,KB,Ctx,A)),!.
get_arity_fast(KB,Ctx,R,A):-
	sigmaCache(R,Cons, _, _, _, KB, Ctx, Proof),
	functor(Cons,R,A),
	asserta(have_arity(R,KB,Ctx,A)),!.
get_arity_fast(KB,Ctx,R,A):-
	sigmaCache(valence, _, valence(R,A), Vars, KBName, Context, Tracking, User, Status),
	asserta(have_arity(R,KB,Ctx,A)),!.
get_arity_fast(KB,Ctx,R,2).
	



