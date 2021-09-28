% Predicate to portray qualitative models learnt 
% using the davinci graph drawing tool

aleph_portray(hypothesis):-
	hypothesis(Head,Body,_),
	exists_causal_order((Head:-Body),Order),
        open('hypothesis.davinci',write,Stream),
        set_output(Stream),
        write_davinci_qmodel(Order),
        close(Stream),
        set_output(user_output),
        concat([davinci,' ','hypothesis.davinci'],Cmd),
        execute(Cmd).


write_davinci_qmodel([_-Head|Body]):-
	numbervars([Head|Body],0,_),
	exogenous_variables(Head,E),
	state_variables(Head,S),
	conc(E,S,Given),
	write('['), nl,
	write_variables(Given,Body),
	(Body \= [] -> write(','), nl; true),
	write_davinci_qmodel(Body,Given),
	nl, write(']'), nl.

write_davinci_qmodel([],_):- !.
write_davinci_qmodel([N-Lit|Lits],Seen):-
	get_vars_in_lit(Lit,Vars),
	set_diff(Vars,Seen,New,Old),
	write_constraint(N-Lit,New),
	(New \= [] -> write(','), nl; true),
	write_variables(New,Lits), 
	(Lits \= [] -> write(','), nl; true),
	conc(Seen,New,Seen1),
	write_davinci_qmodel(Lits,Seen1).



write_variables([],_):- !.
write_variables([Var|Vars],Lits):-
	get_var_occurs(Lits,Var,DepLits),
	write_davinci_node([Var,variable,white,text,Var],DepLits,[solid,none]),
	(Vars \= [] -> write(','), nl; true),
	write_variables(Vars,Lits).

write_constraint(N-Lit,New):-
	functor(Lit,Name,_),
	write_constraint(N,Name,New).

write_constraint(N,Name,New):-
	get_constraint_attributes(Name,[Text,Shape]),
	write_davinci_node([N,constraint,white,Shape,Text],New,[solid,none]).


get_constraint_attributes(deriv,['DT',box]):- !.
get_constraint_attributes(addl,['+/-',circle]):- !.
get_constraint_attributes(addf,['+/-',circle]):- !.
get_constraint_attributes(mult,['x',circle]):- !.
get_constraint_attributes(m_plus,['M+',box]):- !.
get_constraint_attributes(m_minus,['M-',box]):- !.
get_constraint_attributes(minusl,['-',circle]):- !.
get_constraint_attributes(minusf,['-',circle]):- !.
get_constraint_attributes(_,['?',rhombus]).

get_var_occurs([],_,[]):- !.
get_var_occurs([N-Lit|Lits],Var,[N|T]):-
	get_vars_in_lit(Lit,Vars),
	element(Var,Vars), !,
	functor(Lit,Name,_),
	get_var_occurs(Lits,Var,T).
get_var_occurs([_|Lits],Var,T):-
	get_var_occurs(Lits,Var,T).


write_davinci_node([NodeId,Type,Col,GO,Text],Children,EdgeAttributes):-
	tab(4),
	write('l("'), write(NodeId), write('",'), nl,
	tab(8),
	write('n("'), write(Type), write('",'), nl,
	tab(12),
	write('[a("COLOR","'), write(Col), write('"),'), nl,
	tab(12),
	write('a("_GO","'), write(GO), write('"),'), nl,
	tab(12),
	write('a("OBJECT","'), write(Text), write('")],'), nl,
	write_davinci_links(Children,NodeId,EdgeAttributes),
	nl, tab(8), write(')'), nl,
	tab(4),
	write(')').

write_davinci_links([],_,_):-
	!,
	tab(12),
	write('[]').
write_davinci_links(Children,Parent,Attr):-
	tab(12),
	write('['),
	write_davinci_link(Children,Parent,Attr),
	tab(12),
	write(']').

write_davinci_link([],_,_):- !.
write_davinci_link([Child|T],Parent,[Edge,Dir]):-
	write('l("'), write(Parent), write('->'), write(Child), write('",'), nl,
	tab(16),
	write('e("",[a("EDGEPATTERN","'), write(Edge),write('"), '),
	write('a("_DIR","'), write(Dir), write('")],'),
	write('r("'), write(Child), write('")))'),
	(T \= [] -> write(','), nl, tab(12); true),
	write_davinci_link(T,Parent,[Edge,Dir]).

	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Causal ordering of literals in a QM

exists_causal_order((Head:-Body),[H|Order]):-
	number_lits((Head,Body),1,[H|BodyL]),
	exogenous_variables(Head,E),
	state_variables(Head,S),
	conc(E,S,Given),
	causal_order(BodyL,Given,Order).

number_lits((A,B),N,[N-A|T]):-
	!,
	N1 is N + 1,
	number_lits(B,N1,T).
number_lits(true,_,[]):- !.
number_lits(A,N,[N-A]).


causal_order([],_,[]):- !.
causal_order(Lits,Seen,[N-Lit|T]):-
	select_lit(N-Lit,Lits,Rest),
	functor(Lit,Name,Arity),
	(Name/Arity = deriv/2 ->
		causal_order(Rest,Seen,T);
		get_vars_in_lit(Lit,Vars),
		set_diff(Vars,Seen,New,Old),
		length(New,L),
		L =< 1,
		conc(Seen,New,Seen1),
		causal_order(Rest,Seen1,T)).

get_dependencies([],_,[]).
get_dependencies([Var|Vars],Deps,L):-
	mem(Var1/L0,Deps),
	Var == Var1, !,
	get_dependencies(Vars,Deps,L1),
	conc(L0,L1,L).
get_dependencies([Var|Vars],Deps,[Var|L]):-
	get_dependencies(Vars,Deps,L).


select_lit(L,[L|T],T).
select_lit(L,[H|T],[H|T1]):- select_lit(L,T,T1).

get_vars_in_lit(Lit,Vars):-
	functor(Lit,_,Arity),
	get_vars_in_lit(Arity,Lit,Vars0),
	sort(Vars0,Vars).

get_vars_in_lit(0,_,[]):- !.
get_vars_in_lit(N,Lit,[Var|Vars]):-
	arg(N,Lit,Var),
	(var(Var);functor(Var,'$VAR',_)), !,
	N1 is N - 1,
	get_vars_in_lit(N1,Lit,Vars).
get_vars_in_lit(N,Lit,Vars):-
	N1 is N - 1,
	get_vars_in_lit(N1,Lit,Vars).

set_diff([],_,[],[]).
set_diff([Var|Vars],S,S1,[Var|S2]):-
	element(Var,S), !,
	set_diff(Vars,S,S1,S2).
set_diff([Var|Vars],S,[Var|S1],S2):-
	set_diff(Vars,S,S1,S2).

element(X,[Y|_]):- X == Y, !.
element(X,[_|T]):- element(X,T).

mem(X,[X|_]).
mem(X,[_|T]):- mem(X,T).

conc([],L,L).
conc([H|T],L,[H|T1]):- conc(T,L,T1).
