%------------------------------------------------------------------------------%
%
%	CLAUSAL COMPLETION FOR QUINTUS PROLOG
%
% Input:
%
%    Initial clauses: clause([l1,...,ln]).
%    Boolean ops are: 'v'  (or),  '-' (not),  true,  false.
%    Other operators of boolean sort (e.g. '<') must be 
%          declared: boolean_sort(<).
%    Variables are Prolog variables
%    Operator precedence: greater_po( [ [fi,fj]...]  )
%
% Internal representation:
%
%    clauses:      clause([l1,...,ln]).
%    c-rules:  c-rule( old/new, num, Left, [[Cond,Right]..], [CCond..] )
%    LHS's l1 v ... v ln, with n>1, are expressed by lists [l1,...,ln]
%
% -----------------------------------------------------------------------------%


kb:-		statistics(runtime,_),
		display_clauses,nl,
		complete1,!,
		statistics(runtime,[_,T]),
		nl,format('time spent: ~3d sec. ~n', [T] ),nl.

translate_to_cr :-
		cr:clean_database,
		translate, fail.
translate_to_cr :-
                statistics(runtime,[_,Time]),
                nl,format('time spent: ~3d sec. ~n', [Time] ),nl.


translate :-
        get_status(L),
	cr:statusDef(L).
translate :-
	get_greater(L),
 	assert(po_order(L)).
translate :-
	user:hhConstructor(O),
	assert(const(O)).
translate :-
	user:prologVariant(_,'$equation',((C,[A1 = B1]),_)),
	translate_clause((C,[A1 = B1])).

translate_clause((C,[A1 = B1])) :-
	(C = [] ->
		transf('true-bool', A1 , B1)
	;
		treat_condition(C,C2),
		transf(C2 , A1 , B1)
	), 
	!.
translate_clause(_) :-
	write('  "There is no translation to contextual equations for this
		  specification (equality for non-boolean sorts is needed)." ' ),
	cr_clean_database,
	abort.

get_greater(L) :-
	bagof([A, B],(user:kns_gt(A,B), user:(_ ofType (A:_))),L), !.
get_greater([]).

get_status(L) :- 
	bagof((I : B),(user:(A ofType (I:_)),user:tps_status(A,B),atomic(B)),L), !.
get_status([]).

boolean_sort(O) :- 	
	user:(_ ofType (O:[bool|_])).

%------------------------------------------------------------------------------%
% The completion loop
%------------------------------------------------------------------------------%

complete1:-	clause(_),
		treat_clause,
		complete1,!.
complete1:-	c_rule(new,_,_,_,_),
		nl,
		compute_c_pairs,
		complete1,!.
complete1:-	nl,nl,write('Completion successfully terminated.  '),nl,nl,
		write('Final system of c-rules:'),nl,nl,
		display_c_rules,nl,!.

% -----------------------------------------------------------------------------%
% Treatment of clauses: reduction, orientation and creation of c-rules
% -----------------------------------------------------------------------------%

treat_clause:-
		retract( clause(C) ),
		write('  Reducing the clause:  '),
		print('$clause'(C)), nl,
		nf_set( C , NFS ),
		ini_newrule_num,
		new_rules(NFS),!.


new_rules(NFS):-
		vmember(C,NFS),
		orientation( C, [( Left --> Right ) | Rest ] ),
		newrule_num(N),
		assertz( newrule( N,Rest,Left,Right ) ),
		subsume_c_rule(Left),fail.
new_rules(_):-
		create_c_rules,!.

ini_newrule_num:-
		retractall(newrule_ctr(_)),
		assert(newrule_ctr(0)),!.

newrule_num(N):-
		retract(newrule_ctr(N)),
		N1 is N + 1,
		assert(newrule_ctr(N1)),!.

% -----------------------------------------------------------------------------%
% Computing critical pairs between two c-rules.
% -----------------------------------------------------------------------------%

compute_c_pairs:-
		pair_c_rules(Left1,Left2,R1,R2),
		is_subterm(Strm,Left1,NLeft,Subt),
		nonvar(Strm),
		unify( Strm,Left2 ),
		vmember( Eq1, R1 ), Eq1 = [C1,Right1], 
		vmember( Eq2, R2 ), Eq2 = [C2,Right2], 
		compute_clauses(Right1,Right2,C1,C2,NLeft,Subt),
		fail.
compute_c_pairs:-!.


%1: convergent. gives:    (false = false) v ...
%2: convergent. gives:    (true  = true ) v ...
%3: critical pair between (l1 v l2 v ...) --> true and any L2.
%4: critical pair between (l1 --> false ) v ... and (l2 --> true) v ...
%5: critical pair between (l1 --> true  ) v ... and (l  --> r ) v ...(non-bool)
%6: critical pair between (l1 --> false ) v ... and (l  --> r ) v ...(non-bool)
%7: critical pair between (l1 --> r1    ) v ... and (l2 --> r2) v ...(non-bools)

% 1:
compute_clauses(R1, R2,_,_,_,_):- R1 == 'false-bool', R2 == 'false-bool', !.
% 2:
compute_clauses(R1, R2, _,_,NLeft,_):-
		R1 == 'true-bool', R2 == 'true-bool',
		(var(NLeft) ; \+ (NLeft = [_|_])),!.
% 3:
compute_clauses(_,Right2,C1,C2,NLeft,Subt):-
		nonvar(NLeft),
		NLeft = [_|_],
		Subt = Right2,
		vappend(C1,C2,C3),
		vappend(C3,NLeft,C4),
		norm_bool(C4,Clause),!,
		Clause \== ['true-bool'],
		write('	'),print('$clause'(Clause)), nl,
		assertz( clause(Clause) ),!.
% 4:
compute_clauses(R1, R2,C1,C2,_,_):-
		( (R1 == 'false-bool', R2 == 'true-bool') ; (R1 == 'true-bool', R2 == 'false-bool') ),
		vappend(C1,C2,C3),
		norm_bool(C3,Clause),!,
		Clause \== ['true-bool'],
		write('	'),print('$clause'(Clause)), nl,
		assertz( clause(Clause) ),!.
% 5:
compute_clauses(R1,Right2,C1,C2,NLeft,Subt):-
		R1 == 'true-bool',
		Subt = Right2,
		vappend(C1,C2,C3),
		norm_bool([NLeft|C3],Clause),!,
		Clause \== ['true-bool'],
		write('	'),print('$clause'(Clause)), nl,
		assertz( clause(Clause) ),!.
% 6:
compute_clauses(R1,Right2,C1,C2,NLeft,Subt):-
		R1 == 'false-bool',
		Subt = Right2,
		vappend(C1,C2,C3),
		norm_bool([-NLeft|C3],Clause),!,
		Clause \== ['true-bool'],
		write('	'),print('$clause'(Clause)), nl,
		assertz( clause(Clause) ),!.
% 7:
compute_clauses(Right1,Right2,C1,C2,NLeft,Subt):-
		Subt = Right2,
		vappend(C1,C2,C3),
		norm_bool([(NLeft = Right1)|C3],Clause),!,
		Clause \== ['true-bool'],
		write('	'),print('$clause'(Clause)), nl,
		assertz( clause(Clause) ),!.


pair_c_rules(Left1,Left2,CR1,CR2):-
		retract( c_rule(new, N, Left1, CR1, C)),
		assertz( c_rule(old, N, Left1, CR1, C)),!,
		write('  Computing critical pairs with c-rule '), 
%		make_ors(Left1, Leftors),
%		print_esp([' ',N,' ):  ',Leftors,' --> ' , '...']),nl,nl,
		user:make_list(Left1,Left3),
		user:sPrint(" % ) % --> ...",[N,'$clause'(Left3)]),nl,nl,
		superpose_with_bools(Left1,CR1),!,
		c_rule(_, _, Left2, CR2, _), 
		resolution(Left1,CR1,Left2,CR2),
		(var(Left2) ; \+(Left2 = [_|_])) .


% only with the rule:  x v x --> x

superpose_with_bools(Left1,CR):-
		vdelete(Lit1,Left1,Left2),
		vmember(Lit2,Left2),
		unify( Lit1,Lit2 ),
		vmember( Eq, CR ),
		Eq = [C,_],
		vappend(Left2,C,C1),
		norm_bool(C1,Clause),
		Clause \== ['true-bool'],
		write('  superp. with bools:'),
		print( '$clause'(Clause) ), nl,
		assertz( clause(Clause) ),fail.
superpose_with_bools(_,_):-!.

%------------------------------------------------------------------------------%
% Resolution overlappings between two c-rules with several maximal literals.
%------------------------------------------------------------------------------%

resolution(Left1,_,Left2,_):-
		variant(Left1,Left2).
resolution(Left1,CR1,Left2,CR2):-
		nonvar(Left2),
		Left2 = [_|_],
		vdelete( Lit, Left1, NLeft1 ),
		Lit = -X,
		vdelete( CLit, Left2, NLeft2 ),
		\+ ( CLit = -(_) ),
		unify( X,CLit ),
		vappend(NLeft1,NLeft2,Left),
		vmember( Eq1, CR1 ), 	Eq1 = [C1,_], 
		vmember( Eq2, CR2 ), 	Eq2 = [C2,_], 
		vappend(C1,C2,C3),
		vappend(Left,C3,C4),
		norm_bool(C4,Clause),
		Clause \== ['true-bool'],
		user:sPrint("  Resolution: %",['$clause'(Clause)]), nl,
		assertz( clause(Clause) ),fail.
resolution(_,_,_,_):-!.

is_subterm(_,[],_,_):-!,fail.
is_subterm(Subt, [T1|Rest], [T2|Rest], VSubt ):-
		is_subterm(Subt,T1,T2,VSubt).
is_subterm(Subt, [T|Rest1], [T|Rest2], VSubt ):-
		is_subterm(Subt,Rest1,Rest2,VSubt).
is_subterm(T,T, VSubt, VSubt):-
		(var(T) ; \+ (T = [_|_])).
is_subterm(Subt,T1,T2, VSubt):-
		nonvar(T1),
		\+ (T1 = [_|_]),
		T1  =.. [ F|Args  ], 
		is_subterm(Subt, Args, NArgs,VSubt),
		T2 =.. [ F|NArgs ].


%------------------------------------------------------------------------------%
% Keeping the system of c_rules fully interreduced
%------------------------------------------------------------------------------%

subsume_c_rule(Left1):-
		c_rule(_,N,Left2,_,_),
		treat_c_rule(Left1,N,Left2),fail.
subsume_c_rule(_):-!.

treat_c_rule(Left1,N,Left2):-
		variant(Left1,Left2),
		retract(c_rule(_,N,Left2,CR,_)),
		write('    c-rule no. '),
		write(N),write(' modified.'),nl,
		retract(newrule_ctr(N1)),
		more_newrules(N1,_,Left2,CR),!.
treat_c_rule(Left1,N,Left2):-
		subterm_match(Left1,Left2,_,_),!,
		retract(c_rule(_,N,Left2,CR,_)),
		user:sPrint("    c-rule no. % simplified in LHS.",[N]), nl,
		c_rule_to_clauses(Left2,CR),!.

more_newrules(_,N,[]):- assert(newrule_ctr(N)),!.
more_newrules(N,N2,Left,[[C,Right]|Rest]):-
		asserta(newrule(N,C,Left,Right)),
		N1 is N + 1,
		more_newrules(N1,N2,Left,Rest),!.

c_rule_to_clauses(_,[]):-!.
c_rule_to_clauses(Left,[[C,Right]|Rest]):-
		assert_clause( Left, Right, C ),
		c_rule_to_clauses(Left,Rest),!.

assert_clause([L|Rest],Right,C):-
		Right == 'true-bool',
		vappend([L|Rest],C,Clause),
		assertz(clause(Clause)),!.
assert_clause(Left,Right,C):-
		Right == 'true-bool',
		assertz(clause([Left|C])),!.
assert_clause(-Left,Right,C):-
		Right == 'false-bool',
		assertz(clause([Left|C])),!.
assert_clause(Left,Right,C):-
		Right == 'false-bool',
		assertz(clause([-Left|C])),!.
assert_clause(Left, Right, C ):-
		assertz(clause([ (Left = Right) |C])),!.


% -----------------------------------------------------------------------------%
% Creation of new c-rules:
% -----------------------------------------------------------------------------%

create_c_rules:-
		newrule(_,_,Left,_),
		create_c_rule(Left),fail.
create_c_rules:-!.

create_c_rule(Left):-
		collect_set(Left,L,C),
		complem_cond(C,Comp),		% file: cnf
		retract( c_rule_num(N1) ),
		assertz( c_rule(new,N1,Left,L,Comp)),
		N2 is N1 + 1,
		asserta( c_rule_num(N2) ), write('  New c-rule: '),nl,
		\+(\+(	(c_rule_to_print(cr(Left,L,Comp),1,_),
			 print('$Nc_rule'(N1,(Left,L,Comp)))))),
		!.

collect_set(Left,CR,Disj):-
		newrule(_,_,Left1,Right),
		variant(Left,Left1),
		Left = Left1,
		get_all_simple(C1,D1,Left,Right),
		collect_set(Left,C2,D2),
		vappend(C1,C2,CR),
		vappend(D1,D2,Disj),!.
collect_set(_,[],[]):-!.

get_all_simple(C1,D1,Left,Right):-
		get_all(Left,Right,D2),
		simple_cnf_list(D2,D1),
		put_rights(Right,D1,C1),!.

get_all(Left,Right,[D|D1]):-
		newrule(N,D,Left1,Right),
		variant(Left,Left1),
		Left = Left1,
		retract(newrule(N,D,Left1,Right)),
		get_all(Left,Right,D1),!.
get_all(_,_,[]):-!.

put_rights(_,[],[]):-!.
put_rights(Right,[C|Rest1],[[C,Right]|Rest2]):-
		put_rights(Right,Rest1,Rest2),!.


% -------------------------------------------------------------------------%
% Orientation of clauses 
% -------------------------------------------------------------------------%

orientation( [(T1 = T2)| C1], [(L1 --> R1) | C1 ]):-
		user:map(lambda([Lit,Eq],
		    Lit1^(Lit= -(Lit1) ->
			Eq = (Lit1 = 'false-bool')
		    ;	Eq = (Lit = 'true-bool'))), C1,CEqs),
		reductive((CEqs,[T1=T2]),(_,[L1=R1]),M),
		!,
		(M=oriented(_) ; M=hasBecomeReductive).
orientation( C1, [(M1 --> TTFF) | C2 ]):-
		max_lits(C1,ML,C2),
		(
		(ML = -T, M1 = T, !, TTFF = 'false-bool',!);
		(ML = T, M1 = ML, TTFF = 'true-bool')),!.


reductive(E1,E2,M):-
    user:fromProlog(E1,[],E11,S),
    user:reductive(E11,E21,M),
    user:toProlog(E21,S,E2,_),
    !.


% Not needed anymore
%reductivity_test(Cond,Left):-
%  		greater_rpo_multi([Left],Cond),!.
%reductivity_test(Cond,Left):-
%  		write(' Non-reductive clause: '),nl,
%		make_ors(Cond,Condors),
%		make_ors(Left,Leftors),
%		print_esp(['	','(( ',Leftors,' --> ','...',' ))','  v  ',
%			     '[ ', Condors,' ]' ]), abort.
%
%orientation1( T1 = T2, (T1 --> T2) ):-
%		greater_rpo(T1, T2),!.
%orientation1( T1 = T2, (T2 --> T1) ):-
%		greater_rpo(T2, T1),!.
%orientation1( T1 = T2, _ ):-
%		nl,write(' The clause '),print(T1 = T2),
%		write(' is RPO-unorientable.'), abort, !.

max_lits( L1, M, L2 ):-
		vdelete(M,L1,L2),
		varFreeLiterals([M|L2],[M1|L21]),
		user:multiSetOrdering(("Trying to check whether % is a maximal literal of %",['$literal'(M1),'$clause'([M1|L21])]),[M1],L21,
			choices([],[]),_,oriented(_)),
		!.
max_lits( [A1,A2], [A1,A2], [] ):-
    !.
max_lits( L1, ML, L2 ):-
		repeat,
		write('  There is no unique maximal literal. Given: '), nl,
		user:sPrint("  %",['$clause'(L1)]),nl,
		write('  Choose a subset of maximal literals to become the new LHS by typing a list.'),nl,
		read(L),
		extract(L1,L,ML,L2,1),
	        varFreeLiterals(L1,L1VarFree),
		extract(L1VarFree,L,MLVarFree,L2VarFree,1),
		user:multiSetOrdering(("Verifying maximality of % in %",['$literal'(MLVarFree),'$clause'(L2VarFree)]),MLVarFree,L2VarFree,
			choices([],[]),_,M),
		(M=oriented(_) ->
		    true
		;   nl,
		    write('Not maximal. Please, try again.'),
		    nl,
		    fail),
		!.


varFreeLiterals(L,VarFree):-
		user:map(lambda([Lit,Lit1],
		    (Lit= -(Lit1) ->
			true
		    ;	Lit = Lit1)), L,L1),
		user:fromProlog(L1,[],VarFree,_),
		!.






extract([],[],[],[],_):-!.
extract([],_,[],[],_):-!,fail.
extract([X|L1],[N|R],[X|ML],L2,N):-
		N1 is N + 1,
		extract(L1,R,ML,L2,N1),!.
extract([X|L1],R,ML,[X|L2],N):-
		N1 is N + 1,
		extract(L1,R,ML,L2,N1),!.

%_______________________________________________________________________________
