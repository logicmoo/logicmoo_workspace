
display_clauses:-	
		inicount,
		nl,nl,write('Current clauses: '),nl,nl,
		clause( C ), count(N),
		user:sPrint(" % )  %",[N,'$clause'(C)]), 
		nl, fail.
display_clauses.

display_c_rules:-
		c_rule(_,N,I,L,Compcond),
		\+(\+(	(c_rule_to_print(cr(I,L,Compcond),1,_),
			 print('$Nc_rule'(N,(I,L,Compcond)))))),
%		display_c_rule(N,I,L,Compcond),
		fail.
display_c_rules:-!.

% Not needed anymore
%display_c_rule(N,I,L,Compcond):-
%		display_c_rule1(I,L,Compcond,LTerm),
%		print_esp(['    ',N,')'| (LTerm)]),nl,!.

%display_c_rule1(Left,[[[],Right]], _,D):-
%		make_ors(Left,Leftors),
%		D = ['	','(( ',Leftors,' --> ',Right,' ))','cr'],!.
%display_c_rule1(_,[],[],[]):-!.
%display_c_rule1(_,[],[Comp|Cs],D):-
%		make_ors(Comp,Compors),
%		display_c_rule1(_,[],Cs,RTerm),
%		D = ['	', '[ ', Compors , ' ]','cr' | RTerm ],!.
%display_c_rule1(Left,[ [C,Right] | Rest ], Compcond, D ):-
%		make_ors(Left,Leftors),
%		make_ors(C,Cors),
%		display_c_rule1(Left,Rest,Compcond,RTerm),
%		D = ['	','(( ',Leftors,' --> ',Right,' ))','  v  ',
%		'[ ',Cors,' ]','cr'|RTerm],!.

inicount:-
		( retract(counter(_)) ; true ),
		asserta( counter(1) ),!.
count(N):-
		retract( counter(N) ),
		N1 is N + 1,
		asserta( counter(N1) ),!.

%make_ors([T],T):-!.
%make_ors([],'false-bool'):-!.
%make_ors([T|R], T '  v  ' T1):-  !,make_ors(R,T1),!.
%make_ors(T,T):-!.

%------------------------------------------------------------------------------%
% writes terms with prolog variables, enumerating the variables:
% A, B,... in order of appearance.    Has no side effects.
%------------------------------------------------------------------------------%

%print_term(Atom):-atom(Atom),print(Atom),!.
%print_term(Term):- \+(\+(print_term1(Term))),!.
%print_term1(Term):-
%		print_term(Term,0,_),!,
%                print(Term),!.
%
%
%print_term(PVar,N,N1):-
%                var(PVar),
%		PVar = '$VAR'(N),
%		N1 is N + 1.
%print_term(Term,N,N1):- 
%                Term=..[_|LTerm],
%                print_term1(LTerm,N,N1).
%
%print_term1([Term|RTerm],N,N2):- 
%                print_term(Term,N,N1),
%                print_term1(RTerm,N1,N2).
%print_term1([],N,N).
%
%print_esp(LTerm):-
%		\+(\+(print_esp(LTerm,0,_))),!.
%
%print_esp([],N,N).
%print_esp([Atom|RTerm],N,N1):- 
%		atom(Atom),
%		( ( Atom = 'cr', nl ) ; print(term(Atom))),
%		print_esp(RTerm,N,N1),!.
%print_esp([Term|RTerm],N,N2):-
%		print_term(Term,N,N1),
%		print(term(Term)),
%		print_esp(RTerm,N1,N2),!.

c_rule_to_print(PVar,N,N1):-
                var(PVar),
                PVar = '$VAR'(N),
                N1 is N + 1.
c_rule_to_print(Term,N,N1):- 
                Term=..[_|LTerm],
                c_rule_to_print1(LTerm,N,N1).

c_rule_to_print1([Term|RTerm],N,N2):- 
                c_rule_to_print(Term,N,N1),
                c_rule_to_print1(RTerm,N1,N2).
c_rule_to_print1([],N,N).

