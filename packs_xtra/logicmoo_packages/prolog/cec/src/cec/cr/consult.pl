statusDef([(O: rl)|NextDefs]):-
		asserta(status_lexic(rl,O)),
		statusDef(NextDefs).
statusDef([(O: lr)|NextDefs]):-
		asserta(status_lexic(lr,O)),
		statusDef(NextDefs).
statusDef([]).

% -------------------------------------------------------------------------%
% Initialization of the system
% -------------------------------------------------------------------------%

clean_database:-
		retractall( status_lexic(_,_) ),
                retractall( c_rule(_,_,_,_,_) ),
                retractall( po_order(_) ),
                retractall( newrule(_,_,_,_) ),
                retractall( old_cpair(_) ),
                retractall( clause(_) ),
                retractall( c_rule_num(_) ),
                asserta( c_rule_num(1) ),!.

% -------------------------------------------------------------------------%
% Other clauses
% -------------------------------------------------------------------------%

transf( C , T1 , T2 ):-
		nonvar(T1),
		T1 =.. [F|_],
		boolean_sort(F),!,
		simple_cnf_to_list(( -(C) \/ -(T1) \/ T2 )  /\ ( -(C) \/ T1 \/ -(T2)), L ),
		\+ L = ['true-bool'],
		make_eqs(L),!.
transf( C , T1 , T2 ):-
		simple_cnf_to_list(-(C),L),
		\+ L = ['true-bool'],
		make_eqs(L,T1,T2).

%------------------------------------------------------------------------------%
% Replaces any boolean term by its simplified cnf list
%------------------------------------------------------------------------------%

simple_cnf_to_list( T1,L2 ):-
		cnf(T1,T2),
		flat(T2,T3),!,
		to_list_form(T3,L1),
		simple_cnf_list( L1,L2 ),!.

%------------------------------------------------------------------------------%
% Replaces any boolean term (not in list form) by its cnf
%------------------------------------------------------------------------------%
cnf(T1,T2):-    cnf1(T1,T3),
                cnf2(T3,T2),!.

cnf1( -(-T), T):- !.
cnf1( -(T1\/T2), T3 /\ T4):- 
                cnf1(-T1,T3), 
                cnf1(-T2,T4),!.
cnf1( -(T1/\T2), T3 \/ T4):- 
                cnf1(-T1,T3), 
                cnf1(-T2,T4),!.
cnf1( T1\/T2, T3 \/ T4):- 
                cnf1(T1,T3), 
                cnf1(T2,T4),!.
cnf1( T1/\T2, T3 /\ T4):- 
                cnf1(T1,T3), 
                cnf1(T2,T4),!.
cnf1( T,T ):-!.

cnf2( (T1/\T2)\/T3, (NT1 \/ NT3) /\ (NT2 \/ NT3) ):-
                cnf2(T1,NT1),
                cnf2(T2,NT2),
                cnf2(T3,NT3),!.
cnf2( T3\/(T1/\T2), (NT3 \/ NT1) /\ (NT3 \/ NT2) ):-
                cnf2(T1,NT1),
                cnf2(T2,NT2),
                cnf2(T3,NT3),!.
cnf2( T,T ):-!.

%------------------------------------------------------------------------------%

flat(T,T):-	atom(T),!.
flat(T,T):-	var(T),!.
flat(T1,T2):-
		T1 =.. [F|Args1],
		do_flat( F,Args1,Args2 ),
		T2 =.. [F|Args2],!.

do_flat( _, [], [] ):-!.
do_flat( F, [T1|Rest1], L2):-
		ac_op(F),
		nonvar(T1),
		T1 =.. [F|_],!,
		flat( T1,T2 ),
		T2 =.. [F|Args],
		do_flat( F, Rest1, Rest2 ),
		vappend( Args, Rest2, L2 ),!.
do_flat( F, [T1|Rest1], [T2|Rest2] ):-
		flat( T1,T2 ),
		do_flat( F, Rest1, Rest2 ),!.

ac_op( F ):-	ac_pair( F,_ ),!.
ac_op( F ):-	ac_pair( _,F ),!.
ac_pair(/\,\/).

%------------------------------------------------------------------------------%
% Puts flattened cnf terms in list form: a\/ -b\/c /\ d\/e --> [[e,d],[c,-b,a]]
%------------------------------------------------------------------------------%

to_list_form(T,L):-
		nonvar(T),
		T =.. [/\|Lors],
		ors_to_list(Lors,L),!.
to_list_form(Or,L):-
		ors_to_list([Or],L),!.

ors_to_list([Or],[Args]):-
		nonvar(Or),
		Or =.. [\/|Args],!.
ors_to_list([T],[[T]]):-!.
ors_to_list([Or|R],[Args|Rargs]):-
		nonvar(Or),
		Or =.. [\/|Args],!,
		ors_to_list(R,Rargs),!.
ors_to_list([T|R],[[T]|Rargs]):-
		ors_to_list(R,Rargs),!.

%------------------------------------------------------------------------------%

make_eqs([]):-!.
make_eqs([L|Rest]):-
                assertz(clause(L)),
		make_eqs(Rest),!.

make_eqs([],_,_):-!.
make_eqs([L|Rest],T1,T2):-
                assertz(clause([ (T1 = T2) | L ]) ),
		make_eqs(Rest,T1,T2),!.

consmember([PVar,NVar],[[PVar,NVar]|R],[[PVar,NVar]|R]).
consmember(P,[X|L1],[X|L2]):-consmember(P,L1,L2).
consmember([PVar,NVar],[],[[PVar,NVar]]).


treat_condition([T1,T2|LT], C /\ LC ):-
		treat_condition( T1 , C ),
		treat_condition( [T2|LT] , LC ),!.
treat_condition([T1 = T2],C) :-
		treat_condition(T1 = T2,C), !.
treat_condition(A = 'true-bool', A ):-!.
treat_condition(A = 'false-bool', -(A) ):-!.
treat_condition('true-bool' = A, A ):-!.
treat_condition('false-bool' = A, -(A) ):-!.
treat_condition(T1 = T2, ( -(T1) /\ -(T2) ) \/ ( T1 /\ T2 ) ):-
		T1=..[O,_,_],
		boolean_sort(O),!.


makePrologTerm1([Term|RTerm],LVar,[Term1|RTerm1],LVar2):- 
		makePrologTerm(Term,Term1,LVar,LVar1),
                makePrologTerm1(RTerm,LVar1,RTerm1,LVar2).
makePrologTerm1([],L,[],L).


makePrologTerm(Const,Const,LVar,LVar):-const(Const).
makePrologTerm(PNVar,PVar,LVar,LVar1):-
		atomic(PNVar),
		consmember([PNVar,PVar],LVar,LVar1).
makePrologTerm(Term,Term1,LVar,LVar1):- 
		Term=..[F|LTerm],
		makePrologTerm1(LTerm,LVar,LTerm1,LVar1),
		Term1=..[F|LTerm1].

%------------------------------------------------------------------------------%
%	Auxiliary clauses
%------------------------------------------------------------------------------%
vdelete(X,[Y|L],L):- var(X),X = Y.
vdelete(X,[Y|L],L):- X == Y.
vdelete(X,[Y|L],[Y|L1]):- 
		vdelete(X,L,L1).

vappend([],L,L):-!.
vappend([X|R],L,[X|R1]):- 
		vappend(R,L,R1),!.

vmember(X,[Y|_]):- var(X), X = Y.
vmember(X,[Y|_]):- X == Y.
vmember(X,[_|L]):-
		vmember(X,L).

