/*
 *	file:		portray.pl
 *	version:	1.0
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains predicates to pretty-print terms and equations.
 *
 *	history:
 *	891010	js	Added this comment
 *	891208	uh	Changed portraying of 
 *			'ofType'
 *			'$op_decl'
 *			to allow pretty-printing of the disambiguated
 *			order-sorted operators
 *	900320	uh	Changed portraying of orderSortedOrdering
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */

portray(T):-
	var(T),
	!,
	fail.
portray('$nonoperational equation'):-
	write('nonoperational equation').
portray('$rule') :-
	write('rule').
portray('$equation') :-
	write('equation').
portray('$term'(T)):-
%	manyToOrderSorted(T,TO),
	toOrderSortedHalf1(T,TO),
	print(TO).
portray('$osTerm'(T)):-
	toOrderSortedHalf1(T,T0),
	toOrderSortedHalf2(T0,T1),
	print(T1).
portray('$tupel'((Index,Object,Attributes),Ref,Type)) :-
	TypedObj=..[Type,Index,Object],
	print(TypedObj),
	cont1(show,As),
	(nonvar(As),As=[] ->
		true
	;
		nl,
		write('   reference: '),
		write(Ref),
		nl,
		write('   attributes:'),
		nl,
		mapP(lambda([(N,V)],( member(N,As) ->
					print('$attribute'(Type,Ref,N,V))
				    ;
					true
				    )
			   ),
		     Attributes
		)
	).
portray('$manysortedVariants'([])):-
	!.
portray('$manysortedVariants'(Ts)):-
	sPrint("plus redundant axioms (from the order-sorted viewpoint):
%.",['$list'(Ts,' ')]).

portray('$attribute'(_,_,reduceClause,V)) :-
	sPrint("       %= %",[reduceClause,'$list'(V,'
')]),
	nl.
portray('$attribute'(_,_,N,V)) :-
	sPrint("       %= %",[N,V]),
	nl.

portray('$list'(V,A)):-
	writeList(V,A).
portray('$list'([],_A,_L,_R)):-
	!.
portray('$list'(V,A,L,R,N)):-
	append(V1,V2,V),
	length(V1,N),
	print('$list'(V1,A,L,R)),
	nl,
	print('$list'(V2,A,L,R,N)).
portray('$list'(V,A,L,R,_N)):-
	portray('$list'(V,A,L,R)).
portray('$list'(V,A,L,R)):-
	print(L),
	writeList(V,A),
	print(R).
portray('$string'(S)):-
	name(N,S),
	write(N).
portray('$writeq'(T)):-
	writeq(T).
portray('$dec3'(I)):-
	writeDec(I,3,' ').
portray('$dec'(I,N,C)):-
	writeDec(I,N,C).
portray((C,[L=R])):-
	list(C),
	mapP(lambda([E],functor(E,'=',2)),C),
	portray('$equation'(C,[(L=R)])).
portray((L=R)):-
	portray('$equation'([],[(L=R)])).
portray((CAnd => L=R)):-
	andsToList(CAnd,C),
	print('$equation'(C,[L=R])).
portray('$equation'(I,(C,D))) :-
	integer(I),
	list(C),
	sPrint("%    %",['$dec3'(I),'$equation'(C,D)]).
portray('$equation'(C,[L=R])) :-
	list(C),
	vars((C,L,R),V),
	equationDispayed:==true,
	mapF(lambda([X,'$var'(X)],true),V,VV),
	(cont(os,true) ->
		sPrint("%%%",['$list'(VV,',','',' |- '),
			      '$condition'(C),'$singleEq'(L,R)])
	;	sPrint("%%",['$condition'(C),'$singleEq'(L,R)])).
portray('$equation'((C,[(L=R)]))) :-
	portray('$equation'(C,[(L=R)])).
portray('$nonoperational equation'(I,(C,D))) :-
	list(C),
	integer(I),
	sPrint("%    %",['$dec3'(I),'$nonoperational equation'(C,D)]).
portray('$nonoperational equation'(C,[L=R])) :-
	print('$equation'(C,[L=R])).
portray('$rule'(I,(C,D))) :-
	integer(I),
	list(C),
	(auxRule(I,'$rule',(_,none)) ->
		Asterix=' '
	;	Asterix='*'
	),
	sPrint("%%   %",['$dec3'(I),Asterix,'$rule'(C,D)]).
portray('$rule'(C,[(L=R)])) :-
	portray('$equation'(C,[(L=R)])).
portray('$condition'([])).
portray('$condition'(Es)) :-
	sPrint("% => ",['$conjunction'(Es)]).
portray('$Nc_rule'(N,(A,B,C))) :-
	format('~t~d~4|~s',[N," )"]),
	print('$c_rule'(A,B,C)).
portray('$c_rule'(L1,[[Cond,R1]|ORs],Compl)) :-
	\+(\+(printCRule(Cond,L1,R1))),
	nl,
	print('$c_rule'(L1,ORs,Compl)).
portray('$c_rule'(_,[],[])).
portray('$c_rule'(_,[],Compl)) :-
	format('~8|~s~p',["complement ",'$complement'(Compl)]).
portray('$complement'([[L1|Cl1]|Cls])) :-
	!,
	format('~20|~p~n',['$clause'([L1|Cl1])]),
	print('$complement'(Cls)).
portray('$complement'([])).
portray('$complement'(Cl)) :-
	format('~20|~p~n',['$clause'(Cl)]).
portray('$clause'('true-bool')) :-
	print('$term'('true-bool')).
portray('$clause'([])) :-
	write(false).
portray('$clause'(Es)) :-
	\+(\+(printClause(Es,' V '))).
portray('$clause_conjunction'('true-bool')) :-
	print('$term'('true-bool')).
portray('$clause_conjunction'([])) :-
	write(false).
portray('$clause_conjunction'(Es)) :-
	\+(\+(printClause(Es,' /\ '))).
portray('$literal'(T1 = T2)) :-
	print(T1 = T2), !.
portray('$literal'(- T1)) :-
	print('$term'(- T1)), !.
portray('$literal'(T1)) :-
	print('$term'(T1)).
portray('$disjunction'([])) :-
	write(false).
portray('$disjunction'(Es)) :-
	writeList(Es,' or ').
portray('$conjunction'([])).
portray('$conjunction'([L=R,E|Es])) :-
	sPrint("% and %",['$singleEq'(L,R),'$conjunction'([E|Es])]).
portray('$conjunction'([Eq])) :-
	portray('$singleEq'(Eq)).
portray('$singleEq'(L,R)) :-
	(R=='$void'; R== '$true-$pred' ->
		portray('$term'(L))
	;	sPrint("% = %",['$term'(L),'$term'(R)])).
portray('$singleEq'((L=R))) :-
	print('$singleEq'(L,R)).
portray('$singleEq'(T)) :-
	portray('$term'(T)).
portray('$singleRule'(L,R)) :-
	print('$singleEq'(L,R)).
portray('$oref'('$rule',reflexivity)):-
	print('$singleRule'((x=x),'$true-$pred')).
portray('$oref'(Type,Id)) :-
	(number(Id) ->
		I=Id
	;
		R=Id
	),
	Fact=..[Type,I,_],
	clause(Fact,true,R),
	print(Fact).
portray('$shortType'(rule)):-
	write('R').
portray('$shortType'(equation)):-
	write('E').
portray('$shortType'('nonoperational equation')):-
	write('N').
portray((X ofType (_Z:[C|A]))) :-
	makeOp(X,[C|A],Y),		% changed uh 08.12.89: X is order-sorted
	sPrint("%	: %.",['$op_decl'(Y),'$op_signature'([C|A])]).
portray('$op_signature'([CO])):-
	!,
	print(CO).
portray('$op_signature'([CO|D])):-
	sPrint("(% -> %)",['$domain'(D),CO]).
portray('$domain'([])).
portray('$domain'([X])):-
	!,
	print(X).
portray('$domain'([X|Xs])):-
	sPrint("% * %",[X,'$domain'(Xs)]).
portray('$op_decl'(O/N)):-	% changed O -> O/N uh 08.12.89
	(hhConstructor(O/N) ->	% changed O -> O/N uh 08.12.89
		sPrint("cons%",['$op_notation'(O)])
	;	sPrint("op%",['$op_notation'(O)])
	).
portray('$op_notation'(O)):-
	(notation(O,P,N) ->
		sPrint("(%,%,%)",[O,P,N])
	;	sPrint(" %",[O])
	).
portray(@V) :-
	print(V).
portray(((C1,C2),[C3])):-
	nonvar(C1),
	nonvar(C2),
	nonvar(C3),
	(C1='$infty',C11=C1;C1=[C11]),
	(C2='$infty',C22=C2;C2=[C22]),
	sPrint("[%|%|%]",['$term'(C11),'$term'(C22),'$term'(C3)]).
portray(R):-
	nonvar(R),
	R=..[O|L],
	auxOpForComplexities(O),
	map(lambda([X,'$term'(X)],true),L,LT),
	write(O),
	print('$list'(LT,',','(',')')).
portray('$complexity'(L,ms)) :-
	!,
	sPrint("{%}",['$list'(L,',')]).
portray('$complexity'(L,S)) :-
	applyPermutation(L,S,L1),
	print(L1).
portray(date(D,M,Y,H,Mi)):-
	sPrint("% %, % at %:%",[M,D,Y,H,'$dec'(Mi,2,0)]).
portray('$maySuperpose'(_Ch,I,_A,'$nonoperational equation',J,_L,_T)):-
	sPrint("R%xN%",[I,J]).
portray('$maySuperpose'(_Ch,I,_A,'$rule',J,_L,_T)):-
	sPrint("R%xR%",[I,J]).
portray('$superpositionOcc'(0,left)):-
	!,
	write('the left side').
portray('$superpositionOcc'(0,right)):-
	!,
	write('the right side').
portray('$superpositionOcc'(I,A)):-
	var(A),
	!,
	sPrint("condition %",[I]).
portray('$superpositionOcc'(I,left)):-
	sPrint("the left side of condition %",[I]).
portray('$superpositionOcc'(I,right)):-
	sPrint("the right side of condition %",[I]).
portray('$assoc'([])).
portray('$assoc'([(A,B)])):-
	sPrint("% into %",[A,B]).
portray('$assoc'([(A,B),X|Xs])):-
	sPrint("% into %, %",[A,B,'$assoc'([X|Xs])]).
portray('$subst'([],_)).
portray('$subst'([(A,B)],Kind)):-
	Term=..[Kind,B],
	sPrint("%/%",[A,Term]).
portray('$subst'([(A,B),X|Xs],Kind)):-
	Term=..[Kind,B],
	sPrint("%/%, %",[A,Term,'$subst'([X|Xs],Kind)]).
portray('$applying'(Kind,R)):-
	(Kind == os ->
		R = rule(I,(C,[(LS=RS)])),
		toOrderSortedHalf1(LS,LS0),
		toOrderSortedHalf2(LS0,LS1),
		toOrderSortedHalf1(RS,RS0),
		toOrderSortedHalf2(RS0,RS1),
		(LS1 = RS1 ->
			true
		;
			integer(I),
			(auxRule(I,rule,(_,none)) ->
				Asterix=' '
			;	Asterix='*'
			),
			toOrderSortedHalf1(C,C0),
			toOrderSortedHalf2(C0,C1),
			sPrint("Applying %%   %% = %",['$dec3'(I),Asterix,'$condition'(C1),LS1,RS1]),
			nl
		)
	;
		sPrint("Applying %.",[R]),
		nl
	).
portray('$proofStep'(Depth,resolution(C,[E],Compl,Proofs))):-
	mkAtom('%%',[Depth,'  '],Depth1),
	sPrint("%% resolved to %	
%with complexity
%	%%",
		[Depth,'$singleEq'(E),'$conjunction'(C),Depth,Depth,Compl,'$proofs'(Depth1,Proofs)]).
portray('$proofStep'(Depth,subsumed(C,[E],T,J,Compl))):-
	sPrint("%%
%is subsumed by % %	
%with complexity
%	%",
		[Depth,'$equation'(C,[E]),Depth,T,J,Depth,Depth,Compl]).
portray('$proofStep'(Depth,reduction(G,GR,P))):-
	sPrint("%% contextually reduced to %
%%",
		[Depth,'$singleEq'(G),'$singleEq'(GR),Depth,'$proofStep'(Depth,P)]).
portray('$proofStep'(Depth,hypothesis(E))):-
	sPrint("%% by hypothesis",
		[Depth,'$singleEq'(E)]).
portray('$proofStep'(Depth,reflexivity(E))):-
	sPrint("%% by unification",
		[Depth,'$singleEq'(E)]).
portray('$proofs'(D,[P|Ps])):-
	nl,
	print('$proofStep'(D,P)),
	print('$proofs'(D,Ps)).
portray('$proofs'(_,[])).

% Portray-clauses for pretty-printing narrowing solutions
portray('$solution'([])) :-
        write('{}').
portray('$solution'([(A,B)|Subst])) :-
        write('{'), print('$subst'([(A,B)|Subst],'$term')), write('}').
portray('$osSolution'([])) :-
        write('{}').
portray('$osSolution'([(A,B)|Subst])) :-
        write('{'), print('$subst'([(A,B)|Subst],'$osTerm')), write('}').
portray('$var'(X)):-
	opPrefix(X,N,T),
	sPrint("%:%",[N,T]).
portray('$varlist'([])).
portray('$varlist'([X])) :-
        sPrint("%",[@X]).
portray('$varlist'([X1|XL])) :-
        sPrint("%, %",[@X1,'$varlist'(XL)]).
% Portray clauses for pretty-printing let-expressions
portray('$letterm'(let Defs in Exp)) :-
        !, sPrint("let % in %",['$definitions'(Defs),'$letterm'(Exp)]).
portray('$letterm'(Term)) :-
        print('$term'(Term)).
portray('$definitions'([(T1 = T2),Def|Defs])) :-
	!, sPrint("% and %",['$definition'((T1 = T2)),'$definitions'([Def|Defs])]).
portray('$definitions'([(T1 = T2)])) :-
	sPrint("% = %",['$term'(T1),'$letterm'(T2)]).
portray('$definitions'((T1 = T2) and Defs)) :-
	!, sPrint("% and %",['$definition'((T1 = T2)), '$definitions'(Defs)]).
portray('$definitions'(T1 = T2)) :-
	sPrint("% = %",['$term'(T1),'$letterm'(T2)]).
portray('$definition'(T1 = T2)) :-
	sPrint("% = %",['$term'(T1),'$letterm'(T2)]).


% for polynomial interpretations
portray('$$interpretation_with_vars'(I,V)) :-
	pol_write_interpretation_with_vars(I,V).
portray([(q(A,B),M)|R]):-
	pol_write_polynomial([(q(A,B),M)|R]).

portray(poly(X)):-
	var(X),
	write('poly(N)').
portray('q2.0'):-
	write('Quintus-Prolog 2.x').
portray((A:-B)):-
	portray_clause((A:-B)).

portray(orderSortedOrdering(Comment,EqType,X1,X2,C,Ext,M)):-
	sPrint("orderSortedOrdering(%,%,%,%,%,%,%)",
		[Comment,EqType,'$term'(X1),'$term'(X2),C,Ext,M]).
portray(multiSetOrdering(Comment,X1,X2,C,Ext,M)):-
	sPrint("multiSetOrdering(%,%,%,%,%,%)",
		[Comment,'$term'(X1),'$term'(X2),C,Ext,M]).
portray(extendedMultiSetOrdering(Comment,X1,X2,Ext,M)):-
	sPrint("extendedMultiSetOrdering(%,%,%,%,%)",
		[Comment,'$term'(X1),'$term'(X2),Ext,M]).
portray(multiSetDiff(S1,S2,T1)):-
	sPrint("multiSetDiff(%,%,%)",['$term'(S1),'$term'(S2),'$term'(T1)]).
% added uh 29.11.89
portray('$opName'(F)) :-
    printOpName(F).







portray(T) :-
	pretty(T). % for pretty printing rules associated with specifications


% added uh 29.11.89
%printOpName(F) :-
%    name('/',Slash),
%    name(F,FChars),
%    append(OpNameChars,RestChars,FChars),
%    append(Slash,NumberChars,RestChars),
%    name(N,NumberChars),
%    number(N),
%    name(O,OpNameChars),
%    (differentArities(O) ->
%	sPrint("'%'/%",[O,N])
%    ;
%	sPrint("'%'",[O])
%    ),
%    !.
%printOpName(F) :-  
%    write(''''),
%    write(F),
%    write('''').

printOpName(F/X) :-
	var(X),
	!,
	write(''''),
	write(F),
	write('''').
printOpName(F/N) :-
	(differentArities(F) ->
	    sPrint("'%'/%",[F,N])
	;
	    sPrint("'%'",[F])
	),
	!.
printOpName(F) :-
	write(''''),
	write(F),
	write('''').
	

andsToList(L=R and CAnd,[L=R|C]):-
	!,
	andsToList(CAnd,C).
andsToList(L=R,[L=R]).
