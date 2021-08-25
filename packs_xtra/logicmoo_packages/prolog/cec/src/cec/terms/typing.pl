/*
 *	file:		typing.pl
 *	version:	1.0
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		Ullrich Hustadt
 *
 *	description:
 *	This file contains the predicates for syntax- and type-
 *	checking terms, and converting terms from external into
 *	internal representation.
 *
 *	history:
 *	891010	js	Added this comment
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */


syntaxAndTypeCheck(T,T1) :-
	(varsOf(T,[]) ->
		normalizeEquation(T,TN),
		typeCheckAxiom(TN,TT),
		typedVars(TT,T1)
	;	prologVarInInput(T)
	).
syntaxAndTypeCheck(T,T) :-
	nl,
	write('*** syntax error in : '),
	write(T),
	(error :== true).

normalizeEquation(([C|Cs]=>E),([C|Cs]->EN)) :-
	normDisj(E,EN).
normalizeEquation((C=>E),(CN->EN)) :-
	normConj(C,CN),
	normDisj(E,EN).
normalizeEquation((L=R),([]->L=R)).
normalizeEquation((or E),([]->or EN)) :-
	normDisj((or E),(or EN)).
normalizeEquation(A,A).


normDisj((or L),(or LN)) :-
	map(asList,L,LN),
	!.
normDisj(X,X).


normConj([E|Es],[E|Es]) :-
	!.
normConj((E and Es),EsN) :-
	!,
	normConj(E,EN),
	normConj(Es,Es1),
	append(EN,Es1,EsN).
normConj(E,[E]).

typeCheckAxiom((C->L=R),(C1->L1=R1)) :-
	(cont(vars,Env) ->
		true
	;
		Env=[]
	),
	length([L=R|C],NmbEqs),
	genVarList(NmbEqs,TypeVars),
	genList(NmbEqs,'$bool',BoolType),
	pairing(BoolType,TypeVars,LitTypes),
	getTypedVars([L=R|C],LocalEnv,[]),
	append(LocalEnv,Env,Env1),
	typesA([L=R|C],Env1,[L1=R1|C1],_,LitTypes),
	!.
typeCheckAxiom((C->L=R),(C->L=R)) :-
	error("can't associate a type with 
		 %.",['$equation'(C,[L=R])],typeCheck).


typeCheckEq((L='$void'),Env,(LM='$void'),Env1,TM) :-
	!,
	typeA(L,Env,L1,Env1,T),
	maxInComponent(T,TM),
	injection((T,L1),(TM,LM)),
	!.
typeCheckEq((L = R),Env,(L1 = R1),Env1,T) :-
	typeA(L,Env,L11,EnvL1,T1),
	typeA(R,EnvL1,R11,Env1,T2),
	(	nonvar(T1),
		nonvar(T2)
	->	lub(T1,T2,T), % a lub exists, if any connected
		% component has a maximal element. Taking some such lub
		% of T1 and T2 is sufficient. Any
		% variant of the equation for any other common
		% supersort T' of T1 and T2 will be generated
		% by the injectivity axioms
		injection((T1,L11),(T,L1)),
		injection((T2,R11),(T,R1))
	;	T1=T2,
		T=T1,
		L1=L11,
		R1=R11
	),
	!.

getTypedVars(X:T,Env,Env) :-
	atom(X),
	member((X:T),Env),
	!.
getTypedVars(X:T,[(X:T)|Env],Env) :-
	atom(X),
	\+member((X:T),Env),
	!.
getTypedVars(X:T,Env,Env) :-
	atom(X),
	member((X:T1),Env),
	!,
	error("Redeclaration of variable %:% with different type %.",[X,T1,T],typeCheck),
	fail.
getTypedVars(Term,Env1,Env) :-
	Term=..[_|Terms],
	iter(getTypedVars,Env,Terms,[],Env1),
	!.



typeA(Eq,Env,(L1=R1),Env1,T) :-
	(nonvar(T) ->
		T = ('$bool',T1),
		!,
		(Eq = (_ = _) ->
			typeCheckEq(Eq,Env,(L1=R1),Env1,T1)
		 	;
		 	error("can't associate a type with: 
			%.",[Eq],typeCheck),
			!,
			fail
		)
	).
typeA(X:T,Env,'$var'(X,T),Env,T) :-
	atom(X),
	member((X:T),Env),
	!.
typeA(X:T,Env,'$var'(X,T),[(X:T)|Env],T) :-
	atom(X),
	\+member((X:T),Env),
	!.
%typeA(X:T,Env,_,_,_) :-
%	atom(X),
%	member((X:T1),Env),
%	!,
%	error("Redeclaration of variable %:% with different type %.",[X,T1,T],typeCheck),
%	fail.
typeA(X,Env,'$var'(X,T),Env,T) :-
	variable(X),
	member((X:T),Env),
	!.
typeA(X,Env,'$var'(X,T),[(X:T)|Env],T) :-
	variable(X),
	not member((X:_),Env),
	(subsort(_,_) ->
		error("Symbol % undeclared.",[X],typeCheck)
	;	true
	).
typeA((L=R),_,(_=_),_,_) :-
  	error("expecting a term not an equation: 
		%.",[(L=R)],typeCheck),
	!,
	fail.
typeA(Term,Env,Term1,Env1,Type) :-
	Term=..[O|Terms],
	typesA(Terms,Env,Terms1,Env1,Types),
	!,
	minimalOp(O,[Type|Types],O1,[Type|Types1]),
	pairing(Types,Terms1,TT1),
	pairing(Types1,Terms2,TT2),
	map(injection,TT1,TT2),
	Term1=..[O1|Terms2].


typesA([],Env,[],Env,[]).
typesA([T|Terms],Env,[T1|Terms1],Env1,[Ty1|Types]) :-
	typeA(T,Env,T1,EnvT,Ty1),
	typesA(Terms,EnvT,Terms1,Env1,Types).


variable(X) :-
	atom(X),
	\+ (X ofType (_:[_])).


prologVarInInput(X) :-
	error("Prolog variables not allowed in specification elements: 
	%",[X],in).

injectionT(_,T,[],T).
injectionT(S,T,[S1|P],T1) :-
	injection((S,T),(S1,TS1)),
	injectionT(S1,TS1,P,T1),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
internalTermRep(term(T),T) :-
	!.
internalTermRep(T,TT) :-
	error:==none,
	(varsOf(T,[]) ->
		expandTerm(T,TE)
	;
		TE = T
	),
	!,
	syntaxAndTypeCheck((TE='$void'),([]->TT= _)),
	cont1(error,none).

internalEqRep(Eq,_) :-
	varsOf(Eq,L),
	L \== [],
	!,
	error("Prolog variables not allowed:
		%.",[Eq]),
	cont1(error,none).
internalEqRep('$equation'(C,[L=R]),(C,[L=R])) :-
	!.
internalEqRep((C=>L=R),(CT,[LT=RT])) :-
	error:==none,
	expandTerm((C=>L=R),(CE=>LE=RE)),
	syntaxAndTypeCheck((CE=>LE=RE),(CT->LT=RT)),
	cont1(error,none),
	!.
internalEqRep((L=R),([],[LT=RT])) :-
	error:==none,
	expandTerm((L=R),(LE=RE)),
	syntaxAndTypeCheck((LE=RE),([]->LT=RT)),
	cont1(error,none),
	!.
internalEqRep(Eq,_) :-
	error("Goal not in syntactical correct form:
		%.",[Eq],syntaxCheck),
	cont1(error,none).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% dead code!
typeAssoc(Term,T,Type) :-
	typeA(Term,[],T,_,Type),
	!.
typeAssoc(Term,Term,Type) :-
	(error:==yes),
	write('Can''t associate '),
	(var(Type) ->

		write('a type with ')
	;
		write('type '),
		write(Type),
		write(' with ')
	),
	write(Term),
	nl.


/*----------------------------------------------------------------------*/
/* internalTermRep1(+Term,-ITerm,+RedRel)				*/
/* computes the internal representation of Term, if Term is not an	*/
/* let-term and is not statically typeable. So it can be necessary to	*/
/* do some rewriting using the reduction relation RedRel to reduce 	*/
/* argument terms to reach an well-formed term				*/

internalTermRep1('$term'(T),T,_):-
	!.
internalTermRep1(T,TT,RedRel):-
	error:==none,
	(varsOf(T,[]) ->
		expandTerm(T,TE)
	;
		TE = T
	),
	!,
	syntaxAndTypeCheck1((TE='$void'),([]->TT= _),RedRel),
	cont1(error,none).


/*----------------------------------------------------------------------*/
/* syntaxAndTypeCheck1(+Term1,-Term2,RedRel)				*/
/* makes syntax checks and typechecks Term1 getting Term2 		*/

syntaxAndTypeCheck1(T,T1,RedRel) :-
	(varsOf(T,[])->
		normalizeEquation(T,TN),
		typeCheckAxiom1(TN,TT,RedRel),
                typedVars(TT,T1)
	;	prologVarInInput(T)
	).
syntaxAndTypeCheck1(T,T,_) :-
	nl,
	write('*** syntax error in : '),
	write(T),
	(error:==true).


/*----------------------------------------------------------------------*/
/* typeCheckAxiom1(+CondEq1,-CondEq2,+RedRel)				*/
/* typechecks a conditional equation CondEq1 getting CondEq2		*/

typeCheckAxiom1((C->L=R),(C1->L1=R1),RedRel) :-
	(cont(vars,Env)-> true;Env=[]),
	length([L=R|C],NmbEqs),
        genVarList(NmbEqs,TypeVars),
	genList(NmbEqs,'$bool',BoolType),
        pairing(BoolType,TypeVars,LitTypes),
	getTypedVars([L=R|C],Env1,Env),
	typesA1([L=R|C],Env1,[L1=R1|C1],_,LitTypes,RedRel),
	!.
typeCheckAxiom1((C->L=R),(C->L=R),_) :-
	error("can't associate a type with 
		 %.",['$equation'(C,[L=R])],typeCheck).


/*----------------------------------------------------------------------*/
/* typeCheckEq1(+Eq1,+Env1,-Eq2,-Env2,+RedRel)				*/
/* typechecks an equation Eq1 in environment Env1 getting Eq2 and a new	*/
/* environment Env2. 							*/
/* If typecheckEq1 is used to typecheck a single term (the 1rst clause) */
/* injections into a maximal sort will be added to the term.		*/
/* If typecheckEq1 is used to typecheck an equation with two term (the  */
/* second clause), it tried to find a common sort for the two terms and */
/* to find injections for the terms into such a sort.			*/

typeCheckEq1((L='$void'),Env,(LM='$void'),Env1,TM,RedRel) :-
	!,
	typeA1(L,Env,L1,Env1,T,RedRel),
	maxInComponent(T,TM),
	injection((T,L1),(TM,LM)),
	!.
typeCheckEq1((L=R),Env,(L1=R1),Env1,T,RedRel) :-
	typeA1(L,Env,L11,EnvL1,T1,RedRel),
	typeA1(R,EnvL1,R11,Env1,T2,RedRel),
	(	nonvar(T1),
		nonvar(T2)
	->	lub(T1,T2,T), % a lub exists, if any connected
		% component has a maximal element. Taking some such lub
		% of T1 and T2 is sufficient. Any
		% variant of the equation for any other common
		% supersort T' of T1 and T2 will be generated
		% by the injectivity axioms
		injection((T1,L11),(T,L1)),
		injection((T2,R11),(T,R1))
	;	T1=T2,
		T=T1,
		L1=L11,
		R1=R11
	),
	!.


/*----------------------------------------------------------------------*/
/* typesA1(+Terms1,+Env1,-Terms2,-Env2,-Types2,+RedRel)			*/
/* typechecks each term of Terms1 in environment Env1 getting Terms2    */
/* having types Types and a new environment Env2.			*/

typesA1([],Env,[],Env,[],_RedRel).
typesA1([T|Terms],Env,[T1|Terms1],Env1,[Ty1|Types],RedRel) :-
	typeA1(T,Env,T1,EnvT,Ty1,RedRel),
	typesA1(Terms,EnvT,Terms1,Env1,Types,RedRel).


/*----------------------------------------------------------------------*/
/* typeA1(+Term1,+Env1,-Term2,-Env2,-Type2,+RedRel)			*/
/* typechecks Term1 in environment Env1 getting Term2 having Type2 and  */
/* a new environment Env2						*/

typeA1(Eq,Env,(L1=R1),Env1,T,RedRel) :-
	(nonvar(T) ->
		T = ('$bool',T1),
		!,
		(Eq = (_ = _) ->
			typeCheckEq1(Eq,Env,(L1=R1),Env1,T1,RedRel)
		 	;
		 	error("can't associate a type with: 
			%.",[Eq],typeCheck),
			!,
			fail
		)
	).
typeA1(X:T,Env,'$var'(X,T),Env,T,_) :-
	atom(X),
	member((X:T),Env),
	!.
typeA1(X:T,Env,'$var'(X,T),[(X:T)|Env],T,_) :-
	atom(X),
	\+member((X:T),Env),
	!.
%typeA1(X:T,Env,_,_,_,_) :-
%	atom(X),
%	member((X:T1),Env),
%	!,
%	error("Redeclaration of variable %:% with different type %.",[X,T1,T],typeCheck),
%	fail.
typeA1(X,Env,'$var'(X,T),Env,T,_) :-
	variable(X),
	member((X:T),Env),
	!.
typeA1(X,Env,'$var'(X,T),[(X:T)|Env],T,_) :-
	variable(X),
	not member((X:_),Env),
	(subsort(_,_) ->
		(error("Symbol % undeclared.",[X],typeCheck), !, fail)
	;	true
	).
typeA1((L=R),_,(_=_),_,_,_) :-
  	error("expecting a term not an equation: 
		%.",[(L=R)],typeCheck),
	!,
	fail.
typeA1(Term,Env,Term1,Env1,Type,RedRel) :-
	Term=..[O|Terms],
	typesA1(Terms,Env,Terms1,Env1,Types,RedRel),
	!,
	minimalOp1(O,Terms1,Env1,[Type|Types],
                   O1,Terms2,ArgumentTypes,[Type|Types1],RedRel),
	pairing(ArgumentTypes,Terms2,TT1),
	pairing(Types1,Terms3,TT2),
	map(injection,TT1,TT2),
	Term1=..[O1|Terms3].


/*----------------------------------------------------------------------*/
/* minimalOp1(+OP1,+Terms1,+Env1,+Types1,				*/
/*	      -OP2,-Terms2,-Types2,-OperatorTypes,+RedRel)		*/
/* looks for a declaration for OP1 with arity greater or equal to 	*/
/* Types1. If there is no such operator, the argument terms will be 	*/
/* reduced using RedRel, and the types of the normalforms are computed. */
/* If there is again no operator declaration for OP1, there is no       */
/* possibility to make the term well-formed. Otherwise OP2 is the       */
/* internal representation for OP1, Terms2 are equal to Terms1 or equal */
/* to the normalforms of Terms1, Types2 is the arity of OP1 and 	*/
/* OperatorTypes the coarity.						*/

minimalOp1(O,Terms1,Env1,Types1,
           O2,Terms3,ArgumentTypes,OperatorTypes,RedRel):-
	((\+ operatorAbove(O,Types1)) ->
           (( (map(RedRel,Terms1,Terms2),
               get_types(Terms2,Terms3,NewTypes,Env1)) ->
                  (operatorAbove(O,[Type1|NewTypes]) ->
                     ((O ofType (O2:OperatorTypes)),
                      map(subsortRT,[NTy|NewTypes],OperatorTypes),
                      not opBetween(O,[NTy|NewTypes],OperatorTypes),
                      ArgumentTypes = NewTypes)
                     ;
                     (error("no declaration for '% : %'.",[O,'$op_signature'([Type1|NewTypes])],
                            typeCheck), fail)
                  )
                  ;
                  (error("no declaration for '% : %'.",[O,'$op_signature'(Types1)],
                         typeCheck), fail)
            )
           )
           ;
           ((O ofType (O2:OperatorTypes)),
            map(subsortRT,Types1,OperatorTypes),
            not opBetween(O,Types1,OperatorTypes),
            [_TYPE|ArgumentTypes] = Types1,
            Terms3 = Terms1)
        ).


/*----------------------------------------------------------------------*/
/* put_away_injections(+Term1,-Term2)					*/
/* removes all injections on the top of Term1 yielding Term2		*/

put_away_injections(T1,T3) :-
   T1 =.. [OP,T2],
   injection(OP), !,
   put_away_injections(T2,T3).
put_away_injections(T1,T1) :- !.

/*----------------------------------------------------------------------*/
/* get_type(+Term,-Type,+Env)						*/
/* looks for the type Type of an typed term Term in environment Env	*/

get_type(@X,Type,Env) :-
   !, member((X:Type),Env).
get_type(T1,Type,_Env) :-
   functor(T1,OP,_),
   (_ ofType (OP:[Type|_])), !.


/*----------------------------------------------------------------------*/
/* get_types(+TermList1,-TermList2,-TypeList2,+Env)			*/
/* removes the injections on the top of all terms in TermList1 getting  */
/* TermList2 and computes the types of all terms in TermList2 getting   */
/* TypeList2 in environment Env						*/

get_types([],[],[],_Env) :- !.
get_types([T1|TL1],[T2|TL2],[TY1|TYS1],Env) :-
   put_away_injections(T1,T2),
   get_type(T2,TY1,Env),
   get_types(TL1,TL2,TYS1,Env).

/*----------------------------------------------------------------------*/
/* internalLetExpRep(+LetTerm,-ILetTerm)				*/
/* computes the internal representation ILetTerm for some let-		*/
/* expression LetTerm							*/

internalLetExpRep(Term,PTerm) :-
   internalLetExpRep(Term,PTerm,_).

internalLetExpRep(Term,PTerm,Type1) :-
   error:==none,
   (varsOf(Term,[]) ->
       (flattenLetTerm(Term,FTerm),
        internalLetTermRep(FTerm,ITerm1,Type1),
	cont1(error,none),
        correctLetTerm(ITerm1,[]),
        neededDefinitions(ITerm1,ITerm2,_),
        toPrologLetTerm(ITerm2,[],PTerm))
        ;
       prologVarInInput(Term)
   ), !,
   cont1(error,none).



/*----------------------------------------------------------------------*/
/* flattenTerm(+LetTerm1,-LetTerm2)					*/
/* transformes the definitions of LetTerm1 into lists of definitions    */
/* and expands terms.							*/

flattenLetTerm(let Defs in Exp,let DefList in FlatExp) :-
	!, 
	flattenDefs(Defs,DefList),
	flattenLetTerm(Exp,FlatExp).
flattenLetTerm(Term1,Term2) :-
	expandTerm(Term1,Term2).

flattenDefs(Defs1 and Defs2,DefList) :-
	!, 
	flattenDefs(Defs1,DefList1),
	flattenDefs(Defs2,DefList2),
	append(DefList1,DefList2,DefList).
flattenDefs((CT1 = T1),[(CT2 = T2)]) :-
	expandTerm(CT1,CT2),
	flattenLetTerm(T1,T2).

/*----------------------------------------------------------------------*/
/* internalLetTermRep(+LetTerm,-ILetTerm)                               */
/* typechecks LetTerm and computes the many-sorted representation of it */

internalLetTermRep(LetTerm1,ILetTerm1) :-
   internalLetTermRep(LetTerm1,ILetTerm1,_).

internalLetTermRep(LetTerm1,ILetTerm1,Type) :-
   (cont(vars,Env) -> true;Env=[]),
   convertLetTerm(LetTerm1,Env,ILetTerm,_,Type),
   typedVars(ILetTerm,ILetTerm1),
   cont1(error,none).
internalLetTermRep(LetTerm1,_,_) :-
   error("can't associate a type with 
          % ",['$letterm'(LetTerm1)],typecheck).


convertLetTerm(let Defs1 in Exp1,Env1,let Defs2 in Exp2,Env3,Type) :-
   !, convertDefs(Defs1,Env1,Defs2,Env2),
   convertLetTerm(Exp1,Env2,Exp2,Env3,Type).
convertLetTerm(T1,Env1,IT1,Env2,Type) :-
   typeCheckEq((T1 = '$void'),Env1,(IT1 = '$void'),Env2,Type).



convertDefs([],Env1,[],Env1) :- !.
convertDefs([(CT = T)|Defs1],Env1,[Def2|Defs2],Env4) :-
   typeA(CT,Env1,ICT,Env2,Type1),
   convertDefRhs(T,Env2,IT,Env3,(ICT,Type1),InjICT),
   Def2 = (InjICT = IT), !,
   convertDefs(Defs1,Env3,Defs2,Env4).


convertDefRhs(let Defs1 in Exp1,Env1,
              let Defs2 in InjIT2,Env3, (IT1,Type1),InjIT1) :-
   !, 
   convertDefs(Defs1,Env1,Defs2,Env2),
   convertExp(Exp1,Env2,IT2,Env3,Type2),
   ((nonvar(Type1),nonvar(Type2)) ->	
        (lub(Type1,Type2,Type),
	 injection((Type1,IT1),(Type,InjIT1)),
	 injection((Type2,IT2),(Type,InjIT2)))
	;	
        (Type1 = Type2,
         InjIT1 = IT1,
         InjIT2 = IT2)
   ),!.
convertDefRhs(T2,Env1,IT2,Env2,(IT1,Type1),InjIT1) :-
   !, 
   convertExp(T2,Env1,IT2,Env2,Type2),
   ((nonvar(Type1),nonvar(Type2)) ->	
        (lub(Type1,Type2,Type),
	 injection((Type1,IT1),(Type,InjIT1)),
	 injection((Type2,IT2),(Type,InjIT2)))
	;	
        (Type1 = Type2,
         InjIT1 = IT1,
         InjIT2 = IT2)
   ),!.


convertExp(let Defs1 in Exp1,Env1,let Defs2 in Exp2,Env3,Type) :-
   !, 
   convertDefs(Defs1,Env1,Defs2,Env2),
   convertExp(Exp1,Env2,Exp2,Env3,Type) .
convertExp(T1,Env1,IT,Env2,Type) :-
   typeA(T1,Env1,IT,Env2,Type).




/*----------------------------------------------------------------------*/
/* correctLetTerm(+LetTerm,+BoundVars)					*/
/* checks if LetTerm is an correct formed let term, i.e. 		*/
/* - the lefthand-side of a definition is a constructor term		*/
/* - there is at least one variable on the lefthand-side of a           */
/*   definition								*/
/* - there should be no common variables on the lefthand-sides of two   */
/*   parallel definitions						*/
/* - no variable on the righthand-side of a definition should occurr    */
/*   on the lefthand-side of a parallel definition			*/

correctLetTerm(let Defs in Exp,BoundVars1) :-
   !,
   correctDefs(Defs,BoundVars1,[],BoundVars2),
   scope_vars(BoundVars1,BoundVars2,BoundVars),
   correctLetTerm(Exp,BoundVars) .
correctLetTerm(_,_) .

correctDefs([],_BoundVars,LhsVars,LhsVars) :- !.
correctDefs([(T1 = T2)|Defs],BoundVars,LhsVars1,LhsVars3) :-
   checkForConstrTerm((T1 = T2)),
   checkForSomeDefinedVariable((T1 = T2)),
   checkDoubleDefinition((T1 = T2),LhsVars1),
   checkUsingVars((T1 = T2),BoundVars,LhsVars1),
   vars(T1,LVars), append(LVars,LhsVars1,LhsVars2),
   correctDefs(Defs,BoundVars,LhsVars2,LhsVars3), !.


/*----------------------------------------------------------------------*/
/* checkForConstrTerm(+Definition)					*/
/* checks if the lefthand-side of Definition is a constructor term 	*/

checkForConstrTerm((T1 = T2)) :-
   (constructorTerm(T1) ->
       true
       ;
       (error("Lefthand side of definition 
         % is not a constructor term",['$definition'(T1 = T2)],definitionCheck))
   ).


/*----------------------------------------------------------------------*/
/* constructorTerm(+Term)						*/
/* succeeds if Term is a constructor term				*/

constructorTerm(X) :-
   var(X), !.
constructorTerm(@_) :- !.
constructorTerm(T) :-
   functor(T,O,N),
   (hhConstructor(O); hhConstructor(O/N); injection(O)),
   mapA(constructorTerm,T,N),
   !.


/*----------------------------------------------------------------------*/
/* checkForSomeDefinedVariable(+Definition)				*/
/* checks if there is at least one variable on the lefthand-side of the */
/* definition								*/

checkForSomeDefinedVariable((T1 = T2)) :-
   ((vars(T1,X1), X1 == []) ->
       (error("No variables in definition % ",['$definition'(T1 = T2)],
        definitionCheck))
       ;
       true
   ).


/*----------------------------------------------------------------------*/
/* checkDoubleDefinition(+Definition,+LhsVars) 				*/
/* checks that every variable occurrs only once on the lefthand-side of */
/* a list of parallel definitions 					*/

checkDoubleDefinition((T1 = T2),LhsVars) :-
   ((vars(T1,LVars), intersection_s(LVars,LhsVars,IS)) ->
        (IS == [] ->
            true
            ;
            (IS = [X] ->
               (error("Definition % binds variable % a second time.",
                ['$definition'(T1 = T2),'$term'(@X)],definitioncheck))
               ;
               (error("Definition % binds variables % a second time.",
                ['$definition'(T1 = T2),'$varlist'(IS)],definitioncheck))
            )
        )
        ;
        true
   ).


/*----------------------------------------------------------------------*/
/* checkUsingVars(+Definition,+BoundVars,+LhsVars)			*/
/* checks if the variables on the righthand-side of a definition are    */
/* not defined in a parallel definition (this is not really an error,   */
/* but should be avoided, because  otherwise some  additional rule is   */
/* needed to define which variable is meant in the let-expression)      */

checkUsingVars((T1 = T2),BoundVars,LhsVars) :-
   ((vars(T2,RVars), intersection_s(RVars,LhsVars,IS)) ->
        (IS == [] ->
            true
            ;
            (subset(IS,BoundVars) ->
                true
                ;
                ((diff(IS,BoundVars,NB), (NB = [X])) ->
                   (error("Definition % uses variable % 
                    which is not defined yet.",
                    ['$definition'(T1 = T2),'$term'(@X)],definitioncheck))
                   ;
                   (error("Definition % binds variables % 
                    which are not defined yet.",
                    ['$definition'(T1 = T2),'$varlist'(NB)],definitioncheck))
                )
            )
        )
        ;
        true
   ).

/*----------------------------------------------------------------------*/
/* neededDefinitions(+LetTerm1,-LetTerm2,+NeededVars)			*/
/* removes all definitions of LetTerm1 which are certainly not needed   */
/* to evaluate the whole expression					*/
/* If LetTerm1 is (Let Defs1 in Exp1) then the set of variables needed  */
/* to evaluate Exp1 is determined.  Then all definitions in Defs1 will  */
/* be removed which do not bind at least one of those needed variables. */

neededDefinitions(let Defs1 in Exp1,Term,NeededVars) :-
	!, neededDefinitions(Exp1,Exp2,Vars),
	removeUselessDefs(Defs1,Vars,Defs2,(NotBound,NewNeeded)),
	append(NotBound,NewNeeded,NeededVars),
        (Defs2 == [] ->
            (Term = Exp2)
            ;
            (Term = (let Defs2 in Exp2))
        ).
neededDefinitions(Term,Term,NeededVars) :-
	vars(Term,NeededVars).
   
removeUselessDefs([],Vars,[],(Vars,[])) :- !.
removeUselessDefs([(CT = _T)|Defs1],Vars,Defs2,(NotBound,NewNeeded)) :-
	vars(CT,VCT), 
	intersection_s(Vars,VCT,[]), !,
	removeUselessDefs(Defs1,Vars,Defs2,(NotBound,NewNeeded)).
removeUselessDefs([(CT = T)|Defs1],Vars,
                  [(CT = T)|Defs2],(NotBound,NewNeeded)) :-
	vars(T,Vars1),
	vars(CT,NewBound),
	diff(Vars,NewBound,NotBoundHere),
	removeUselessDefs(Defs1,NotBoundHere,Defs2,(NotBound,Vars2)),
	append(Vars1,Vars2,NewNeeded).


/*----------------------------------------------------------------------*/
/* toPrologLetTerm(+LetTerm1,+Subst,-LetTerm2)				*/
/* changes all variables on the lefthand-sides of definitions to 	*/
/* PROLOG-variables and makes consistent replacements in all other 	*/
/* terms.								*/

toPrologLetTerm(let Defs in Exp,SEval1,let PDefs in PExp) :-
   !, toPrologDefs(Defs,SEval1,[],PDefs,SNew1),
   scope_subst(SEval1,SNew1,SEval2),
   toPrologLetTerm(Exp,SEval2,PExp).
toPrologLetTerm(Term,SEval1,PTerm) :-
   subst(Term,SEval1,PTerm).

toPrologDefs([],_SEval,SNew1,[],SNew1) :- !.
toPrologDefs([(CT = T1)|Defs],SEval,SNew1,
             [(PCT = PT1)|PDefs],SNew3) :-
   !, toPrologLetTerm(T1,SEval,PT1),
   toProlog(CT,[],PCT,S2),
   append(S2,SNew1,SNew2),
   toPrologDefs(Defs,SEval,SNew2,PDefs,SNew3).

/*----------------------------------------------------------------------*/
/* scope_subst(+Outer,+Inner,-Binding)                                  */
/* computes from the outer binding Outer and the new bindings of the    */
/* definition the bindings Binding for evaluation of the next           */
/* expression                                                           */

scope_subst([],SInner,SInner) :- !.
scope_subst([(X,_PX)|S1],SInner,S2) :-
	member((X,_),SInner),!,
	scope_subst(S1,SInner,S2).
scope_subst([(X,PX)|S1],SInner,[(X,PX)|S2]) :-
	scope_subst(S1,SInner,S2).


/*----------------------------------------------------------------------*/
/* scope_env(+Outer,+Inner,-Binding)					*/
/* equivalent to scope_subst, but working with environments             */

scope_env([],Env,Env) :- !.
scope_env([(X:_T)|Env1],Env2,Env3) :-
	member((X:_),Env2), !,
        scope_env(Env1,Env2,Env3).
scope_env([(X:T)|Env1],Env2,[(X:T)|Env3]) :-
	scope_env(Env1,Env2,Env3).


/*----------------------------------------------------------------------*/
/* scope_vars(+Outer,+Inner,-Binding)					*/
/* equivalent to scope_env, but working with lists of variables		*/

scope_vars([],VL2,VL2) :- !.
scope_vars([X|VL1],VL2,VL3) :-
	member(X,VL2), !,
        scope_vars(VL1,VL2,VL3).
scope_vars([X|VL1],VL2,[X|VL3]) :-
	scope_vars(VL1,VL2,VL3).

/* dead code 

checkForCycle((T1 = T2),RhsVars) :-
   vars(T1,LVars), vars(T2,RVars), 
   intersection_s(LVars,RVars,IS1), !,
   ((IS1 = [],
     intersection_s(LVars,RhsVars,IS2), !,
     ((IS2 = [])
      ;
      (IS2 = [X] ->
          (error("Defined variable % in definition %
                  allready occurred in some defining term before",
                 ['$term'(@X),'$definition'(T1 = T2)],definitionCheck))
          ;
          (error("Defined variables % in definition %
                  allready occurred in some defining term before",
                 ['$varlist'(IS2),'$definition'(T1 = T2)],definitionCheck))
      )
     )
    )
    ;
    (IS1 = [X] ->
        (error("Defined variable % occurrs in 
               defining term % .",
               ['$term'(@X),'$letterm'(T2)],definitionCheck))
        ;
        (error("Defined variables % occurr in 
                defining term % .",
                ['$varlist'(LVars),'$letterm'(T2)],definitionCheck))
    )
   ).

*/

