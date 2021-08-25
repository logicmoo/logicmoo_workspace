/*
 *	file:		interface.pl
 *	version:	1.0
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains the interface to the termination proof system
 *	(TPS) of CEC.
 *
 *	history:
 *	891010	js	Added this comment
 *	900215	uh	Changed definition of
 *			tps_ordering
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */

% interface to the termination proof system  (TPS)
%                  -           -     -
%
% A) ordering predicates
%
%    1. tps_ordering(->Comment,->L,->R,->ChoicesUponFailure,?Extension,<-Mode).
%
%       succeeds,if L > R with respect to the current termination ordering.
%       For user information the string in Comment will be displayed
%       if user interaction is needed.
%	Extension=no disables extension of precedences and interpretations.
%	Otherwise, Extension must be uninstantiated.
%
%    2. tps_ordering(->Comment,->L,->R,<-NL,<-NR,->ChoicesUponFailure,?Extension,<-Mode).
%
%
% B) modification and enrichment/extension of the current ordering
%
%    1. tps_init
%
%       initializes the current termination ordering. Hereafter old
%       termination proofs are no longer valid.
%       Rules must be turned back into equations!
%
%    2. tps_greater(->[[a, b, c ...],[g, h, i ...], ...]).
%
%       Meaningful only for proof methods "kns" and "neqkns".
%       Declaration of ordered operators; fails if the extension
%       is inconsistent. Meaning: a > b > c and g > h > i ...
%
%    3. tps_equal(->[[a, b, c ...],[g, h, i ...], ...]).
%
%       Meaningful only for proof methods "kns".
%       Declaration of equivalent operators; fails if the extension
%       is inconsistent. Meaning: a = b = c and g = h = i ...
%
%    4. tps_status(->OP,->Status).
%
%       Meaningful only for proof methods "kns" and "neqkns".
%       Declaration of operator status; fails if the extension
%       is inconsistent. Possible Stati are
%               "l" left-to-right,
%               "r" right-to-left,
%               "m" multiset.
%
%    5. tps_constructors(->[a,b,c,...]).
%
%       Meaningful only for proof methods "kns" and "neqkns".
%       Declaration of constructors; fails if the extension
%       is inconsistent.
%
%    6. tps_interpretation(Ichanges)
%
%	interpretation displays in the case of poly(N) as current termination
%	ordering all operator interpretations and asks the user if he wants to
%	change any.
%	Ichanges is the list of operators, whose interpretations has been
%	changed.
%
%    7. tps_setInterpretation(-> Op(@x,@y,@z),-> 2 * (@x^2) + 3 * @y,Changed).
%
%       Meaningful only for the proof method "poly(N)".
%	Changed == yes ->
%		Redefinition of polynomial operator interpretations.
%       	Hereafter old termination proofs are no longer valid.
%       	Rules must be turned back into equations!
%	Changed == no
%		Addition of polynomial operator interpretations.
%
%    8. tps_declac(X).
%
%    9. tps_declc(X).
%
%
% C) displaying the current ordering
%
%    1. tps_operators(Mode)
%
%       prints the current ordering definition in a user readable form.
%	Mode == all -> prints all definitions
%	Mode == os  -> prints all definitions for os-Operators
%	Mode == ms  -> prints all definitions for ms-Operators
%
%
% D) combination of termination orderings
%
%    1. tps_combine(->System1,->System2,<-System3).
%
%
% E) Renaming of operators in a termination ordering
%
%    1. tps_rename(->[(old1,new1),(old2,new2),...]).
%
%       succeeds, if the renaming is consistently possible.
%
%
% F) changing the current termination proof technique
%
%    1. tps_order
%
%       change the ordering for termination proofs.
%
%
% E) the state of the termination proof system
%
%    1. tps_getCurrentTPS(<-State).
%
%    2. tps_makeCurrentTPS(->State).
%
%
% The import list of the module ordering:
%
%

tps_ordering(Comment,L,R,_,Ext,Mode) :-
	tps_current_ordering(kns_or_neqkns),
	kns_ordering(Comment,L,R,Ext),
	(Ext = no ->
		Mode = oriented(automatic)
	;
		Mode = oriented(userHelp)
	),
	!.


tps_ordering(Comment,L,R,_,_,Mode) :-
	tps_current_ordering(poly(_N)),
	tps_getCurrentTPS(SBefore),
	pol_ordering(Comment,L,R),
	tps_getCurrentTPS(SAfter),
	(SBefore = SAfter ->
		Mode = oriented(automatic)
	;
		Mode = oriented(userHelp)
	), 
	!.

tps_ordering(_,_,R,_,_,oriented(automatic)) :-
	tps_current_ordering(manual),
	R =.. [O|Args],
	(	isConstructor(O,args(Args))
	;	O = '@'
	),
	!.
tps_ordering(_,L,_,_,_,oriented(automatic)) :-
	tps_current_ordering(manual),
	L =.. [O|Args],
	isConstructor(O,args(Args)),
	!,
	fail.
tps_ordering((_Comment,_CArgs),L,R,choices(_Requests,Answers),_,Mode) :-
	tps_current_ordering(manual),
	fetchNewAction(orient(Action),lr,'$equation'(_C,[L=R]),_,Reverse,
		       [a,u,l,r|Answers]),
	(member((Action,Reverse),[(a,no),(l,no),(u,yes),(r,yes)]) ->
		Mode = oriented(manually)
	;
		(member((Action,Reverse),[(a,yes),(l,yes),(u,no),(r,no)]) ->
			Mode = notoriented(u)
		;
			member(Action,Answers),
			Mode = notoriented(Action)
		)
	).
tps_ordering((Comment,CArgs),L,R,choices(Requests,Answers),_,Mode) :-
	tps_current_ordering(manual),
	nl,
	sPrint(Comment,CArgs),
        nl,
	write('A proof of'),
	nl,
	write('	'),
	print('$term'(L)),
	nl,
	write('>'),
	nl,
	write('	'),
	print('$term'(R)),
	nl,
	write('is required.

At this point you may take any of the the following actions:'),
	prompt1(['u. for unprovable',
		'a. for assume to been proved'|Requests],[u,a|Answers],A),
	(A = a -> 
		Mode = oriented(manually)
	;
		Mode = notoriented(A)
	),
	assertz(actionNew(orient(A),'$equation'([],[L=R]))),
	!.

tps_ordering((_Comment,_CArgs),_L,_R,choices([],[]),_,M):-
	!,
	M=notoriented.

tps_ordering((_Comment,_CArgs),L,R,choices(_Requests,Answers),_,Mode) :-
	fetchNewAction(orient(A),lr,'$equation'(_C,[L=R]),_,no,[a|Answers]),
	(A = a ->
		Mode = oriented(manually)
	;
		Mode = notoriented(A)
	),
	!.
tps_ordering((Comment,CArgs),L,R,choices(Requests,Answers),_,Mode) :-
	sPrint(Comment,CArgs),
        nl,
	write('The current ordering fails to prove'),
	nl,
	write('	'),
	print('$term'(L)),
	nl,
	write('>'),
	nl,
	write('	'),
	print('$term'(R)),
	write('.

At this point you may take any of the the following actions:'),
	prompt1(['a. for assume to been proved'|Requests],[a|Answers],A),
	(A = a -> 
		Mode = oriented(manually)
	;
		Mode = notoriented(A)
	),
	assertz(actionNew(orient(A),'$equation'([],[L=R]))),
	!.


tps_ordering(Comment,L,R,LO,RO,_,Ext,Mode) :-
	tps_current_ordering(kns_or_neqkns),
	kns_ordering(Comment,L,R,LO,RO,Ext),
	(Ext = no ->
		Mode = oriented(automatic)
	;
		Mode = oriented(userHelp)
	),
	!.


tps_ordering(Comment,L,R,LO,RO,_,_,Mode) :-
	tps_current_ordering(poly(_N)),
	tps_getCurrentTPS(SBefore),
	pol_ordering(Comment,L,R,LO,RO),
	tps_getCurrentTPS(SAfter),
	(SBefore = SAfter ->
		Mode = oriented(automatic)
	;
		Mode = oriented(userHelp)
	), 
	!.

tps_ordering(_,L,R,L,R,_,_,oriented(automatic)) :-
	tps_current_ordering(manual),
	R =.. [O|Args],
	(	isConstructor(O,args(Args))
	;	O = '@'
	),
	!.

tps_ordering(_,L,R,R,L,_,_,oriented(automatic)) :-
	tps_current_ordering(manual),
	L =.. [O|Args],
	(	isConstructor(O,args(Args))
	;	O = '@'
	),
	!.

tps_ordering((_Comment,_CArgs),L,R,LO,RO,choices(_Requests,Answers),_,Mode) :-
	tps_current_ordering(manual),
	fetchNewAction(orient(Action),lrOrRl,
		       '$equation'(_C,[L=R]),_,Reverse,[l,r|Answers]),
	(member((Action,Reverse),[(l,no),(r,yes)]) ->
		LO = L,
		RO = R,
		Mode = oriented(manually)
	;
		(member((Action,Reverse),[(l,yes),(r,no)]) ->
			LO = R,
			RO = L,
			Mode = oriented(manually)
		;
			member(Action,Answers),
			Mode = notoriented(Action)
		)
	).
tps_ordering((Comment,CArgs),L,R,LO,RO,choices(Requests,Answers),_,Mode) :-
	tps_current_ordering(manual),
	nl,
	sPrint(Comment,CArgs),
	nl,
	write('Consider the terms: '),
	nl,
	write('	'),
	print('$term'(L)),
	nl,
	write('and'),
	nl,
	write('	'),
	print('$term'(R)),
	write('.

You may take any of the following actions:
'),
	prompt1(Requests,Answers,C), % changed uh 15.02.90 there are no default Requests l and r anymore
	(C = l ->
		LO   = L,
		RO   = R,
		Mode = oriented(manually)
	;
		(C = r ->
			LO   = R,
			RO   = L,
			Mode = oriented(manually)
		;
			Mode = notoriented(C)
		)
	),
	assertz(actionNew(orient(C),'$equation'([],[L=R]))),
	!.

tps_ordering(_,_,_,_,_,choices([],[]),_,notoriented) :- !.

tps_ordering((_Comment,_CArgs),L,R,LO,RO,choices(_Requests,Answers),_,Mode) :-
	fetchNewAction(orient(Action),lrOrRl,'$equation'(_C,[L=R]),_,
		       Reverse,[l,r|Answers]),
	(member((Action,Reverse),[(l,no),(r,yes)]) ->
		LO = L,
		RO = R,
		Mode = oriented(manually)
	;
		(member((Action,Reverse),[(l,yes),(r,no)]) ->
			LO = R,
			RO = L,
			Mode = oriented(manually)
		;
			member(Action,Answers),
			Mode = notoriented(Action)
		)
	).
tps_ordering((Comment,CArgs),L,R,L0,R0,choices(Requests,Answers),_,Mode) :-
	nl,
	sPrint(Comment,CArgs),
        nl,
	write('The current ordering fails to orient the following pair of terms'),
	nl,
	write('	'),
	print('$term'(L)),
	nl,
	write('and'),
	nl,
	write('	'),
	print('$term'(R)),
	write('.

You may take any of the following actions:
'),
	prompt1(Requests,Answers,C), % changed uh 15.02.90 there are no default requests l and r anymore
	(C = l ->
		L0   = L,
		R0   = R,
		Mode = oriented(manually)
	;
		(C = r ->
			L0   = R,
			R0   = L,
			Mode = oriented(manually)
		;
			Mode = notoriented(C)
		)
	),
	assertz(actionNew(orient(C),'$equation'([],[L=R]))),
	!.


tps_init :-
	tps_current_ordering(kns_or_neqkns),
	kns_clear,
	!.
tps_init :-
	tps_current_ordering(poly(_N)),
	pol_delete_interpretations,
	!.
tps_init :-
	tps_current_ordering(manual),
	nl,
	write(' *** The current ordering is "manual".').

tps_greaterC(Precs) :-
	mapP(declareGreater,Precs).

declareGreater([A,B]):-
	kns_gt(A,B),
	!.
declareGreater([A,B]):-
	kns_makePrecedence(gt(A,B)).




tps_greater(X) :-
	tps_current_ordering(kns_or_neqkns),
	(kns_gtprec(X) ->
		true
	;
		sPrint(" *** ERROR ***,in the greater declaration, % is probably not a list of lists",
	[X])
	),
	!.

tps_greater(X) :-
	tps_current_ordering(Ord),
	\+ member(Ord,[kns,neqkns]),
	tps_error(Ord,kns_or_neqkns,greater(X)).


tps_equal(X) :-
	tps_current_ordering(kns),
	(kns_eqprec(X) ->
		true
	;
		sPrint(" *** ERROR ***,in the equal declaration, % is probably not a list of lists",
	[X])
	),
	!.

tps_equal(X) :-
	tps_current_ordering(Ord),
	Ord \== kns,
	tps_error(Ord,kns,equal(X)).


tps_status(OP,Status) :-
	tps_current_ordering(kns_or_neqkns),
	kns_statusprec(OP,Status),
	!.

tps_status(OP,Status) :-
	tps_current_ordering(Ord),
	\+ member(Ord,[kns,neqkns]),
	tps_error(Ord,kns_or_neqkns,status(OP,Status)).


tps_constructors(Cs) :-
	tps_current_ordering(kns_or_neqkns),
	kns_declConstr(Cs),
	!.
tps_constructors(_Cs) :-
	tps_current_ordering(Ord),
	\+ member(Ord,[kns,neqkns]),
	tps_error(Ord,kns_or_neqkns,tps_constructors).


tps_interpretation(IChanges) :-
	tps_current_ordering(poly(_N)),
	pol_current_interpretations(Changes),
	tps_assocConvert1(IChanges,Changes),
	!.
tps_interpretation([]) :-
	tps_current_ordering(Ord),
	\+ (Ord = poly(_N)),
	tps_error(Ord,poly(_N),interpretation),
	!.


tps_setInterpretation(Op,IP,Changed) :-
	tps_current_ordering(poly(_N)),
	!,
	Op =.. [O|Vars],
	tps_addInterpretation(O,Vars,IP,Changed),
	!.
tps_setInterpretation(Op,IP,_Changed) :-
	tps_current_ordering(Ord),
	\+ (Ord = poly(_N)),
	tps_error(Ord,poly(_N),setInterpretation(Op,IP)),
	!.



tps_addInterpretation(Op,Vars,IP,Changed) :-
	tps_current_ordering(poly(_N)),
	pol_get_current(pol_state(_,IList,_,_)),
	setof1(Arity,
	       Op1^T^L^(((Op ofType (Op1:T));(Op1 ofType (Op:T))),
		        length(T,L),
			Arity is L-1),
	       Arities),
	length(Vars,NrOfVars),
	(member(NrOfVars,Arities) ->
		true
	;
		((atomic(Op), name(Op,[36|_L])) ->
			true % Op is an auxiliary operator
			;
			nl,
			write(' *** Warning *** '),
			nl,
			sPrint(" *** % : There is no operator % with arity %.",
			      [setInterpretation,Op,NrOfVars]),
			nl
		)
	),
	(member(pol_op_interpretation(Op,NrOfVars,IPOld),IList)->
		(var(Changed) ->
			pol_add_interpretation(Op,Vars,IP),
			pol_get_current(pol_state(_,IListNew,_,_)),
			(	(	redComplOp(Op)
				;	nRedComplOp(Op)
				;	member(pol_op_interpretation(Op,NrOfVars,IPOld),IListNew))
			->	Changed = no
			;	Changed = yes
			)
		;	true)
	;	pol_add_interpretation(Op,Vars,IP)),
	!.



tps_operators(Mod) :-
	tps_current_ordering(kns),
	nl,
	write('The current termination ordering is "kns".'),
	kns_displayOpPrec(Mod),
	!.
tps_operators(Mod) :-
	tps_current_ordering(neqkns),
	nl,
	write('The current termination ordering is "neqkns".'),
	kns_displayOpPrec(Mod),
	!.
tps_operators(_Mod) :-
	tps_current_ordering(poly(N)),
	nl,
	sPrint("The current termination ordering is ""poly(%)"".",[N]),
	nl,
	pol_show_interpretations,
	!.
tps_operators(_Mod) :-
	tps_current_ordering(manual),
	nl,
	write('The current termination ordering is "manual".'),
	!.


%tps_operators :-
%	tps_current_ordering(kns),
%	nl,
%	write('The current termination ordering is "kns".'),
%	kns_displayOpPrec,
%	!.
%tps_operators :-
%	tps_current_ordering(neqkns),
%	nl,
%	write('The current termination ordering is "neqkns".'),
%	kns_displayOpPrec,
%	!.
%tps_operators :-
%	tps_current_ordering(poly(N)),
%	nl,
%	sPrint("The current termination ordering is ""poly(%)"".",[N]),
%	pol_show_interpretations,
%	!.
%tps_operators :-
%	tps_current_ordering(manual),
%	nl,
%	write('The current termination ordering is "manual".'),
%	!.


tps_rename(Assoc) :-
	tps_current_ordering(kns_or_neqkns),
	kns_rename(Assoc),
	!.

tps_rename(Assoc) :-
	tps_current_ordering(poly(_N)),
%	tps_assocConvert(Assoc,Polassoc),    % Poly distinguishes operators with identical
					     % names but different arities.
       	pol_get_current(System),
	pol_rename2(System,Assoc,NewSystem),
        pol_make_current(NewSystem),
	!.

tps_rename(_Assoc) :-
	tps_current_ordering(manual).


tps_assocConvert([],[]) :- !.

tps_assocConvert([(A,B)|AssocL],[(A/_,B/_)|PolassocL]) :-
	tps_assocConvert(AssocL,PolassocL).


tps_assocConvert1([],[]) :- !.

tps_assocConvert1([A|AssocL],[A/_|PolassocL]) :-
	tps_assocConvert1(AssocL,PolassocL).

tps_combine(tps(noorder,_,_),O,O):-!.
tps_combine(O,tps(noorder,_,_),O):-!.
tps_combine(tps(Current1,Poly1,kns_state(Flex1,Kns1)),tps(Current2,Poly2,kns_state(Flex2,Kns2)),
	    tps(Current3,Poly3,kns_state(Flex3,Kns3))) :-
	tps_combineable(Current1,Current2,Current3),
	(Current3 = poly(_N) ->
		pol_combine(Poly1,Poly2,Poly3),
		Kns3 = Kns1
	;
		Kns3 = Kns2,
		Poly3 = Poly1,
		tps_combineFlexible(Flex1,Flex2,Flex3)
	),
	!.


tps_combineable(kns,kns,kns).

tps_combineable(neqkns,neqkns,neqkns).

tps_combineable(kns,neqkns,kns).

tps_combineable(neqkns,kns,kns).

tps_combineable(poly(N),poly(M),poly(N)) :-
	N >= M.

tps_combineable(poly(M),poly(N),poly(N)) :-
	N > M.

tps_combineable(manual,manual,manual).


tps_combineFlexible(on,on,on).

tps_combineFlexible(on,off,on).

tps_combineFlexible(off,on,on).


%    tps_order(->Ordering).  ("kns" or "neqkns" or "poly(N)" or "manual")
%
%    change the termination proof technique
%

tps_order(NewCurrent) :-
	member(NewCurrent,[kns,neqkns,poly(N),manual]),
	tps_getCurrentTPS(tps(_Current,PolState,KnsState)),
	(ac_ist_AC(_),
	 member(NewCurrent,[kns,neqkns]) -> 
		nl,
		sPrint(
"There are AC-operators in your specification so that termination proofs
via ""%"" are not applicable.
",		       [NewCurrent]),
		fail
	;
		true
	),
	tps_makeCurrentTPS(tps(NewCurrent,PolState,KnsState)),
	(NewCurrent = poly(N) ->
		pol_change_tuplelength(N)
	;
		true
	),
	!.

tps_getCurrentTPS(State) :-
	tps_state(State),
	!.

tps_makeCurrentTPS(State) :-
	abolish(tps_state,1),
	assert(tps_state(State)),
	!.


%tps_tuplelength(NewTupleLength) :-
%	tps_current_ordering(poly(N)),
%	N \== NewTupleLength,
%	pol_change_tuplelength(NewTupleLength),
%	!.
%tps_tuplelength(_NewTupleLength) :-
%	tps_current_ordering(Ord),
%	Ord \== poly(_N),
%	nl,
%	write(' *** The current ordering is "'),
%	write(Ord),
%	write('".
%	Ordering = "poly(N)" must be specified before calling '),
%	nl,
%	write('        '),
%	write(tps_tuplelength),
%	nl,
%	!. 


tps_current_ordering(Current) :-
	Current == kns_or_neqkns,
	!,
	tps_current_ordering(Ord),
	member(Ord,[kns,neqkns]),
	!.
tps_current_ordering(Current) :-
	tps_getCurrentTPS(tps(Current,_,_)),
	!.


tps_declac(Op) :-
	tps_current_ordering(poly(_N)),
	pol_declare_op(Op,ac),
	!.
tps_declac(_Op) :-
	tps_current_ordering(Ord),
	\+ (Ord = poly(_N)),
	tps_error(Ord,poly(_N),tps_declac).


tps_declc(Op) :-
	tps_current_ordering(Ord),
	(Ord = poly(_N) ->
		pol_declare_op(Op,c)
	;	tps_status(Op,ms)
	),	
	!.



pol_get_current(PolState) :-
	tps_getCurrentTPS(tps(poly(_N),PolState,_KnsState)).


pol_make_current(PolState) :-
	tps_getCurrentTPS(tps(poly(N),_,KnsState)),
	tps_makeCurrentTPS(tps(poly(N),PolState,KnsState)).


tps_error(CurrentOrd,kns_or_neqkns,Predicate) :-
	nl,
	sPrint(" *** The current ordering is ""%"".
	Ordering = ""kns"" or ""neqkns""  must be specified before
	calling ""%"".", 
	       [CurrentOrd,Predicate]),
	nl.
tps_error(CurrentOrd,ForcedOrd,Predicate) :-
	nl,
	sPrint(" *** The current ordering is ""%"".
	Ordering = ""%"" must be specified before
	calling ""%"".",
	       [CurrentOrd,ForcedOrd,Predicate]),
	nl.
