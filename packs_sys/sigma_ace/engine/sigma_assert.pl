:-include('sigma_header.pl').

/*

When this enter in thru this interface various things may happen:

1)   The surface is stored as-is (noncanonicalize)
2)   The surface is checked for truth (untrusted/trusted)
3)   The surface is canonicalized (canonicalize)
4)   The canonicalized form is complied (compile)
5)   The compiled form reaches active memory									 
									 
									 
| ?- agent_tell("(isa Fido Dog)",'ToplevelContext',55).
% (assertion (Always (Implies (proven 55 ) ('surface-instance' Fido Dog 'ToplevelContext' ))) )

| ?- agent_tell("(genls Dog Mammel)",'ToplevelContext',55).
% (assertion (Always (Implies (proven 55 ) (i_genls  Dog Mammel 'ToplevelContext' ))) )
% (assertion (Always (Implies (proven 55 ('surface-instance' ?_h707  Dog 'ToplevelContext' ) ) ('surface-instance' ?_h707  Mammel 'ToplevelContext' ))) )

| ?- agent_tell("(genlPreds SubSpecies SubClass)",'ToplevelContext',55).
% (assertion (Always (Implies (proven 55 (SubSpecies ?_h820  ?_h818  'ToplevelContext' ) ) (SubClass ?_h820  ?_h818  'ToplevelContext' ))) )
% (assertion (Always (Implies (proven 55 (SubSpecies ?_h820  ?_h818  ?_h816  'ToplevelContext' ) ) (SubClass ?_h820  ?_h818  ?_h816  'ToplevelContext' ))) )
% (assertion (Always (Implies (proven 55 (SubSpecies ?_h820  ?_h818  ?_h816  ?_h814  'ToplevelContext' ) ) (SubClass ?_h820  ?_h818  ?_h816  ?_h814  'ToplevelContext' ))) )
% (assertion (Always (Implies (proven 55 (SubSpecies ?_h820  ?_h818  ?_h816  ?_h814  ?_h812  'ToplevelContext' ) ) (SubClass ?_h820  ?_h818  ?_h816  ?_h814  ?_h812  'ToplevelContext' ))) )

| ?- agent_tell("(genlInverse MotherOf SonOf)",'ToplevelContext',55).
% (assertion (Always (Implies (proven 55 (SonOf ?_h782  ?_h784  'ToplevelContext' ) ) (MotherOf ?_h784  ?_h782  'ToplevelContext' ))) )
% (assertion (Always (Implies (proven 55 (MotherOf ?_h782  ?_h784  'ToplevelContext' ) ) (SonOf ?_h784  ?_h782  'ToplevelContext' ))) )

| ?- agent_tell("(result MotherOfFn Woman)",'ToplevelContext',55).
% (assertion (Always (Implies (proven 55 ) (result (MotherOfFn ?_h862 ) Woman 'ToplevelContext' ))) )

| ?- agent_tell("(resultIsa MotherOfFn Woman)",'ToplevelContext',55).
% (assertion (Always (Implies (proven 55 ) ('surface-instance' MotherOf Function 'ToplevelContext' ))) )
% (assertion (Always (Implies (proven 55 ) ('surface-instance' (MotherOfFn ?_h930 ) Woman 'ToplevelContext' ))) )

| ?- agent_tell("(arg1Isa MotherOfFn Child)",'ToplevelContext',55).
% (assertion (Always (Implies (proven 55 ) ('domain' (MotherOfFn ?_h847 ) 1 Child 'ToplevelContext' ))) )

| ?- agent_tell("(arg2Genl isa Collection)",'ToplevelContext',55).
% (assertion (Always (Implies (proven 55 ) ('domainSubclass' isa 2 class 'ToplevelContext' ))) )

| ?- agent_tell("(thereExists ?X (isa ?X Person))",'ToplevelContext',55).
% (assertion (Always (Implies (proven 55 ) ('surface-instance' (entity1Fn ) Person 'ToplevelContext' ))) )

| ?- agent_tell("(forall ?X (=> (isa ?X Person) (isa ?X Human)))",'ToplevelContext',55).
% (assertion (Always (Implies (proven 55 ('surface-instance' ?_h1595  Person 'ToplevelContext' ) ) ('surface-instance' ?_h1595  Human 'ToplevelContext' ))) )

| ?- agent_tell("(thereExists (?X Woman) (isa ?X Person))",'ToplevelContext',55).
% (assertion (Always (Implies (proven 55 ) ('surface-instance' (entity2Fn ) Woman 'ToplevelContext' ))) )
% (assertion (Always (Implies (proven 55 ) ('surface-instance' (entity2Fn ) Person 'ToplevelContext' ))) )

| ?- agent_tell("(thereExists (?X ?Y) (married ?X ?Y))",'ToplevelContext',55).
% (assertion (Always (Implies (proven 55 ) (married (entity8Fn ?_h1376  ) (entity7Fn (entity8Fn ?_h1376  ) ) 'ToplevelContext' ))) )

| ?- agent_tell("(forall ((?X Man)) (likes ?X FootBall))",'ToplevelContext',55).
% (assertion (Always (Implies (proven 55 ('surface-instance' ?_h1347  Man 'ToplevelContext' ) ) (likes ?_h1347  FootBall 'ToplevelContext' ))) )

| ?- agent_tell("(forall ((?X Woman)(?Y Man)) (married ?X ?Y))",'ToplevelContext',55).
% (assertion (Always (Implies (proven 55 (and ('surface-instance' ?_h1667  Man 'ToplevelContext' ) ('surface-instance' ?_h1660  Woman 'ToplevelContext' ) ) ) (married ?_h1660  ?_h1667  'ToplevelContext' ))) )

| ?- agent_tell("(thereExists ((?X Woman)(?Y Man)) (married ?X ?Y))",'ToplevelContext',55).
% (assertion (Always (Implies (proven 55 ) ('surface-instance' (entity5Fn ?_h1740  ) Man 'ToplevelContext' ))) )
% (assertion (Always (Implies (proven 55 ) ('surface-instance' (entity6Fn ?_h1740  ) Woman 'ToplevelContext' ))) )
% TODO %Needs Work
% (assertion (Always (Implies (proven 55 ) (married (entity6Fn ?_h1740  ) (entity5Fn (entity6Fn ?_h1740  ) ) 'ToplevelContext' ))) )

| ?- agent_tell("(new Consultation Relation LooksLike (+ -)  )",'ToplevelContext',55).
% (assertion (Always (Implies (proven 55 ) ('surface-instance' LooksLike Relation 'ToplevelContext' ))) )
% (assertion (Always (Implies (proven 55 ) (functsymbol LooksLike 2 'ToplevelContext' ))) )
% (assertion (Always (Implies (proven 55 (consultation LooksLike (+ - ) (?_h1910  ?_h1912  ) 'ToplevelContext' ) ) (LooksLike ?_h1910  ?_h1912  'ToplevelContext' ))
) )

| ?- agent_tell("(new Consultation Relation TheUsersName ( - )  )",'ToplevelContext',55).
% (assertion (Always (Implies (proven 55 ) ('surface-instance' TheUsersName Relation 'ToplevelContext' ))) )
% (assertion (Always (Implies (proven 55 ) (functsymbol TheUsersName 1 'ToplevelContext' ))) )
% (assertion (Always (Implies (proven 55 (consultation TheUsersName - ?_h2301  'ToplevelContext' ) ) (TheUsersName ?_h1824  'ToplevelContext' ))) )

| ?- agent_tell("(new Consultation Relation WhatIsYourName (-) )",'ToplevelContext',55).
% (assertion (Always (Implies (proven 55 ) ('surface-instance' WhatIsYourName Relation 'ToplevelContext' ))) )
% (assertion (Always (Implies (proven 55 ) (functsymbol WhatIsYourName 1 'ToplevelContext' ))) )
% (assertion (Always (Implies (proven 55 (consultation WhatIsYourName - ?_h2304  'ToplevelContext' ) ) (Fn WhatIsYourName ?_h1807  'ToplevelContext' ))) )

| ?- agent_tell("(new Entity Poo Bear)",'ToplevelContext',55).
% (assertion (Always (Implies (proven 55 ) ('surface-instance' Poo Entity 'ToplevelContext' ))) )
% (assertion (Always (Implies (proven 55 ) (i_genls  Bear Entity 'ToplevelContext' ))) )
% (assertion (Always (Implies (proven 55 ('surface-instance' ?_h1110  Bear 'ToplevelContext' ) ) ('surface-instance' ?_h1110  Entity 'ToplevelContext' ))) )

| ?- agent_tell("(new Function CapitolOf (Sovern) City)",'ToplevelContext',55).
% (assertion (Always (Implies (proven 55 ) ('surface-instance' CapitolOf Function 'ToplevelContext' ))) )
% (assertion (Always (Implies (proven 55 ) (functsymbol CapitolOf 1 'ToplevelContext' ))) )
% (assertion (Always (Implies (proven 55 ) ('surface-instance' CapitolOf Function 'ToplevelContext' ))) )
% (assertion (Always (Implies (proven 55 ) ('surface-instance' (CapitolOfFn ?_h1703 ) City 'ToplevelContext' ))) )
*/


                                                                                                                      
/*

Prototype: tell(+Assertion[,Ctx][,Tracking]).

Assertion: is any writeFmt in KIF/SUMO/Prolog
Ctx: Prolog atom defining the context the assertion or command is ran in
Tracking: Prolog Term provided by external source for tracking purposes


Usage: a component needs to modify the known world

See examples above

*/
% Entry point for Compiling assertions and retractions 

% =====================================================================================
%  agent_tell(Tell_chars,Ctx,TN,KB,User)
% =====================================================================================
agent_tell(Tell_chars,Cxt):-!,
         agent_tell(Tell_chars,Ctx,TN,KB,'Author').
         
agent_tell(Tell_chars,Ctx,TN):- 
         agent_tell(Tell_chars,Ctx,TN,KB,'Author').

agent_tell(Tell_chars,Ctx,TN,KB,User):-!,   
            once(tell_retract_parse_chars(Tell_chars,Pterm,Vars)),
            invokeTell([untrusted,canonicalize],surface,Pterm,Ctx,TN,KB,Vars,User).


dte:- agent_tell("(isa Joe Human)",'ToplevelContext').

% ===================================================================
% Normalizing Surfaces
% ===================================================================

toFSCK(Form,_,_,AssertionO,SourceCtxO,SourceTNO):-
			Form=..[asserted,SourceCtx,Assertion],!,
			toFSCK(Assertion,SourceCtx,SourceTN,AssertionO,SourceCtxO,SourceTNO).
toFSCK(Form,_,_,AssertionO,SourceCtxO,SourceTNO):-
			Form=..[pnf,Assertion,SourceCtx,SourceTN],!,
			atom_codes(SourceTN,[84,45|TNCODES]),catch(number_codes(SourceTNM,TNCODES),_,SourceTNM=TN),
			toFSCK(Assertion,SourceCtx,SourceTNM,AssertionO,SourceCtxO,SourceTNO).
toFSCK(Form,_,_,AssertionO,SourceCtxO,SourceTNO):-
			Form=..[ist,SourceCtx,Assertion],!,
			toFSCK(Assertion,SourceCtx,SourceTN,AssertionO,SourceCtxO,SourceTNO).
toFSCK('BACKWARD'(Formula),Ctx,TN,Formula,Ctx,TN):-!.
toFSCK('FORWARD'(Formula),Ctx,TN,Formula,Ctx,TN):-!.
toFSCK('sharedNotes'(_,Formula),Ctx,TN,Formula,Ctx,TN):-!.
toFSCK('clause-form'(Formula),Ctx,TN,Formula,Ctx,TN):-!.
toFSCK('query'(Formula),Ctx,TN,'query'(Formula),Ctx,TN):-!.
toFSCK((Formula),Ctx,TN,(Formula),Ctx,TN):-!.


% ===================================================================
% SIGMA TELL/RETRACT
% ===================================================================

:-was_indexed(invokeTell(1,1,1,0,0,0,0,0)).

% ======================================================
% Ignored Invokations
% ======================================================
invokeTell(Driver,Any,surf,Ctx,TN,KB_Name,Vars,Author):-!.
invokeTell(Driver,Form,formula,Ctx,TN,KB_Name,Vars,Author):-!.        
invokeTell(Driver,Any,comment(_),Ctx,TN,KB_Name,Vars,Author):-!.
invokeTell(Driver,Any,file_comment(_),Ctx,TN,KB_Name,Vars,Author):-!.
invokeTell(Driver,Any,end_of_file,Ctx,TN,KB_Name,Vars,Author):-!.
invokeTell(Driver,Any,true,Ctx,TN,KB_Name,Vars,Author):-!.
invokeTell(Driver,Any,(true:-true),Ctx,TN,KB_Name,Vars,Author):-!.
invokeTell(Driver,Any,Form,Ctx,TN,KB_Name,Vars,Author):-var(Ctx),
	sendNote(user,'Assert mech','Variable in Context',Form),!,true.
invokeTell(Driver,Any,Form,Ctx,TN,KB_Name,Vars,Author):-var(KB_Name),
	sendNote(user,'Assert mech','Variable in KB',Form),!,true.



% ======================================================
% Conjunctive Invokations
% ======================================================
invokeTell(Driver,Type,(Form,Ula),Ctx,TN,KB,Vars,Author):-!,
		logOnFailure(invokeTell(Driver,Type,Form,Ctx,TN,KB,Vars,Author)),!,
		logOnFailure(invokeTell(Driver,Type,Ula,Ctx,TN,KB,Vars,Author)),!.

invokeTell(Driver,Type,and(Form,Ula),Ctx,TN,KB,Vars,Author):-!,
		logOnFailure(invokeTell(Driver,Type,Form,Ctx,TN,KB,Vars,Author)),!,
		logOnFailure(invokeTell(Driver,Type,Ula,Ctx,TN,KB,Vars,Author)),!.


% ======================================================
% Choose KIF or ACE (For now always choosing kif)
% ======================================================
invokeTell(Driver,chars,CHARS,Ctx,TN,KB_Name,Vars,Author):-!,
         invokeTell(Driver,kif,CHARS,Ctx,TN,KB_Name,Vars,Author),!.       


% ======================================================
% Make Surface Forms from KIF chars
% ======================================================
invokeTell(Driver,kif,CHARS,Ctx,ETN,KB_Name,_,Author):-!,
         once(getSurfaceFromChars(CHARS,STERM,Vars)),                      
         getSigmaTermFromSurface(STERM,Formula),!,   
         invokeTell(Driver,surface,Formula,Ctx,ETN,KB_Name,Vars,Author),!.       

% ======================================================
% Make Surface Forms from ACE chars
% ======================================================
invokeTell(Driver,ace,CHARS,Ctx,TN,KB_Name,Vars,Author):-
         ace_to_surface(CHARS,Formula),
         getSigmaTermFromSurface(STERM,Formula),!,
         invokeTell(Driver,surface,Formula,Ctx,TN,KB_Name,Vars,Author),!.       

% ======================================================
% Actually Assert the surface (trusted/untrusted)
% ======================================================
invokeTell(Driver,surface,FormulaIn,Ctx,TN,SKB,Vars,Author):-!,         
	logOnFailure(toFSCK(FormulaIn,Ctx,TN,Formula,SCtx,STN)),!,
	invokeTell(Driver,fsck,Formula,SCtx,STN,SKB,Vars,Author),!.         

% ======================================================
% Check for Simple Surface
% ======================================================
/*
invokeTell(Driver,fsck,Surface,Ctx,TN,KB,Vars,Author):-
	        getConstants(atomic,Surface,C,_,_),
		once(disabled_keywords(C)),!,
		idGen(TN), % create tracking now if needed
		destroyTN(KB,TN,_),
		ignore((
			logOnFailure(assertaClean(sigmaCache(Surface,entails(true,true),[],Vars,KB,Ctx,TN,Author,not_used)))
		)),!.
*/
     
disabled_keywords(L):-
	member(T,L),
	disabled_keyword(T),!.
	
disabled_keyword('AbstractionFn').	
disabled_keyword('GeneralizedUnionFn').	
disabled_keyword('GeneralizedIntersectionFn').	
disabled_keyword('UnionFn').	
disabled_keyword('ExtensionFn').	
disabled_keyword('IntersectionFn'). 
disabled_keyword('ComplementFn'). 
disabled_keyword('RangeFn'). 
disabled_keyword('DomainFn'). 
%disabled_keyword(A):-atom(A),atom_concat(_,'Fn',A),!.	
disabled_keyword(A):-atom(A),atom_concat(_,'On',A),!.	

/*
disabled_keyword(exists).	
*/

tam(Surface,Vars):-
	flag(indent,_,0),
	TN = test,
	KB = 'Merge',
	Author = 'Author',
	Ctx = 'Context',
%	writeObject(Surface,Vars),
%	once(writeObject('$spacer',Vars)),
	logOnFailure(getAssertionClauses(KB,Ctx,Surface,CAN,Vars,Flags)),
	once(writeObject('$spacer',Vars)),
	once(writeObject(ff(Flags),Vars)),
	once(writeObject('$spacer',Vars)),
	flag(clause_id,_,0),
	Result = 'on',
	tam(Surface,CAN,Flags,Vars,KB,Ctx,TN,Author,Result),
	flag(clause_id,CLID,CLID),
	format('<hr>Clauses: ~w',[CLID]).

	

tam(Surface,and(CAN1,CAN2),Flags,Vars,KB,Ctx,TN,Author,Result):-!,
	tam(Surface,CAN1,Flags,Vars,KB,Ctx,TN,Author,Result),
	tam(Surface,CAN2,Flags,Vars,KB,Ctx,TN,Author,Result).
tam(Surface,entails(true,CAN2),Flags,Vars,KB,Ctx,TN,Author,Result):-!,
	tam(Surface,CAN2,Flags,Vars,KB,Ctx,TN,Author,Result).
tam(Surface,entails(false,CAN2),Flags,Vars,KB,Ctx,TN,Author,Result):-!,
	tam(Surface,absurd(CAN2),Flags,Vars,KB,Ctx,TN,Author,Result).
tam(Surface,true,Flags,Vars,KB,Ctx,TN,Author,Result):-!.
	

tam(Surface,entails(OAnte,OConsq),Flags,Vars,KB,Ctx,TN,Author,Result):-!, 
	flag(indent,_,0),
	once(writeObject('$spacer',Vars)),
	once(writeObject_conj(entails(OAnte,OConsq),Vars)),
%	once(writeObject('$spacer',Vars)),
%	once(writeObject(ff(Flags),Vars)),
	once(writeObject('$spacer',Vars)),
	flag(clause_id,CLID,CLID+1),
	logOnFailure(once(putAttributeStructures(Surface,Rule,KB,Flags,entails(OAnte,OConsq),entails(Ante,Consq)))),!,
		convertListNotNeg([Consq],[NConsq]),
		conjunctsToList(Ante,List),%trace,
		reorderAnteceedants([NConsq|List],List,All),!,
		convertListNotNeg(All,AnteListS),!,
		unnumbervars(
				(NConsq,AnteListS,Vars,KB,Ctx,surf(KB,TN,CLID,Vars)),
				(UNConsq,UAnteListS,UVars,UKB,UCtx,UProof)),
		sigma_numbervars((UNConsq,UAnteListS,UVars,UKB,UCtx,UProof),0,_),% trace,
	length(AnteListS,Cost),
		format('~q.~n',['2'(UNConsq,UAnteListS,Cost,UVars,UKB,UCtx,UProof)]),!.
		


tam(Surface,surface,Flags,Vars,KB,Ctx,TN,Author,Result):-!,
	tam(Surface,Surface,Flags,Vars,KB,Ctx,TN,Author,Result).
     
% Simple Fact
tam(Surface,OConsq,Flags,Vars,KB,Ctx,TN,Author,Result):-!,
	flag(indent,_,0),
	once(writeObject('$spacer',Vars)),
	once(writeObject_conj(OConsq,Vars)),
%	once(writeObject('$spacer',Vars)),
%	once(writeObject(ff(Flags),Vars)),
	once(writeObject('$spacer',Vars)),
	flag(clause_id,CLID,CLID+1),
	logOnFailure(once(putAttributeStructures(Surface,Rule,KB,Flags,OConsq,Consq))),!,
	convertListNotNeg([Consq],[NConsq]),
	unnumbervars((NConsq,Ctx,surf(KB,TN,CLID,Vars)),(UConsq,UCtx,UProof)),
	sigma_numbervars((UConsq,UCtx,UProof),0,_),!,
	format('~q.~n',['1'(UConsq,KB,UCtx,UProof)]).
	
	

% ======================================================
% Do truth checks and assets surface and NNF
% ======================================================
invokeTell(Driver,fsck,Surface,Ctx,TN,KB,Vars,Author):-!,
		idGen(TN), % create tracking now if needed
	        destroyTN(KB,TN,_),
		ignore((
		% This may fail and will give an error message
			% Vars come back Numbered
			((
			  logOnFailure(getAssertionClauses(KB,Ctx,Surface,CAN,Vars,Flags)) ->
				((
						% This may fail but it will give a message
						%getTruthCheckResults(tell,Driver,Surface,CAN,Flags,Ctx,TN,KB,Vars,Author,Result),
						%Result = on, 
						% TODO assert the kr terms and delete them from list
						(save_can_to_file(KB,Handle) ->
							format(Handle,'~q.\n',['sigmaCache'(Surface,CAN,Flags,Vars,KB,Ctx,TN,Author,on)]);
                                                        assert(sigmaCache(Surface,CAN,Flags,Vars,KB,Ctx,TN,Author,Result))),!,
						ignore((
							memberchk(compile,Driver),
							recanonicalizeTN(KB,TN)))
					));
				% This will save errors
						(save_can_to_file(KB,Handle) ->
							format(Handle,'~q.\n',['error1'(Surface,CAN,Flags,Vars,KB,Ctx,TN,Author,on)]);
                                                        assert(sigmaCache(Surface,CAN,Flags,Vars,KB,Ctx,TN,Author,on))),!
			  )),!
				
		)),!.

	
% =================================================
% Assert clean and new (Prolog) 												 
% =================================================
assertaClean(X):-unnumbervars(X,Y),!,asserta(Y).

assertzClean(X):-unnumbervars(X,Y),!, assert(Y).


assertAll([]):-!.
assertAll(end_of_file).
assertAll([H|T]):-!,notrace((assertAll(H),assertAll(T))),!.
assertAll((H,T)):-!,notace((assertAll(H),assertAll(T))),!.	
assertAll(':-'(T)):-!,call(T).
assertAll(InKB):-isClaused(InKB),!.
assertAll(T):-catch(asserta(T),E,format('~n% prolog warning ~w ~n',[E])),!. 

isClaused(InKB):-
	sigma_notrace(not(not((sigma_numbervars(InKB,0,_),!,isClausedG(InKB))))),!.
	
isClausedG(sigmaCache(C,A,ProofID:KRVars:KR,KB,Ctx,TN)):-
	clause(sigmaCache(C,A,_:_:_,KB,_,_),true,OldID),
	clause(sigmaCache(OC,OA,_,_,_,_),true,OldID),
	sigma_numbervars(OC:OA,0,_),
	C:A==OC:OA,!,ifInteractive(write(',')),!.

isClausedG(sigmaCache(C,ProofID:KRVars:KR,KB,Ctx,TN)):-
	clause(sigmaCache(C,_:_:_,KB,_,_),true,OldID),
	clause(sigmaCache(OC,_,_,_,_),true,OldID),
	sigma_numbervars(OC,0,_),
	C==OC,!,ifInteractive(write(',')),!.

isClausedG(sigmaCache(_:_,C,KB,_,_)):-
	clause(sigmaCache(_:_,C,KB,_,_),true,OldID),
	clause(sigmaCache(_:_,OC,KB,_,_),true,OldID),
	sigma_numbervars(OC,0,_),
	C==OC,!,ifInteractive(write(',')),!.

isClausedG(InKB:-B):-isClausedG(InKB,B),!.
isClausedG(InKB):-isClausedG(InKB,true),!.

isClausedG(C,A):-
	clause(C,A,OldID),
	clause(OC,OA,OldID),
	sigma_numbervars(OC:OA,0,_),
	C:A==OC:OA,!.
	
		
countAssertions(C:-A,N):-countAssertions(C,A,N).
countAssertions(C,N):-countAssertions(C,true,N).	

countAssertions(C,A,N):-
       % sigma_numbervars(C:A,0,_),
	flag(clauses_count,_,0),
	clause(C,A,ID),
	flag(clauses_count,X,X+1),fail.
countAssertions(C,A,N):-flag(clauses_count,N,N),!.




