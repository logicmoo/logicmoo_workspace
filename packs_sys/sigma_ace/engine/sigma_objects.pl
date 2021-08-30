
:-include('sigma_header.pl').

isNonVar(Denotation):-not(isSlot(Denotation)).

% Is var means to Sigma this is a Slot
isSlot(Denotation):-sigma_notrace((isVarProlog(Denotation);isVarObject(Denotation))),!.

isSlot(Denotation,Denotation2):- isVarProlog(Denotation),!,Denotation2=Denotation.
isSlot(Denotation,PrologVar):- isVarObject(Denotation,PrologVar),!.

isHiddenSlot(Term):-fail.

isVarProlog(A):-sigma_notrace((var(A);A='$VAR'(_))).

isVarObject(Denotation):-sigma_notrace((
		  isObject(Denotation,BaseType),
		  arg(1,Denotation,Value),!,isSlot(Value))).

isVarObject(Denotation,Value):-sigma_notrace((
		  isObject(Denotation,BaseType),
		  arg(1,Denotation,Value),!,isSlot(Value))).
	
isObject(Denotation,BaseType):-
	sigma_notrace(((atom(BaseType) ->
		  (atom(F),atom_concat('$',BaseType,F),functor(Denotation,F,2));
		  (functor(Denotation,F,2),atom(F),atom_concat('$',BaseType,F))
		 ),!)).

isQualifiableAsClass(Atom):-atom(Atom),!.
isQualifiableAsClass('$Class'(Atom,_)):-atom(Atom),!.

isQualifiableAs(Denotation,BaseType,Value):-
		  isObject(Denotation,BaseType),
		  arg(1,Denotation,Value).


isQualifiedAs(Denotation,_,_):-not(compound(Denotation)),!,fail.
isQualifiedAs(Denotation,BaseType,Value):-
		  isQualifiedAs(Denotation,BaseType,Value,SubType).
isQualifiedAs(Denotation,BaseType,Value,SubType):-
		  isObject(Denotation,BaseType),
		  arg(1,Denotation,Value),
		  arg(2,Denotation,List),
		  lastImproperMember(BaseType,SubType,List).

lastImproperMember(Default,Default,List):-isVarProlog(List),!.
lastImproperMember(Default,Default,[]):-!.
lastImproperMember(Default,SubType,List):-proper_list(List),last(SubType,List).
lastImproperMember(Default,SubType,[SubType|End]):-isVarProlog(End),!.
lastImproperMember(Default,SubType,[_|Rest]):-
	lastImproperMember(Default,SubType,Rest),!.
	
		  

isQualifiedAndKnownAs(Denotation,BaseType,Value):-
		  isQualifiedAs(Denotation,BaseType,Value),!,
		  not(isVarProlog(Value)).

isQualifiedAndVarAs(Denotation,BaseType,Value):-
		  isQualifiedAs(Denotation,BaseType,Value),!,
		  isVarProlog(Value).

isQualifiedAndVarAndUnifiable(Denotation,BaseType,NValue):-
		  isQualifiedAs(Denotation,BaseType,Value),!,
		  (isVarProlog(Value);
		  (\+ \+ NValue=Value)),!.

isBodyConnective(Funct):-atom_concat(_,'_',Funct),!.
isBodyConnective(Funct):-atom_concat('t~',_,Funct),!.
isBodyConnective(Funct):-atom_concat('f~',_,Funct),!.
isBodyConnective(Funct):-member(Funct,[and,or,until,',',';',':-',unless,xor,holdsDuring]). % Other Propositional Wrappers

isEntityref(Var,Var):-isSlot(Var),!.
isEntityref(Term,A):-Term=..[F,A,B],!,atom_concat('$',_,F),!.





isLiteralTerm(A):-isLiteralTerm_util(A),!.
isLiteralTerm(not(A)):-isLiteralTerm_util(A),!.

isLiteralTerm_util(A):-var(A),!.
isLiteralTerm_util('$VAR'(_)):-!.
isLiteralTerm_util(string(_)):-!.
isLiteralTerm_util(A):-not(compound(A)),!.
isLiteralTerm_util(A):-string(A).


isEntitySlot(Term):-isSlot(Term),!.
isEntitySlot(Term):-not(compound(Term)),!.
isEntitySlot(Term):-isEntityFunction(Term,FnT,ArgsT),!.

isEntityFunction(Term,FnT,ArgsT):-isSlot(Term),!,fail.
isEntityFunction(Term,FnT,ArgsT):-atomic(Term),!,fail.
isEntityFunction(Term,FnT,ArgsT):-Term=..[FnT|ArgsT],hlPredicateAttribute(FnT,'Function'),!.



% ===================================================================
% getPrologVars/4. 
% ===================================================================

getPrologVars(Term, Vars, Singletons, Multiples) :-
    sigma_notrace((getPrologVars(Term, VarList, []),
    close_list(VarList),
    keysort(VarList, KeyList),
    split_key_list(KeyList, Vars, Singletons, Multiples))).

getPrologVars(Term,  [Term - x|V], V) :-isVarProlog(Term),!.
getPrologVars(Term, V, V) :-not(compound(Term)),!.
getPrologVars(Term,  V0, V) :-
	isQualifiedAs(Term,Type,PrologVar),
	Type \= existential,!,
	(isVarProlog(PrologVar) -> V0=[PrologVar - x|V]; V0=V),!.
	
getPrologVars(Term, V0, V) :- 
	 functor(Term, F, N),
	 getPrologVars(1, N, Term, V0, V).
getPrologVars(I, N, Term, V0, V) :-
    (  (I > N) -> V=V0
    ;   arg(I, Term, Arg),
	getPrologVars(Arg, V0, V1),
	J is I + 1,
	getPrologVars(J, N, Term, V1, V)
    ).

% ===================================================================
% getPrologVars/4. 
% ===================================================================

getAllPrologVars(Term, Vars, Singletons, Multiples) :-
    sigma_notrace((getAllPrologVars(Term, VarList, []),
    close_list(VarList),
    keysort(VarList, KeyList),
    split_key_list(KeyList, Vars, Singletons, Multiples))).

getAllPrologVars(Term,  [Term - x|V], V) :-isVarProlog(Term),!.
getAllPrologVars(Term, V, V) :-not(compound(Term)),!.
getAllPrologVars(Term, V0, V) :- 
	 functor(Term, F, N),
	 getAllPrologVars(1, N, Term, V0, V).
getAllPrologVars(I, N, Term, V0, V) :-
    (  (I > N) -> V=V0
    ;   arg(I, Term, Arg),
	getAllPrologVars(Arg, V0, V1),
	J is I + 1,
	getAllPrologVars(J, N, Term, V1, V)
    ).

% ===================================================================
% getSlots/4. Returns no Existential Body Vars
% ===================================================================

getSlots(Term, Vars, Singletons, Multiples) :-
    sigma_notrace((getSlots(Term, VarList, []),
    close_list(VarList),
    keysort(VarList, KeyList),
    split_key_list(KeyList, Vars, Singletons, Multiples))).

getSlots(Term,  [Term - x|V], V) :-isVarProlog(Term),!.
getSlots(Term, V, V) :-not(compound(Term)),!.
getSlots(Term, V, V) :-isHiddenSlot(Term),!.
getSlots(Term,  VO, V) :-
	isQualifiedAs(Term,existential,EVar),!,
	getSlots(EVar,  VO, V).
getSlots(Term,  V0, V) :-
	isQualifiedAs(Term,Type,PrologVar),!,
	(isVarProlog(PrologVar) -> V0=[Term - x|V]; V0=V),!.
getSlots(Term, V0, V) :- 
	 functor(Term, F, N),
	 getSlots(1, N, Term, V0, V).
getSlots(I, N, Term, V0, V) :-
    (  (I > N) -> V=V0
    ;   arg(I, Term, Arg),
	getSlots(Arg, V0, V1),
	J is I + 1,
	getSlots(J, N, Term, V1, V)
    ).


% ===================================================================
% getConstants/4. 
% ===================================================================

getConstants(Types,Term, Vars, Singletons, Multiples) :-
    sigma_notrace((getConstants(Types,Term, VarList, []),
    close_list(VarList),
    keysort(VarList, KeyList),
    split_key_list(KeyList, Vars, Singletons, Multiples))).

getConstants(Types,Term, [Term - x|V], V) :- getConstants(Types,Term),!.
getConstants(Types,Term, V, V) :- var(Term),!.
getConstants(Types,Term,  FOUND, V) :-
            Term=..[L,I|ST],
            getConstants(Types,L, VL, []),
            consts_l(Types,[I|ST], FLIST),
            append(V,FLIST,UND),
            append(VL,UND,FOUND),!.

getConstants(Types,Term, V, V) :- !.
    
consts_l(Types,[],[]).
consts_l(Types,[L|IST], FLIST):-
         getConstants(Types,L, FOUND,[]), 
         consts_l(Types,IST, FOUNDMore), !,
         append(FOUND,FOUNDMore,FLIST).

    
getConstants(_,Term):- (var(Term) ; Term='$VAR'(_)),!,fail.
getConstants(atomic,A):-atomic(A).
getConstants(skolems,'zzskFn'(_)).
getConstants(funct,'AssignmentFn'(_,_)).
getConstants(_,A):-!,fail.



