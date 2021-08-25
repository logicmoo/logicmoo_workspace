% ===================================================================
% File 'sigma_arg_domains.pl' 
% Authors: Douglas Miles
% Contact: dmiles@teknowledge.com ;apease@teknowledge.com
% Version: 'sigma_chaining.pl' 1.0.0 
% History:
% Created - 2000/11/10 dmiles@teknowledge.com
% ===================================================================
  
:-include('sigma_header.pl').

/*
This File only exports one Predicate called: putAttributeStructures/4 and the rest of the predicates are helper to putAttributeStructures/2
                                                                                                                                                                
It does two things : Adds arg and predicate type checking .


ProtoType: putAttributeStructures(Surface,KRVars,Rule,KB,TN,CLID, ProofID,Anontate,Flags,IN_UnconstrainedFormua,OUT_UnconstrainedFormua)

1) if a variable is found in the head of a clause it confirms that it is contrained by a argument type checking

2) if a varaible is found in the predicate position (Head or Body) it rewrites the predicate to use functsymbol (list of Hiloged predicates) 

                                                                                  
Examples:
                                                                                  
   | ?- putAttributeStructures(Surface,KRVars,Rule,KB,TN,CLID, ProofID,Anontate,Flags,('instance'(R,agent) :-'instance'(R,human)),S).
   R = _h99
   S = ('instance'(_h99,agent) :- 'instance'(_h99,human))
   
   | ?- putAttributeStructures(Surface,KRVars,Rule,KB,TN,CLID, ProofID,Anontate,Flags,('instance'(R,agent):-performs(R,task1)),S).
   R = _h95
   S = ('instance'(_h95,agent) :- 'domain'(performs,1,_h95)  ','  performs(_h95,task1))
   
   | ?- putAttributeStructures(Surface,KRVars,Rule,KB,TN,CLID, ProofID,Anontate,Flags,('instance'(R,agent):-performs(R,Act)),S).
   R = _h95
   Act = _h141
   S = ('instance'(_h95,agent) :- 'domain'(performs,1,_h95)  ','  'domain'(performs,2,_h141)  ','  performs(_h95,_h141))

*/

% ===============================================
% putAttributeStructures: Has Two Phases
% ===============================================
	
putAttributeStructures(KB,Anontate,Flags,Formula,SlotStructure):-
		putPropositionAttributes(KB,Anontate,Flags,'=>',1,Formula,SlotStructure),!.



localCanonicalizerNotice(Type,Details):-
       getSigmaOption(putAttributeStructures,Surface:Rule:CLID:Flags:KRVars:KB:Ctx:TN),
       % writeq(Warning:Data),nl,
       (not(not(canonicalizerWarnings(KB,Type,Details)))  -> ifInteractive(write(','));
	(assertz(canonicalizerWarnings(KB,Type,Details)),
	ifInteractive(writeObject(nv([nl,Details,nl,Type,nl,Surface,nl,nl]),KRVars)))),!.


localCanonicalizerWarning(Type,Details):-
	localCanonicalizerNotice(Type,Details).

localCanonicalizerError(Type,Details):-
	throwSigma(argDomains,error:Type,Details).


putPropositionAttributes(KB,Anontate,Flags,Caller,ArgN,Var,Slot):-isVarProlog(Var),!,
	getDomainsForVar(Caller,ArgN,Var,Flags,Domains),
	getPutBestConstraintAttribute(Caller,ArgN,KB,Anontate,Var,Domains,Slot),!.

putPropositionAttributes(KB,Anontate,Flags,Caller,ArgN,ArgIn,ArgIn):-isSlot(ArgIn),!.

/*
putPropositionAttributes(KB,Anontate,Flags,Caller,ArgN,'$existential'(ArgIn,Lit,Formula),SlotStructure):-!,
	getDomainsForVar(Caller,ArgN,ArgIn,Flags,[Caller:ArgN|Domains]),
	getPutBestConstraintAttribute(Caller,ArgN,KB,Anontate,'$existential'(Lit,Formula),Domains,SlotStructure),!.
*/
	

getDomainsForVar(Caller,ArgN,ArgIn,Flags,Domains):-!,member(domainV(Var,Domains),Flags),Var==ArgIn,!.
getDomainsForVar(Caller,ArgN,ArgIn,Flags,[]):-!,write(getDomainsForVar(Caller,ArgN,ArgIn,Flags,[])),trace.

putPropositionAttributes(KB,Anontate,Flags,Caller,ArgN,[],[]):-!.

putPropositionAttributes(KB,Anontate,Flags,Caller,ArgN,[ArgIn|B],[AA|BB]):-!,
	putPropositionAttributes(KB,Anontate,Flags,Caller,ArgN,ArgIn,AA),
	putPropositionAttributes(KB,Anontate,Flags,Caller,ArgN,B,BB),!.

putPropositionAttributes(KB,Anontate,Flags,Caller,ArgN,string(ArgIn),SlotStructure):-!,nonvar(ArgIn),!,
	 putStringAttribute(ArgIn,Caller,ArgN,SlotStructure),!.

putPropositionAttributes(KB,Anontate,Flags,Caller,ArgN,ArgIn,SlotStructure):-atom(ArgIn),!,
	putAtomAttributeProc(KB,Anontate,Flags,Caller,ArgN,ArgIn,SlotStructure),!.

putPropositionAttributes(KB,Anontate,Flags,Caller,ArgN,ArgIn,SlotStructure):-number(ArgIn),!,
	inferClassesForDomainVList([Caller:ArgN],Classes),!,
	sort(Classes,ClassesM), % removes duplicates
	delete(ClassesM,'Entity',ClassesMM), % Entity too general
	getListOfConstraintStructuresFromListOfClasses(ArgIn,ClassesMM,LCM),
	mostConstrainingAttribute(LCM,TightestFit),
	applyNumericFit(Caller,ArgN,TightestFit,ArgIn,SlotStructure),!.
	
		applyNumericFit(Caller,ArgN,TightestFit,ArgIn,SlotStructure):-TightestFit=[_,_,_,_|_],!,
			applyAttributeStructureToEntity(Caller,ArgN,ArgIn,TightestFit,SlotStructure),!.
			
		applyNumericFit(Caller,ArgN,_,ArgIn,SlotStructure):-!,
			applyAttributeStructureToEntity(Caller,ArgN,ArgIn,['Abstract','Quantity','Number'],SlotStructure).

putPropositionAttributes(KB,Anontate,Flags,Caller,ArgN,Term,OTerm):-
	Term=..[Funct|Args],
	putPropositionAttributesFunctor(KB,Anontate,Flags,Caller,ArgN,Funct,Args,OTerm),!.


% Connectives We deal with each argument	
putPropositionAttributesFunctor(KB,Anontate,Flags,Caller,ArgN,Funct,Args,OTerm):-
	(memberchk(Funct,[and,or,not,possible,searchable,impossible,absurd,proves,prove,xor,
	'<=>','=>',entails,(:-),',',';',ifThen]);
	atom_concat(_,'_',Funct)),!,
	putPropositionAttributes(KB,Anontate,Flags,and,1,Args,ArgsO),
	OTerm=..[Funct|ArgsO],!.

% Connectives We Skip all but the first argument
putPropositionAttributesFunctor(KB,Anontate,Flags,Caller,ArgN,Funct,[ArgIn|B],OTerm):-
	(atom_concat('t~',_,Funct);atom_concat('f~',_,Funct)),!,
	putPropositionAttributes(KB,Anontate,Flags,Caller,ArgN,ArgIn,AA),
	OTerm=..[Funct,AA|B],!.

% Functors That we dont touch Any Arguments
putPropositionAttributesFunctor(KB,Anontate,Flags,Caller,ArgN,Funct,Args,OTerm):-
	(atom_concat('$',_,Funct);memberchk(Funct,['v','$existential','include-context'])),!,
	OTerm=..[Funct|Args],!.

% Functors That Are Variable Arity That we cannot hash the arg length
putPropositionAttributesFunctor(KB,Anontate,Flags,Caller,ArgN,Funct,Args,OTerm):-
	memberchk(Funct,[disjointDecomposition,exhaustiveDecomposition,partition]),!,
	putPropositionAttributes(KB,Anontate,Flags,Funct,1,Args,ArgsO),!,
	OTerm=..[Funct,ArgsO],!.

% Functors That Are Variable Arity That we *CAN* hash the arg length
putPropositionAttributesFunctor(KB,Anontate,Flags,Caller,ArgN,Funct,[Arg1|Args],OTerm):-
	memberchk(Funct,[holds,function,'AssignmentFn']),!,
	putPropositionAttributesArguments(KB,Anontate,Flags,Funct,1,[Arg1],[SlotedF]),!,
	putPropositionAttributesArguments(KB,Anontate,Flags,Arg1,1,Args,ArgsO),!,  
	OTerm=..[Funct,SlotedF|ArgsO],!.

putPropositionAttributesFunctor(KB,Anontate,Flags,Caller,ArgN,Funct,Args,TermO):-
	putPropositionAttributesArguments(KB,Anontate,Flags,Funct,1,Args,ArgsO),!,
	TermO=..[Funct|ArgsO].


putPropositionAttributesArguments(KB,Anontate,Flags,Caller,N,[],[]):-!.
putPropositionAttributesArguments(KB,Anontate,Flags,Caller,N,[Arg|ArgS],[ArgO|ArgsO]):-
	     putPropositionAttributes(KB,Anontate,Flags,Caller,N,Arg,ArgO),NN is N+1,
	     putPropositionAttributesArguments(KB,Anontate,Flags,Caller,NN,ArgS,ArgsO),!.

	
% ===============================================
% Slot Atom
% ===============================================

%:-dynamic(putAtomAttributeProc/6).

putAtomAttribute(KB,Anontate,Flags,Caller,ArgN,ArgIn,SlotStructure):-
	var(ArgIn),write(putAtomAttribute(KB,Anontate,Flags,Caller,ArgN,ArgIn,SlotStructure)),trace,fail.

% Quoted Atom
putAtomAttribute(KB,Anontate,Flags,Caller,ArgN,ArgIn,SlotStructure):-atom_concat('"',_,ArgIn),!,
	putStringAttribute((ArgIn),Caller,ArgN,SlotStructure),!.

/*
putAtomAttribute(KB,Anontate,Flags,Caller,ArgN,ArgIn,SlotStructure):-atom(ArgIn),	
	sigmaCache(KB,Anontate,putAtomAttributeProc(ArgIn,SlotStructure)),!.

*/
putAtomAttribute(KB,Anontate,Flags,Caller,ArgN,ArgIn,SlotStructure):-
	putAtomAttributeProc(KB,Anontate,Flags,Caller,ArgN,ArgIn,SlotStructure),!.
%	asserta(sigmaCache(KB,Anontate,putAtomAttributeProc(ArgIn,SlotStructure))),!.

       
% Class name
putAtomAttributeProc(KB,Anontate,Flags,Caller,ArgN,ArgIn,'$Class'(ArgIn)):-
	getClassesListFromKB(Rs,Ctx,KB),identical_member(ArgIn,Rs),!.
	
/*
	putClassEntityAttributes(KB,Anontate,Flags,Caller,ArgN,ArgIn,SlotStructure),!.
	
% Classes use special wrapper
putClassEntityAttributes(KB,Anontate,Flags,Caller,ArgN,ArgIn,ArgIn):-!.

	getConstraintStructureForClass(ArgIn,'Class',Slot),!,
	%inferSingleSubclassPathList(ArgIn,ArgIn,'Entity',SlotStructure)),!,
	%reverse(ReversedSlots,Slot),
	applyAttributeStructureToEntity(Caller,ArgN,ArgIn,['Abstract','Class'|_],SlotStructure),!.
*/
  
% Any Name
putAtomAttributeProc(KB,Anontate,Flags,Caller,ArgN,ArgIn,SlotStructure):-
	inferClassFromEntity(KB,Anontate,ArgIn,Class),nonvar(Class),!,
	getConstraintStructureForClass(ArgIn,Class,Slot),
	applyAttributeStructureToEntity(Caller,ArgN,ArgIn,Slot,SlotStructure),!.
					 
% Function Name
putAtomAttributeProc(KB,Anontate,Flags,Caller,ArgN,ArgIn,SlotStructure):-
	getFunctionListFromKB(Rs,Ctx,KB),memberchk(ArgIn,Rs),!,
	inferClassFromEntity(KB,Anontate,ArgIn,Class),nonvar(Class),!,
	getConstraintStructureForClass(ArgIn,Class,Slot),
	applyAttributeStructureToEntity(Caller,ArgN,ArgIn,Slot,SlotStructure),!.

% Predicate Name
putAtomAttributeProc(KB,Anontate,Flags,Caller,ArgN,ArgIn,SlotStructure):-
	getPredicatesListFromKB(Rs,Ctx,KB),memberchk(ArgIn,Rs),!,
	applyAttributeStructureToEntity(Caller,ArgN,ArgIn,['Abstract', 'Relation', 'Predicate'],SlotStructure).

% Attribute Name
putAtomAttributeProc(KB,Anontate,Flags,Caller,ArgN,ArgIn,SlotStructure):-
	getAttributeNamelistFromKB(Rs,Ctx,KB),memberchk(ArgIn,Rs),!,
	applyAttributeStructureToEntity(Caller,ArgN,ArgIn,[ 'Abstract', 'Attribute'],SlotStructure).

% Assume By Previous Holder Predicate

putAtomAttributeProc(KB,Anontate,Flags,Caller,ArgN,ArgIn,SlotStructure):-
	ground(ArgIn),
	sigmaCache(KB,Anontate,classFromWrap(ArgIn,Class)),!,
	getConstraintStructureForClass(ArgIn,Class,Slot),
	applyAttributeStructureToEntity(Caller,ArgN,ArgIn,Slot,SlotStructure),!.


% Assume By Holder Predicate
putAtomAttributeProc(KB,Anontate,Flags,Caller,ArgN,ArgIn,SlotedA):-
	ground(ArgIn),
	once(getPutBestConstraintAttribute(Caller,ArgN,KB,Anontate,ArgIn,[Caller:ArgN],SlotedA)),
	getClassFromWrap(SlotedA,Assumed),!,
	localCanonicalizerWarning('Assumed Term From Domain ','=>'(domain(Caller,ArgN,Assumed),instance(ArgIn,Assumed))),!,
	(ground(classFromWrap(ArgIn,Assumed)) ->asserta(sigmaCache(KB,Anontate,classFromWrap(ArgIn,Assumed))) ;true).

% Assume New Physical
putAtomAttributeProc(KB,Anontate,Flags,Caller,ArgN,ArgIn,'$Object'(ArgIn,_)):-nonvar(ArgIn),!,
	localCanonicalizerWarning('Assumed Term (No domain available) ',
		and(domain(Caller,ArgN,'?UNKNOWN'),instance(ArgIn,'Object'))),!.

	
putStringAttribute(Lit,Caller,ArgN,SlotStructure):-
	applyAttributeStructureToEntity(Caller,ArgN,string(Lit),['Physical','Object','ContentBearingObject'],SlotStructure),!.


inferClassFromEntity(KB,Anontate,Entity,Class):-
%	inferTransitiveClosure_PartialOrderingRelation(KB,Anontate,Agent3,subclass,M,Class,SCProof),
	inferSurface_gaf(instance(Entity,Class),_UAgent,KB,Proof),not(Class='InheritableRelation'),!.
	
getPutBestConstraintAttribute(Caller,ArgN,KB,Anontate,Var,Domains,SlotStructure):-
	inferClassesForDomainVList(Domains,Classes),!,
	sort(Classes,ClassesM), % removes duplicates
	delete(ClassesM,'Entity',ClassesMM), % Entity too general
	getListOfConstraintStructuresFromListOfClasses(Var,ClassesMM,LCM),
	mostConstrainingAttribute(LCM,TightestFit),
	applyAttributeStructureToEntity(Caller,ArgN,Var,TightestFit,SlotStructure),!.

% getClassFromWrap(Slot,Class).

getClassFromWrap(ArgIn,'Entity'):-isPrologVar(ArgIN),!.
getClassFromWrap('$Class'(ArgIn),'Class'):-!.
getClassFromWrap(Slot,Class):-
	functor(Slot,F,A),atom_concat('$',_,F),
	arg(2,Slot,ClassesTerm),!,
	getClassBottemFromTerm(ClassesTerm,Class).
	
getClassBottemFromTerm(ClassesTerm,ClassO):-
	ClassesTerm=..[Class,Var], 
	(isPrologVar(Var) -> 
		ClassO = Class ;
		getClassBottemFromTerm(Var,ClassO)),!.
	
mostConstrainingAttribute([],[]):-!. % none
mostConstrainingAttribute([ArgIn],ArgIn):-!. %only one
mostConstrainingAttribute(List,Longest):-lengthSort(List,[Longest|_]),!.

% Put the longest list first
lengthSort(ListOfLists,LongestFirst):-	
	predsort(lengthSort,ListOfLists,LongestFirst),!.
lengthSort(Result,List1,List2):-
	length(List1,L1),
	length(List2,L2),
	compare(Result,L2,L1).

% ===============================================    
% Build Class Path
% ===============================================

getListOfConstraintStructuresFromListOfClasses(Orig,[],[]):-!.
getListOfConstraintStructuresFromListOfClasses(Orig,[Class|More],[Choice|Classes]):-
	getConstraintStructureForClass(Orig,Class,Choice),!,
	getListOfConstraintStructuresFromListOfClasses(Orig,More,Classes),!.


getConstraintStructureForClass(Orig,V,['Entity']):-var(V),!.

getConstraintStructureForClass(Orig,'UnionFn'(Class, _),Slot):-
	getConstraintStructureForClass(Orig,Class,Slot),!.

getConstraintStructureForClass(Orig,Class,Slot):-
	inferSingleSubclassPathList(Orig,Class,'Entity',ReversedSlots),!,
	reverse(ReversedSlots,Slot).

getConstraintStructureForClass(Orig,Class,[]):-
	localCanonicalizerWarning('Problem: Class is not connected to Knowledgebase ontology',(Class)),!.
	

inferSingleSubclassPathList(Entity,Origin,Destination,Path):-not(ground((Origin,Destination))),trace,fail.

inferSingleSubclassPathList(InheritableRelation, 'InheritableRelation', _, ['Abstract','Relation']):-!.

inferSingleSubclassPathList(Entity,Origin,Destination,Path):-
		sigmaCache(KB,Anontate,inferSingleSubclassPathList(Origin,Destination,Path)),!.

inferSingleSubclassPathList(Entity,Origin,Destination,Path):-
	inferSingleSubclassPathListProc(Entity,Origin,Destination,Path),
	asserta(sigmaCache(KB,Anontate,inferSingleSubclassPathList(Origin,Destination,Path))).

inferSingleSubclassPathListProc(Entity,Origin,Origin,[]):-!.
inferSingleSubclassPathListProc(Entity,Origin,Destination,[Origin|O]):-
	inferDirectSubclass(Origin,ClassMid),
	inferSingleSubclassPathList(Entity,ClassMid,Destination,O).

inferSingleSubclassPathListProc(Entity,Origin,ClassFinal,[]):-
	localCanonicalizerError('Problem with knowledgebase: Could not deduce ',(subclass(Origin,'?CLASS'))),!.
	
inferDirectSubclass(Origin,ClassMid):-
	inferSurface_easy(subclass(Origin,ClassMid),_,Ctx,_P),
	nonvar(Origin),not((ClassMid='InheritableRelation')),!.
	
% ===============================================
% Slot Entity
% ===============================================

argVar(Arg):-catch(arg(1,Arg,$),_,true).

% Wraps Entity
applyAttributeStructureToEntity(Caller,ArgN,ArgV,[V|L],ArgV):-var(V),!,
	localCanonicalizerError('Could not retrieve domains for',domain(Caller,ArgN,ArgV)).

applyAttributeStructureToEntity(Caller,ArgN,ArgV,[],ArgV):-!,
	localCanonicalizerNotice('Specialize for rule',domain(Caller,ArgN,'Entity')),!.
	
applyAttributeStructureToEntity(Caller,ArgN,ArgV,['Entity'],ArgV):-!,
	localCanonicalizerNotice('Specialize for rule',domain(Caller,ArgN,'Entity')),!.

applyAttributeStructureToEntity(Caller,ArgN,ArgV,['Abstract','Class'|_],'$Class'(ArgV)):-!.

applyAttributeStructureToEntity(Caller,ArgN,ArgV,[Class],'$IdentityFn'(ArgV,Slot)):-!,
	Slot=..[Class,_],localCanonicalizerNotice('Specialized for rule',domain(Caller,ArgN,Class)),!.
	
applyAttributeStructureToEntity(Caller,ArgN,ArgV,[Base,Class|Remainder],Slot):-!,
	atom_concat('$',Class,DClass),
	Slot=..[DClass,ArgV,Classes],
	makeClassNodeF(Remainder,Classes),!.

applyAttributeStructureToEntity(Caller,ArgN,ArgV,Class,ArgV):-!,
	localCanonicalizerError('Unknown Structure',domain(Caller,ArgN,Class)),!.

makeClassNodeF(V,V):-isSlot(V),!.
makeClassNodeF([],_):-!.
makeClassNodeF([Super|L],Class):-
	Class=..[Super,SubClass],
	makeClassNodeF(L,SubClass),!.

% ===============================================
% Convert [Pred1:N1,Pred2:N2,...] list to Domain List
% ===============================================

inferClassesForDomainVList([],[]):-!.
inferClassesForDomainVList([Pred:N|PredN],[Class|More]):-
	inferClassForDomainV(Pred:N,Class),!,
	inferClassesForDomainVList(PredN,More).

inferClassForDomainV(Var:N,'Entity'):-var(Var),!.
inferClassForDomainV('$instanceof':Class,Class):-!.

inferClassForDomainV(Pred:N,Class):-inferSurface(domain(Pred,N,Class),_,_,_),!.
inferClassForDomainV(Pred:range,Class):-inferSurface(range(Pred,Class),_,_,_),!.

inferClassForDomainV(Pred:N,Class):-
		inferSurfaceDomainVector(Arity,Pred,VectS,Agent,KB,Anontate,_),
		nth1(N,VectS,Class),!.

inferClassForDomainV(Pred:N,Class):-inferSurface(range(Pred,Class),_,_,_),!.
		%sendNote(currentUser,'Canonicalizer','Warning While Compiling Knowledgebase (using range) ',domain(Pred,N,Class)),!.

inferClassForDomainV(Pred:range,Class):-inferSurface_gaf(domain(Pred,4,Class),_,_,_),!.
inferClassForDomainV(Pred:range,Class):-inferSurface_gaf(domain(Pred,3,Class),_,_,_),!.
inferClassForDomainV(Pred:range,Class):-inferSurface_gaf(domain(Pred,2,Class),_,_,_),!.

inferClassForDomainV(Pred:range,'Class'):-inferSurface(rangeSubclass(Pred,Class),_,_,_),!.

inferClassForDomainV(Pred:N,'Entity'):-isEntityref(Pred,_),number(N),!.
 		
inferClassForDomainV(Pred:N,'Entity'):-!,
	localCanonicalizerWarning('Warning: No Predicate Domain ',domain(Pred,N,'Entity')),!.


