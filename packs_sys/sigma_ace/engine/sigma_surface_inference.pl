-include('sigma_header.pl').


/* 
Purpose of this file:

Before a KB is canonicalized we must be able to make simple inference on

(instance AnyTerm ?What)
(domain AnyTerm ?N ?What)
(range AnyTerm ?What)
(subclass AnyTerm ?What)
(valence AnyTerm ?What)

(instance ?What Class)
(instance ?What Relation)
(instance ?What Function)
(instance ?What Predicate)
(instance ?What Attribute)

And get back the 'Simplest' answer first


Also we should be able to in some situations:

(domainSubclass AnyTerm ?N ?What)
(rangeSubclass AnyTerm ?What)



*/

:-include('sigma_header.pl').
% ================================================
% Superfulous Entry
% ================================================




inferSurfaceGuarded(Fact,KB,Ctx,Proof):-	  
	ground(Fact),!,
	inferSurface_full(Fact,KB,Ctx,Proof).
	
inferSurfaceGuarded(Fact,KB,Ctx,Proof):-	  
	not(ground(Fact)),!,
	inferSurface(Fact,KB,Ctx,Proof).
	
getFactForTransitiveClosure(KB,Ctx,Call,Proof):-
	inferSurface_easy(Call,KB,Ctx,Proof).

:-was_indexed(inferSurface_gaf(Fact,KB,Ctx,Proof)).
	
% ========================================================
% inferSurface => inferSurface_full
% ========================================================

% Non defined presently	except this redirection
inferSurface_full(Fact,KB,Ctx,Proof):-
	inferSurface(Fact,KB,Ctx,Proof).

/*
inferSurface_full(instance,false,KB,Ctx,holds(instance,E,C),P1 * P2 * crossref(instance,disjoint)):-
	     inferSurface(instance(E,M),KB,Ctx,P1),
	     inferSurface_full(disjoint(M,C),KB,Ctx,P2).

inferSurface_full_reflexive(disjoint(M,C),KB,Ctx,P2):-
		inferSurface_full(disjoint(C,M),KB,Ctx,P2).

inferSurface_full_reflexive(disjoint(M,C),KB,Ctx,P2):-
		inferSurface_full(disjoint(M,C),KB,Ctx,P2).

inferSurface_full(PredR,false,KB,Ctx,holds(instance,E,C),P1 * P2):-
	     inferSurface(instance(E,M),P1),
	     inferSurface_full(subclass,false,KB,Ctx,holds(subclass,M,C),P2),M\=C.
*/	     

% ========================================================
% Easy Forwardchains
% ========================================================

inferSurface_easy(Fact,KB,Ctx,Proof):-
	inferSurface_gaf(Fact,KB,Ctx,Proof).
	


/*
inferSurface_easy(instance(X,Class),KB,Ctx,Proofs):-
	atom(Class),inferSurface_dc_instance1(X,Class,KB,Ctx,Proofs).
*/
inferSurface_gaf_sym(Fact,KB,Ctx,P1):-inferSurface_gaf(Fact,KB,Ctx,I).
inferSurface_gaf_sym(Fact,KB,Ctx,P1):-compound(Fact),
	Fact=..[F,A,B],
	Term=..[F,B,A],
	inferSurface_gaf(Term,KB,Ctx,I).

inferSurface_gaf(Fact,KB,Ctx,surf(KB,_Ctx,TN,0)):-
	sigmaCache(Fact,_,Flags,[],KB,_Ctx,TN,Author,TMResult).
	

% ========================================================
% subclass/partition True
% ========================================================

inferSurface_easy(subclass(A,B),KB,Ctx,Proof * incode(holds(subclass,A,B),crossref(Composition,subclass))):-
	member(Composition,[disjointDecomposition,exhaustiveDecomposition,partition]),
	inferSurface_gaf(G,KB,Ctx,Proof),
	G=..[Composition,B|Children],
%	not(B='Entity'),
	member(A,Children),
%	not(A='Entity'),
	A\=B.
% ====================================================
% make_disjoint_classes(KB,Ctx)
% ===================================================

inferSurface_easy(disjoint(A,B),KB,Ctx,  Proof * incode(holds(disjoint,A,B),Composition) ):-%trace,
	member(Composition,[disjointDecomposition,exhaustiveDecomposition,partition]),
	inferSurface_gaf(G,KB,Ctx,Proof),
	G=..[Composition,C|Children],
	%`not(C='Entity'),
	member(A,Children),
	member(B,Children),
	A\=B.


% ================================================
% Normal Forwardchains
% ================================================

inferSurface(valence(Predicate,N),KB,Ctx,Proof):- 
 ( nonvar(Predicate) ->! ; true),
	inferValence(KB,Ctx,Predicate,N,Proof).

inferSurface(Fact,KB,Ctx,Proof):-
	inferSurface_easy(Fact,KB,Ctx,Proof).

inferSurface(instance(X,Class),KB,Ctx,incode(instance(X,Class),'Found in Class Constants')):-
	Class='Class',!,
	getClassesListFromKB(Rs,KB,Ctx),
	member(X,Rs).		

inferSurface(instance(X,Class),KB,Ctx,incode(instance(X,Class),'Found in Predicate Constants')):-
	Class='Predicate',!,
	getPredicatesListFromKB(Rs,KB,Ctx),
	member(X,Rs).	
	
inferSurface(instance(X,Class),KB,Ctx,incode(instance(X,Class),'Found in Attribute Constants')):-
	Class='Attribute',!,
	getAttributeNamelistFromKB(Rs,KB,Ctx),
	member(X,Rs).	

inferSurface(instance(X,Class),KB,Ctx,incode(instance(X,Class),'Found in Relation Constants')):-
	Class='Relation',!,
	getRelationsListFromKB(Rs,KB,Ctx),
	member(X,Rs).		

inferSurface(instance(X,Class),KB,Ctx,incode(instance(X,Class),'Found in Function Constants')):-
	Class='Function',!,
	getFunctionListFromKB(Rs,KB,Ctx),
	member(X,Rs).		

inferSurface(instance(E,C),KB,Ctx,P1 * P2 *  Proof):-
	inferSurface_easy(instance(E,M),KB,Ctx,P1),
	not((M=='Entity')),
	inferSurface_full(subclass(M,C),KB,Ctx,P2),
	(M\==C),
	Proof=sfindi((holds(subclass,M, C)=>forall(E,( holds(instance,E, M)=>holds(instance,E, C))))).
	     	
% ========================================================
% subrelations
% ========================================================

inferSurface(subrelation(S,C),KB,Ctx,Proof):-!,
	inferTransitiveClosure_PartialOrderingRelation(KB,Ctx,subrelation,S,C,Proof),not((S=C)).
						 
% ========================================================
% instance true
% ========================================================

inferSurface(subclass(S,C),KB,Ctx,Proof):-!,
	inferTransitiveClosure_PartialOrderingRelation(KB,Ctx,subclass,S,C,Proof),not((S=C)).

inferSurface(instance(R,C),KB,Ctx, (P1*P2*P3)):-
	inferSurface_easy(subrelation(R,P),KB,Ctx,P1),R\=P,
	inferSurface_easy(instance(C,'InheritableRelation'),KB,Ctx, P2), 
	inferSurface(instance(P,C),KB,Ctx, P3).


:-retractall(sigmaCache(_KB,_Ctx,completed_table(_))).


inferSurface(disjoint(A,B), P1 * P2 * P3 ):-
	(nonvar(A);nonvar(B)),
	inferSurface(holds(disjoint,SuperOfB,SuperOfA),P1),
	inferTransitiveClosure_PartialOrderingRelation(KB,Ctx,subclass,A,SuperOfA,P2),
	inferTransitiveClosure_PartialOrderingRelation(KB,Ctx,subclass,B,SuperOfB,P3),
	((A\=SuperOfA);(B\=SuperOfB)).
	%TODO((A=AA,B=BB);(B=AA,A=BB)).


inferSurface(domain(disjointDecomposition,N,'Class'),KB,Ctx, incode(domain(disjointDecomposition,N,'Class'))):-integer(N),!.
inferSurface(domain(exhaustiveDecomposition,N,'Class'),KB,Ctx, incode(domain(exhaustiveDecomposition,N,'Class'))):-integer(N),!.
inferSurface(domain('AssignmentFn',1,'Function'),KB,Ctx, incode(domain('AssignmentFn',1,'Function'))):-!.
inferSurface(domain('AssignmentFn',N,'Entity'),KB,Ctx, incode(domain(AssignmentFn,N,'Entity'))):-integer(N),N>1,!.
inferSurface(domain('holds',1,'Relation'),KB,Ctx, incode(domain('holds',N,'Relation'))):-!.
inferSurface(domain('holds',N,'Entity'),KB,Ctx, incode(domain('holds',N,'Entity'))):-integer(N),N>1,!.


inferSurface(domain(R,N,S),KB,Ctx,(P1 * P2 * Proof)):-
	inferSurface_full(subrelation(R,P),KB,Ctx,P1),
	R\=P,
	inferSurface_full(domain(P,N,S),KB,Ctx, P2),
	Proof=(holds(subrelation,R, P)and domain(P, N, S)and domain(R, N, S)=>holds(subclass,S, S)).

inferSurface(domain(R,N,S),KB,Ctx, P1 * Proof):-
	nonvar(N),
	inferSurface(valence(R,N),KB,Ctx,P1),
	inferSurface(range(R,S),KB,Ctx, Proof),!.

inferSurface(domain(R,N,S),KB,Ctx,P1 * Proof):-
	nonvar(N),
	inferSurface_full(subrelation(R,PredR),KB,Ctx,P1),R\=P,
	inferSurface(valence(P,N),KB,Ctx,_),
	inferSurface(range(P,S),KB,Ctx, Proof),!.
	

inferSurface(domainSubclass(R,N,S),KB,Ctx,(P1 * P2 * Proof)):-
	inferSurface_full(subrelation(R,P),KB,Ctx,P1),
	R\=P,
	inferSurface_full(domainSubclass(P,N,S),KB,Ctx, P2),
	Proof=(holds(subrelation,R, P)and domainSubclass(P, N, S)and domainSubclass(R, N, S)=>holds(subclass,S, S)).

inferSurface(domainSubclass(R,N,S),KB,Ctx, P1 * Proof):-
	nonvar(N),
	inferSurface(valence(R,N),KB,Ctx,P1),
	inferSurface(rangeSubclass(R,S),KB,Ctx, Proof),!.

inferSurface(domainSubclass(R,N,S),KB,Ctx,P1 * Proof):-
	nonvar(N),
	inferSurface_full(subrelation(R,PredR),KB,Ctx,P1),R\=P,
	inferSurface(valence(P,N),KB,Ctx,_),
	inferSurface(rangeSubclass(P,S),KB,Ctx, Proof),!.

inferSurface(domain(R,N,'Class'),KB,Ctx, Proof):-
	nonvar(N),nonvar(R),
	inferSurface(domainSubclass(R,N,S),KB,Ctx, Proof),!.

inferSurface(range(R,'Class'),KB,Ctx, Proof):-
	nonvar(R),
	inferSurface(rangeSubclass(R,N,S),KB,Ctx, Proof),!.


% ==========================================================
% Instance Deduction (Specialization) via Domains
% ==========================================================
%domain(A, 2, B)=>forall(C, forall(D, holds(A, D, C)=>holds(instance,C, B)))

expireOptimizationsInKB(KB,Ctx,Assertion):-
	%writeDebug(yellow,'erasing instancell'),
	retractall((sigmaCache(KB,_Ctx,inferInstanceTable(Class,Set)))).

	
instance_all(KB,Ctx,Predicate,Args):-
	inferSurfaceDomainVector(N,Predicate,VectS,KB,Ctx,Proofs),
	inferPossibleInstancesFromClasslist(KB,Ctx,VectS,Args).

inferPossibleInstancesFromClasslist(KB,Ctx,[],[]).
inferPossibleInstancesFromClasslist(KB,Ctx,[Class|Classes],[Arg|ArgS]):-
	inferInstanceTable(KB,Ctx,Arg,Class,_),
	inferPossibleInstancesFromClasslist(KB,Ctx,Classes,ArgS).
	


inferInstanceTable(KB,Ctx,Arg,'Enitity',_):-
	writeDebug(red,inferInstanceTable(KB,Ctx,Arg,'Enitity')),!,fail.

inferInstanceTable(KB,Ctx,Arg,'Class',incode(instance(Arg,'Class'),'Found in Class Constants')):-
	getClassesListFromKB(Rs,KB,Ctx),!,
	member(Arg,Rs).

inferInstanceTable(KB,Ctx,Arg,'Relation',incode(instance(Arg,'Relation'),'Found in Relation Constants')):-
	getRelationsListFromKB(Rs,KB,Ctx),!,
	member(Arg,Rs).

inferInstanceTable(KB,Ctx,Arg,'Predicate',incode(instance(Arg,'Predicate'),'Found in Predicate Constants')):-
	getPredicatesListFromKB(Rs,KB,Ctx),!,
	member(Arg,Rs).

inferInstanceTable(KB,Ctx,Arg,'Function',incode(instance(Arg,'Function'),'Found in Function Constants')):-
	getFunctionListFromKB(Rs,KB,Ctx),!,
	member(Arg,Rs).
	
inferInstanceTable(KB,Ctx,Arg,'Attribute',incode(instance(Arg,'Attribute'),'Found in Attribute Constants')):-
	getAttributeNamelistFromKB(Rs,KB,Ctx),!,
	member(Arg,Rs).
	

inferInstanceTable(KB,Ctx,Arg,Class,incode(instance(Arg,Class),'Found in Defined Constants')):-
	sigmaCache(KB,_Ctx,inferInstanceTable(Class,Set)),
	%writeDebug(silver,extent(holds(instance,Class,Set))),
	!,
	member(Arg,Set).
	
inferInstanceTable(KB,Ctx,Arg,Class,incode(instance(Arg,Class),'Found in Defined Cached Constants')):-
	atom(Class),
	findall(A,
			inferSurface(instance(Arg,Class),KB,Ctx,Proof),
		List),sort(List,Set),!,
	asserta(sigmaCache(KB,_Ctx,inferInstanceTable(Class,Set))),
	writeDebug(green,made_extent(KB,Ctx,inferInstanceTable(Class,Set))),!,
	member(Arg,Set).
     


expireOptimizationsInKB(KB,Ctx,Assertion):-
		writeDebug(yellow,'erasing inferInstanceTable'),
		retractall((sigmaCache(KB,_Ctx,inferInstanceTable(Class,Set)))).
	


getRelationsListFromKB:-retractall(sigmaCache(KB,_Ctx,relation_list(Rs))),!,getRelationsListFromKB(Rs,KB,Ctx),write(Rs).


getRelationsListFromKB(Rs,KB,Ctx):-
	sigmaCache(KB,_Ctx,relation_list(Rs)),!.
getRelationsListFromKB(Rs,KB,Ctx):-
	findall(X,
		(
			(
				(
					(
						inferTransitiveClosure_PartialOrderingRelation(KB,Ctx,subclass,M,'Relation',SCProof),
						inferSurface_gaf(instance(X,M),KB,Ctx,Proof)
					);
					(
					inferSurface_gaf(GAF,KB,Ctx,_Proof),
					((
					  GAF=subrelation(X,_);GAF=subrelation(_,X);
					  GAF=inverse(X,_);GAF=inverse(_,X);
					  GAF=range(X,_);GAF=domain(X,1,_);GAF=domainSubclass(X,_,_);GAF=rangeSubclass(X,_,_)
					  
					  ))
				)
			),
			atom(X)
		)),Relations),
	sort(Relations,Rs),
	asserta(sigmaCache(KB,_Ctx,relation_list(Rs))),!.

getAttributeNamelistFromKB(Rs,KB,Ctx):-
	sigmaCache(KB,_Ctx,attribute_list(Rs)),!.
getAttributeNamelistFromKB(Rs,KB,Ctx):-
	findall(X,
		(
			(
				(
					(
						inferTransitiveClosure_PartialOrderingRelation(KB,Ctx,subclass,M,'Attribute',SCProof),
						inferSurface_gaf(instance(X,M),KB,Ctx,Proof)
					);
					(
					inferSurface_gaf(GAF,KB,Ctx,_Proof),
					((GAF=subAttribute(X,_);
						GAF=subAttribute(_,X);
							GAF=successorAttribute(X,_);
								GAF=successorAttribute(_,X);
									GAF=contraryProperty(X,_);
									GAF=contraryProperty(_,X);
									GAF=attribute(X,_)))
				)                                   
			),
			atom(X)
		)),Attributes),
	sort(Attributes,Rs),
	asserta(sigmaCache(KB,_Ctx,attribute_list(Rs))),!.


getPredicatesListFromKB(Rs,KB,Ctx):-
	sigmaCache(KB,_Ctx,predicatesListFromKB(Rs)),!.
getPredicatesListFromKB(Rs,KB,Ctx):-
	getRelationsListFromKB(RR,KB,Ctx),!,
	findall(X,
		((
		member(X,RR),
		not(atom_concat(_,'Fn',X))
		)),Relations),
	sort(Relations,Rs),
	asserta(sigmaCache(KB,_Ctx,predicatesListFromKB(Rs))),!.

getFunctionListFromKB(Rs,KB,Ctx):-
	sigmaCache(KB,_Ctx,functionListFromKB(Rs)),!.
getFunctionListFromKB(Rs,KB,Ctx):-
	getRelationsListFromKB(RR,KB,Ctx),!,
	findall(X,
		((
		member(X,RR),
		atom_concat(_,'Fn',X)
		)),Relations),
	sort(Relations,Rs),
	asserta(sigmaCache(KB,_Ctx,functionListFromKB(Rs))),!.

getClassesListFromKB(Rs,KB,Ctx):-
	sigmaCache(KB,_Ctx,classListFromKB(Rs)),!.
getClassesListFromKB(Rs,KB,Ctx):-
	findall(X,
		((
					inferSurface_gaf(GAF,KB,Ctx,_Proof),
					((GAF=subclass(_,X);GAF=subclass(X,_);GAF=disjoint(_,X);GAF=disjoint(X,_);GAF=instance(_,X);GAF=range(_,X);
						inferSubclassBySurfacePartition(GAF,Classes)
					)),nonvar(X))),Classes),
	sort(Classes,Rs),
	asserta(sigmaCache(KB,_Ctx,classListFromKB(Rs))),!.
			
inferSubclassBySurfacePartition(GAF,A):-
	GAF=..[Composition|Children],
	member(Composition,[disjointDecomposition,exhaustiveDecomposition,partition]),
	member(A,Children).
/*
inferSurface_dc_instance(X,Class,KB,Ctx,Proofs  * SCProof):-%var(X),var(Class),!,
	atom(Class),
	inferSurface_dc_instance1(X,M,KB,Ctx,Proofs),
        inferTransitiveClosure_PartialOrderingRelation(KB,Ctx,subclass,M,Class,SCProof).
*/
       /*
inferSurface_dc_instance1(X,Class,KB,Ctx,argOf(Predicate,N,X)):-%var(X),var(Class),!,
	inferSurfaceDomainVector(_Arity,Predicate,VectS,KB,Ctx,Proofs),
	not(atom_concat(_,'Fn',Predicate)),
	nth1(N,VectS,Class),
	inferSurfaceEntityInRelationArg(N,Predicate,X,KB,Ctx,ProofO).
	 */

% ==================================================
% inferSurfaceEntityInRelationArg(N,Predicate,VectS,KB,Ctx,ProofO)
%
% (un)precacheSurfaceEntityInRelationArg(KB,Ctx)
% ==================================================

inferSurfaceEntityInRelationArg(N,Predicate,X,KB,Ctx,ProofO):-
	precacheSurfaceEntityInRelationArg(KB,Ctx),!,
	sigmaCache(KB,_Ctx,arg_vector(Predicate,N,X,ProofO)).
	
precacheSurfaceEntityInRelationArg(KB,Ctx):-sigmaCache(KB,_Ctx,done(precacheSurfaceEntityInRelationArg)),!.

/*
precacheSurfaceEntityInRelationArg(KB,Ctx):-
	sigmaCache(Fact, surface, KB,Ctx,  Proof),
	precacheSurfaceEntityInRelationArg_util(Fact,Classification,KB,Ctx,Proof),fail.

precacheSurfaceEntityInRelationArg(KB,Ctx):-
	sigmaCache(Predicate,HLFact,HLConds,_,  KB,Ctx,  Proof),
	precacheSurfaceEntityInRelationArg_util(entails(HLFact,HLConds),Classification,KB,Ctx,Proof),fail.
*/
	
precacheSurfaceEntityInRelationArg(KB,Ctx):-assert(sigmaCache(KB,_Ctx,done(precacheSurfaceEntityInRelationArg))),!.
		
expireSurfaceEntityInRelationArg(KB,Ctx):-
	 retractall(sigmaCache(KB,_Ctx,arg_vector(Predicate,N,VectS,_))),
	  retractall(sigmaCache(KB,_Ctx,done(precacheSurfaceEntityInRelationArg))).

% ==================================================
% Memorize Constants
%		precacheSurfaceEntityInRelationArg_util(entails(HLFact,HLConds),Classification,KB,Ctx,Proof),
%		precacheSurfaceEntityInRelationArg_util(HLFact,Classification,KB,Ctx,Proof),
% ==================================================

precacheSurfaceEntityInRelationArg_util(A,Classification,KB,Ctx,Proof):-(isSlot(A);string(A);atom(A);number(A)),!.

precacheSurfaceEntityInRelationArg_util([H|T],Classification,KB,Ctx,Proof):-!,
	precacheSurfaceEntityInRelationArg_util(H,Classification,KB,Ctx,Proof),!,
	precacheSurfaceEntityInRelationArg_util(T,Classification,KB,Ctx,Proof).
precacheSurfaceEntityInRelationArg_util(Formula,Classification,KB,Ctx,Proof):-
		Formula=..[holds,P|Args],!,
		precacheSurfaceEntityInRelationArg_util_holdsN(P,1,Args,Classification,KB,Ctx,Proof).
precacheSurfaceEntityInRelationArg_util(skolem(_,_),Classification,KB,Ctx,Proof):-!.
precacheSurfaceEntityInRelationArg_util(Formula,Classification,KB,Ctx,Proof):-
		Formula=..[_|Args],!,
		precacheSurfaceEntityInRelationArg_util(Args,Classification,KB,Ctx,Proof).
	
precacheSurfaceEntityInRelationArg_nosaveRelation(precacheSurfaceEntityInRelationArg_nosaveRelation). %dummy 

precacheSurfaceEntityInRelationArg_util_holdsN(P,N,_,Classification,KB,Ctx,Proof):-precacheSurfaceEntityInRelationArg_nosaveRelation(P),!.
precacheSurfaceEntityInRelationArg_util_holdsN(P,N,[],Classification,KB,Ctx,Proof).
precacheSurfaceEntityInRelationArg_util_holdsN(P,N,[A|RGS],Classification,KB,Ctx,Proof):-
	precacheSurfaceEntityInRelationArg_argN(PredR,P,N,A,Classification,KB,Ctx,Proof),
	NN is N+1,
	precacheSurfaceEntityInRelationArg_util_holdsN(P,NN,RGS,Classification,KB,Ctx,Proof).
	

precacheSurfaceEntityInRelationArg_argN(PredR,P,N,A,Classification,KB,Ctx,Proof):-non_memerable(A),!.
precacheSurfaceEntityInRelationArg_argN(PredR,Predicate,N,VectS,Classification,KB,Ctx,Proof):-
	sigmaCache(KB,_Ctx,arg_vector(Predicate,N,VectS,_)),!.
precacheSurfaceEntityInRelationArg_argN(PredR,Predicate,N,VectS,Classification,KB,Ctx,Proof):-
	asserta(sigmaCache(KB,_Ctx,arg_vector(Predicate,N,VectS,Proof))),!.

non_memerable(A):-isSlot(A).
%non_memerable(A):-number(A).
%non_memerable(A):-string(A).
%non_memerable(A):-not(atom(A)),!,fail.
%non_memerable(A):-atom_codes(A,[34|_]).

	

%reif(Var):-var(Var),!,fail.
%reif('Entity'):-!,fail.
reif('Formula'):-!,fail.
%reif(N):-number(N),!,fail.
reif(_):-!.

%extentable('Entity'):-!,fail.
%extentable('Entity'):-!,fail.
%extentable('Formula'):-!,fail.
%extentable('SymbolicString'):-!,fail.
%extentable(Var):-var(Var),!,fail.
%extentable(_):-!.



expireOptimizationsInKB(KB,Ctx,Assertion):-
		writeDebug(silver,'destroy_valence_vectors/expireDomainsListForRelation(KB,Ctx)/expireSurfaceEntityInRelationArg(KB,Ctx)'),
		retractall(sigmaCache(KB,_Ctx,functionListFromKB(Rs))),
		retractall(sigmaCache(KB,_Ctx,relation_list(Rs))),
		retractall(sigmaCache(KB,_Ctx,predicatesListFromKB(Rs))),
		retractall(sigmaCache(KB,_Ctx,classListFromKB(Rs))),
		retractall(sigmaCache(KB,_Ctx,attribute_list(Rs))),
		expireSurfaceEntityInRelationArg(KB,Ctx),
		expireDomainsListForRelation(KB,Ctx),
		precacheSurfaceEntityInRelationArg(KB,Ctx).
		



% Get Domain (Will return all predicates and arity with proof of how it was derived)

inferSurfaceDomainVector(N,Predicate,VectS,KB,Ctx,ProofO):-
	buildDomainsListForRelation(KB,Ctx),!,
	sigmaCache(KB,_Ctx,domain_vector(Predicate,N,VectS,ProofO)).
	
buildDomainsListForRelation(KB,Ctx):-sigmaCache(KB,_Ctx,done(buildDomainsListForRelation)),!.
buildDomainsListForRelation(KB,Ctx):- 
	once(getRelationsListFromKB(Rs,KB,Ctx)),
	member(Predicate,Rs),  %trace, 	
	once(inferValence(KB,Ctx,Predicate,N,_Proof)),
	once(inferHoldsNDomVect(N,Predicate,VectS,KB,Ctx)),
	%writeDebug(green,domain_vector(Predicate,N,VectS)),
	asserta_if_new(sigmaCache(KB,_Ctx,domain_vector(Predicate,N,VectS,cached))),fail.
buildDomainsListForRelation(KB,Ctx):-assert(sigmaCache(KB,_Ctx,done(buildDomainsListForRelation))),!.
		
expireDomainsListForRelation(KB,Ctx):-
	 retractall(sigmaCache(KB,_Ctx,domain_vector(Predicate,N,VectS,cached))),
	  retractall(sigmaCache(KB,_Ctx,done(buildDomainsListForRelation))).


% assert cache/2 
inferSurface_domain(Predicate,N,Class,KB,Ctx):-
	atom(Predicate),
	inferSurface(domain(Predicate,N,Class),KB,Ctx,Proof2).

:-was_indexed(inferHoldsNDomVect(1,0,1,0,0)).


inferHoldsNDomVect(2,Predicate,[Class1,Class2],KB,Ctx):-!,
	inferSurface_domain(Predicate,1,Class1,KB,Ctx),!,
	inferSurface_domain(Predicate,2,Class2,KB,Ctx),!.

inferHoldsNDomVect(3,Predicate,[Class1,Class2,Class3],KB,Ctx):-!,
	inferSurface_domain(Predicate,1,Class1,KB,Ctx),!,
	inferSurface_domain(Predicate,2,Class2,KB,Ctx),!,
	inferSurface_domain(Predicate,3,Class3,KB,Ctx),!.

inferHoldsNDomVect(1,Predicate,[Class1],KB,Ctx):-!,
	inferSurface_domain(Predicate,1,Class1,KB,Ctx),!.

inferHoldsNDomVect(4,Predicate,[Class1,Class2,Class3,Class4],KB,Ctx):-!,
	inferSurface_domain(Predicate,1,Class1,KB,Ctx),!,
	inferSurface_domain(Predicate,2,Class2,KB,Ctx),!,
	inferSurface_domain(Predicate,3,Class3,KB,Ctx),!,
	inferSurface_domain(Predicate,4,Class4,KB,Ctx),!.

inferHoldsNDomVect(5,Predicate,[Class1,Class2,Class3,Class4,Class5],KB,Ctx):-!,
	inferSurface_domain(Predicate,1,Class1,KB,Ctx),!,
	inferSurface_domain(Predicate,2,Class2,KB,Ctx),!,
	inferSurface_domain(Predicate,3,Class3,KB,Ctx),!,
	inferSurface_domain(Predicate,4,Class4,KB,Ctx),!,
	inferSurface_domain(Predicate,5,Class5,KB,Ctx),!.

 

isPropositional(A):-isSlot(A),!.
isPropositional(not(A)):-!,isPropositional(A).
isPropositional(forall(_,A)):-!,isPropositional(A).
isPropositional(exists(_,A)):-!,isPropositional(A).
isPropositional(poss(A)):-!,isPropositional(A).
isPropositional(nec(A)):-!,isPropositional(A).
isPropositional(<=>(A,B)):-!,isPropositional(A),isPropositional(B).
isPropositional(=>(A,B)):-!,isPropositional(A),isPropositional(B).
isPropositional(or(A,B)):-!,isPropositional(A),isPropositional(B).
isPropositional(and(A,B)):-!,isPropositional(A),isPropositional(B).
isPropositional(A):-A=..[P|_],is_instance_of(P,C),is_subclass_of(C,'Predicate').

