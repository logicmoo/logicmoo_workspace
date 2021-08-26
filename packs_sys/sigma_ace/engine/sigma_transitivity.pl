%:-include('sigma_header.pl').

% =====================================================================================================
%% TC Database
% =====================================================================================================


:-op(600,xfx,(myUniv)).


myUniv(domainFn(A,B),[domainFn,A,B]):-!.
myUniv(rangeFn(A,B),[rangeFn,A,B]):-!.
%myUniv(holds(P,A,B),[P,A,B]):-!.
myUniv(Term,[P|AB]):-!,Term=..[P|AB].
myUniv(holds(P,AB),[P|AB]):-!.


eraseTransitiveClosureCache:-
	retractall(sigmaCache(KB,_Ctx,tc(UN,_,_))),!,
	retractall(sigmaCache(KB,_Ctx,tc(domainFn(_,UN),_),_)),!,
	retractall(sigmaCache(KB,_Ctx,tc(domainFn(UN,_),_),_)),!,
	retractall(sigmaCache(KB,_Ctx,tc(rangeFn(_,UN),_),_)),!,
	retractall(sigmaCache(KB,_Ctx,tc(rangeFn(UN,_),_),_)),!,
	writeDebug(eraseTransitiveClosureCache).


%eraseTransitiveClosureCache(KB,Ctx,Var):-expireOptimizationsInKB(KB,Ctx,Assertion),fail.

eraseTransitiveClosureCache(KB,Ctx,Var):-var(Var),!.
eraseTransitiveClosureCache(KB,Ctx,UN):-number(UN),!.
eraseTransitiveClosureCache(KB,Ctx,[]):-!.
eraseTransitiveClosureCache(KB,Ctx,[H|L]):-!,
	eraseTransitiveClosureCache(KB,Ctx,H),
	eraseTransitiveClosureCache(KB,Ctx,L),!.

eraseTransitiveClosureCache(KB,Ctx,UN):-atom(UN),
	%writeDebug(eraseTransitiveClosureCache(KB,Ctx,UN)),
	retractall(sigmaCache(KB,_Ctx,tc(UN,_,_))),!,
	retractall(sigmaCache(KB,_Ctx,tc(domainFn(_,UN),_),_)),!,
	retractall(sigmaCache(KB,_Ctx,tc(domainFn(UN,_),_),_)),!,
	retractall(sigmaCache(KB,_Ctx,tc(rangeFn(_,UN),_),_)),!,
	retractall(sigmaCache(KB,_Ctx,tc(rangeFn(UN,_),_),_)),!.
	

eraseTransitiveClosureCache(KB,Ctx,UN):-
	getConstants(atomic,UN,Consts,_,_),
	logOnFailure(eraseTransitiveClosureCache(KB,Ctx,Consts)).

writeDebug_tc(_).

storeInCacheIffNew(KB,Ctx,S,C,P):-(sigmaCache(KB,Ctx,tc(S,C),_)),!.
storeInCacheIffNew(KB,Ctx,S,C,P):-asserta(sigmaCache(KB,Ctx,tc(S,C),P)),!.

storeInCacheIffNew(KB,Ctx,S,C,P):-!.

% =====================================================================================================
%% TC for PartialOrderingRelation
% =====================================================================================================
inferTransitiveClosure_PartialOrderingRelation(KB,Ctx,Predicate,S,C,
%        sfind(instance(Predicate,'PartialOrderingRelation')) *
     	sfind(subclass('PartialOrderingRelation','TransitiveRelation'))  *
	Proof  
	):- inferTransitiveClosure_Redir(KB,Ctx,'PartialOrderingRelation',Predicate,S,C,Proof). 
	
inferTransitiveClosure_PartialOrderingRelation(KB,Ctx,Predicate,S,S,
%	sfind(instance(Predicate,'PartialOrderingRelation')) * 
	sfind(subclass('PartialOrderingRelation','ReflexiveRelation')) * 
	sfindi('=>'(instance(Predicate,'ReflexiveRelation'),
	forall(S,holds(Predicate,S,S))))):- isAxiomInKB(
	sfind(instance(Predicate,'PartialOrderingRelation')) * 
	sfind(subclass('PartialOrderingRelation','ReflexiveRelation')) * 
	sfind('=>'(instance(Predicate,'ReflexiveRelation'),
	forall(S,holds(Predicate,S,S))))).

% =====================================================================================================
%% TC for TotalOrderingRelation
% =====================================================================================================
inferTransitiveClosure_TotalOrderingRelation(KB,Ctx,Predicate,S,C,
      %  sfind(instance(Predicate,'TotalOrderingRelation')) *
 	sfind(subclass('TotalOrderingRelation','PartialOrderingRelation')) 
	):- inferTransitiveClosure_PartialOrderingRelation(KB,Ctx,Predicate,S,C,Proof). 

% =====================================================================================================
%% TC for TransitiveRelation
% =====================================================================================================
inferTransitiveClosure_TransitiveRelation(KB,Ctx,Predicate,S,C,Proof):- 
	inferTransitiveClosure_Redir(KB,Ctx,'TransitiveRelation',Predicate,S,C,Proof). 


% =====================================================================================================
%% Generic Transitive Relation
% =====================================================================================================
inferTransitiveClosure_Redir(KB,Ctx,RType,Predicate,S,C, Proof  *
	       sfindi( 
	   =>(instance(Predicate,'TransitiveRelation'),
	       forall(C,forall(D,forall(S,
	=>(and(holds(Predicate,S,D),holds(Predicate,D,C)),holds(Predicate,S,C)))))))
		):-
	    /*   isAxiomInKB( 
	   =>(instance(Predicate,'TransitiveRelation'),
	       forall(C,forall(D,forall(S,
	=>(and(holds(Predicate,S,D),holds(Predicate,D,C)),holds(Predicate,S,C))))))),!,	*/
		
		(inferTransitiveClosure_O_TransitiveRelation(KB,Ctx,Predicate,S,C,Proof)).

inferTransitiveClosure_O_TransitiveRelation(KB,Ctx,Predicate,S,C,Proof):-nonvar(C),!,
	inferTransitiveClosure_rl_TransitiveRelation(KB,Ctx,Predicate,S,C,Proof).
inferTransitiveClosure_O_TransitiveRelation(KB,Ctx,Predicate,S,C,Proof):-nonvar(S),!,
	inferTransitiveClosure_lr_TransitiveRelation(KB,Ctx,Predicate,S,C,Proof).
inferTransitiveClosure_O_TransitiveRelation(KB,Ctx,Predicate,S,C,Proof):-!,
	inferTransitiveClosure_open_TransitiveRelation(KB,Ctx,Predicate,S,C,Proof).

% =====================================================================================================
%% Generic Transitive Open 
% =====================================================================================================

inferTransitiveClosure_open_TransitiveRelation(KB,Ctx,Predicate,S,C,Proof):-
	%KeyTerm myUniv  [holdsFn,Predicate],!,
	inferTransitiveClosure_open_t_TransitiveRelation(KB,Ctx,Predicate,Predicate,S,C,Proof).

inferTransitiveClosure_open_t_TransitiveRelation(KB,Ctx,KeyTerm,Predicate,S,C,_):-
	once(table_make_inferTransitiveClosure_open_0_TransitiveRelation(KB,Ctx,KeyTerm,Predicate)),fail.
inferTransitiveClosure_open_t_TransitiveRelation(KB,Ctx,KeyTerm,Predicate,S,C,P):-
	DataTerm myUniv  [Predicate,S,C],
	sigmaCache(KB,_Ctx,tc(Predicate,DataTerm),P).

table_make_inferTransitiveClosure_open_0_TransitiveRelation(KB,Ctx,KeyTerm,Predicate):-
		sigmaCache(KB,_Ctx,tc(KeyTerm,complete,table)),!.
table_make_inferTransitiveClosure_open_0_TransitiveRelation(KB,Ctx,KeyTerm,Predicate):-
		once((
		sigmaCache(KB,_Ctx,tc(KeyTerm,incomplete,table));assert(sigmaCache(KB,_Ctx,tc(KeyTerm,incomplete,table))))),fail.
table_make_inferTransitiveClosure_open_0_TransitiveRelation(KB,Ctx,KeyTerm,Predicate):-
		(DataTerm myUniv  [Predicate,S,C]),
		make_inferTransitiveClosure_open_0_TransitiveRelation(KB,Ctx,KeyTerm,Predicate,S,C,P),
		once(storeInCacheIffNew(KB,Ctx,KeyTerm,DataTerm,P)),fail.

table_make_inferTransitiveClosure_open_0_TransitiveRelation(KB,Ctx,KeyTerm,Predicate):-
		retractall(sigmaCache(KB,_Ctx,tc(KeyTerm,incomplete,table))),
		assert(sigmaCache(KB,_Ctx,tc(KeyTerm,complete,table))).
		


make_inferTransitiveClosure_open_0_TransitiveRelation(KB,Ctx,KeyTerm,Predicate,S,C,P):-
	(Call  myUniv   [Predicate,S,C]),
	getFactForTransitiveClosure(KB,Ctx,Call,P).

make_inferTransitiveClosure_open_0_TransitiveRelation(KB,Ctx,KeyTerm,Predicate,S,C,(P *Proof )):-
	(Call  myUniv   [Predicate,S,M]),
	getFactForTransitiveClosure(KB,Ctx,Call,P),
	inferTransitiveClosure_O_TransitiveRelation(KB,Ctx,Predicate,M,C,Proof).

% =====================================================================================================
%% Generic Transitive Left to Right 
% =====================================================================================================

inferTransitiveClosure_lr_TransitiveRelation(KB,Ctx,Predicate,S,C,Proof):-
	KeyTerm myUniv  [rangeFn,Predicate,S],!,
	inferTransitiveClosure_l_r_TransitiveRelation(KB,Ctx,KeyTerm,Predicate,S,C,Proof).


inferTransitiveClosure_l_r_TransitiveRelation(KB,Ctx,KeyTerm,Predicate,S,C,_):-
	once(table_inferTransitiveClosure_l_r_0_TransitiveRelation(KB,Ctx,KeyTerm,Predicate,S)),fail.
	
inferTransitiveClosure_l_r_TransitiveRelation(KB,Ctx,KeyTerm,Predicate,S,C,P):-
	%writeDebug(silver,have_tabel(KeyTerm)),
	DataTerm myUniv  [Predicate,S,C],!,
	sigmaCache(KB,_Ctx,tc(KeyTerm,DataTerm),P).
	

table_inferTransitiveClosure_l_r_0_TransitiveRelation(KB,Ctx,KeyTerm,Predicate,S):-
		sigmaCache(KB,_Ctx,tc(KeyTerm,complete,table)),!.

table_inferTransitiveClosure_l_r_0_TransitiveRelation(KB,Ctx,KeyTerm,Predicate,S):-
		once((
		sigmaCache(KB,_Ctx,tc(KeyTerm,incomplete,table));assert(sigmaCache(KB,_Ctx,tc(KeyTerm,incomplete,table))))),fail.

table_inferTransitiveClosure_l_r_0_TransitiveRelation(KB,Ctx,KeyTerm,Predicate,S):-
		(DataTerm myUniv  [Predicate,S,C]),
		inferTransitiveClosure_l_r_0_TransitiveRelation(KB,Ctx,KeyTerm,Predicate,S,C,P),
		once(storeInCacheIffNew(KB,Ctx,KeyTerm,DataTerm,P)),fail.
		
table_inferTransitiveClosure_l_r_0_TransitiveRelation(KB,Ctx,KeyTerm,Predicate,S):-
		retractall(sigmaCache(KB,_Ctx,tc(KeyTerm,incomplete,table))),
		assert(sigmaCache(KB,_Ctx,tc(KeyTerm,complete,table))),!.


inferTransitiveClosure_l_r_0_TransitiveRelation(KB,Ctx,KeyTerm,Predicate,S,C,P):-
	(Call  myUniv   [Predicate,S,C]),
	getFactForTransitiveClosure(KB,Ctx,Call,P).

inferTransitiveClosure_l_r_0_TransitiveRelation(KB,Ctx,KeyTerm,Predicate,S,C,(P *Proof )):-
	(Call  myUniv   [Predicate,S,M]),
	getFactForTransitiveClosure(KB,Ctx,Call,P),
	inferTransitiveClosure_O_TransitiveRelation(KB,Ctx,Predicate,M,C,Proof).

% =====================================================================================================
%% Generic Transitive  Right  to Left
% =====================================================================================================

inferTransitiveClosure_rl_TransitiveRelation(KB,Ctx,Predicate,S,C,Proof):-
	KeyTerm myUniv  [domainFn,Predicate,C],!,
	inferTransitiveClosure_r_l_TransitiveRelation(KB,Ctx,KeyTerm,Predicate,S,C,Proof).

inferTransitiveClosure_r_l_TransitiveRelation(KB,Ctx,KeyTerm,Predicate,S,C,_):-
		table_inferTransitiveClosure_r_l_0_TransitiveRelation(KB,Ctx,KeyTerm,Predicate,C),fail.
inferTransitiveClosure_r_l_TransitiveRelation(KB,Ctx,KeyTerm,Predicate,S,C,P):-
		%writeDebug(have_tabel(KeyTerm)),
		DataTerm myUniv  [Predicate,S,C],!,sigmaCache(KB,_Ctx,tc(KeyTerm,DataTerm),P).

table_inferTransitiveClosure_r_l_0_TransitiveRelation(KB,Ctx,KeyTerm,Predicate,C):-
		sigmaCache(KB,_Ctx,tc(KeyTerm,complete,table)),!.

table_inferTransitiveClosure_r_l_0_TransitiveRelation(KB,Ctx,KeyTerm,Predicate,C):-
		once((
		sigmaCache(KB,_Ctx,tc(KeyTerm,incomplete,table));assert(sigmaCache(KB,_Ctx,tc(KeyTerm,incomplete,table))))),fail.

table_inferTransitiveClosure_r_l_0_TransitiveRelation(KB,Ctx,KeyTerm,Predicate,C):-
		(DataTerm myUniv  [Predicate,S,C]),
		inferTransitiveClosure_r_l_0_TransitiveRelation(KB,Ctx,KeyTerm,Predicate,S,C,P),
		once(storeInCacheIffNew(KB,Ctx,KeyTerm,DataTerm,P)),fail.

table_inferTransitiveClosure_r_l_0_TransitiveRelation(KB,Ctx,KeyTerm,Predicate,C):-
		retractall(sigmaCache(KB,_Ctx,tc(KeyTerm,incomplete,table))),assert(sigmaCache(KB,_Ctx,tc(KeyTerm,complete,table))),!.


inferTransitiveClosure_r_l_0_TransitiveRelation(KB,Ctx,KeyTerm,Predicate,S,C,P):-
	(Call  myUniv   [Predicate,S,C]),
	getFactForTransitiveClosure(KB,Ctx,Call,P).

inferTransitiveClosure_r_l_0_TransitiveRelation(KB,Ctx,KeyTerm,Predicate,S,C,(P2 * P)):-
	(Call  myUniv   [Predicate,M,C]),
	getFactForTransitiveClosure(KB,Ctx,Call,P),
	inferTransitiveClosure_O_TransitiveRelation(KB,Ctx,Predicate,S,M,P2).

isAxiomInKB(_):-!. %TODO



:- eraseTransitiveClosureCache.
