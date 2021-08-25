/* 
File: "pred_Merge_valence.pll"

Author:  dmiles@teknowledge.com [Douglas R. Miles]
Contacts:   dmiles@teknowledge.com, 
		apease@ks.teknowledge.com,  
		sigma-dev@ks.teknowledge.com (support)

Purpose:  Individual loading of Sigma KB,Ctx Functors.

Exported:  
	proof_line/2,  									% Table of Proof Formats
	inferValence(KB,Ctx, Relation, Valence,   Proof),                   % Basic Access

Uses: inferTransitiveClosure_PartialOrderingRelation/6, forwardchain_Merge_instance2/5, forwardchain/6.

Type of SourceInfo: predicate_module (generated runtime)
Module:   'Merge_valence'
SUMO:   valence
Holds:   holds(valence, _G3041, _G3044)
KB,Ctx:   'Merge'
Debug:  no_debug

*/
	     /*
	     
:-module('Merge_valence', 
       ['Merge_valence2'/6, 
	backchain_Merge_valence2/6,
	forwardchain_Merge_valence2/6]).
	     */
:-include('sigma_header.pl').

% =====================================================
% inference_module(KB,Ctx,SUMOPred,Holds,Imported,ConnectionType,SourceInfo,HowOften).
% 	KB,Ctx = The KnowedgeBase
% 	Ctx = The Context 
% 	SUMOPred = The Functor's SUMO Name
%	Goal = Functors SUMO Prototype	 "goal(Table,holds(SUMOPred,A,B),KB,Ctx,ProofOut)"
%	Imported = Functor's Prolog Prototype
% 	ConnectionType = 'prolog'  meaning its compiled by consulting this file to memory.
% 	SourceInfo = The connection parameters
% 	HowOften = always,never,once
% =====================================================
/*

:-was_indexed(inference_module(1,0,1,1,1,0,0,0)).

	
inference_module(
	'Merge',_AllContexts,
	valence,goal(ProofIn,holds(valence,A, B),'Merge',ProofOut),
	inferValence_util(KB,Ctx, A, B,  ProofIn, ProofOut),
	prolog, 'pred_Merge_valence.pll',
	always).
			    
*/	 

% =====================================================
% index/dynamic All exported predicates
% =====================================================

:-dynamic('Merge_valence2'/6).
:-dynamic(forwardchain_Merge_valence2/6).
:-dynamic(backchain_Merge_valence2/6).

inferValence(KB,Ctx,not,1,inlinecode(valence(not,1))):-!.

inferValence(KB,Ctx,X,2,inlinecode(valence(X,2))):-
	inferSurfaceGuarded(instance(X,'SententialOperator'),KB,Ctx,Proof),!.

inferValence(KB,Ctx,R,N,Proof):-
	inferValence_util(KB,Ctx,R,N,Proof),!.

% Valence #2
inferValence(KB,Ctx,X,N,Proof * Proof3 * inlinecode(valence,N)):-
	inferTransitiveClosure_PartialOrderingRelation(KB,Ctx,subrelation,X,Super,Proof3),
	X\=Super,
	inferValence_util(KB,Ctx,Super,N,Proof),!.

inferValence(KB,Ctx,X,N,Proof * Proof3 * inlinecode(valence,N)):-
	inferSurface_gaf_sym(disjointRelation(X,Super),KB,Ctx,Proof3),
	X\=Super,
	inferValence_util(KB,Ctx,Super,N,Proof),!.
	


	

% ==================================================================================
% Entry Points for singleValued   valence/2
% ==================================================================================
% Valence #1
inferNegValence(KB,Ctx,X,YY,Proof * inlinecode(valence,1)):-
	isValenceInt(YY),
	inferValence_util(KB,Ctx,X,Y,Proof),!,Y=YY.
	
% Valence #10
inferNegValence(KB,Ctx,X,YY,Proof * inlinecode(valence,10)):-
	inferValence_util(KB,Ctx,X,Y,Proof),isValenceInt(YY),Y\=YY,!.

	
% Valence Asserted
inferValence_util(KB,Ctx,X,Y,Proof):-
	sigmaCache(valence(X,Y), _,_,_,KB,Ctx,_,_,Proof),!.
					    
% Valence #6
inferValence_util(KB,Ctx,X,2,Proof * inlinecode(valence,6)):-
	inferSurfaceGuarded(instance(X,'BinaryPredicate'),KB,Ctx,Proof),!.

% Valence #7
inferValence_util(KB,Ctx,X,3,Proof * inlinecode(valence,7)):-
	inferSurfaceGuarded(instance(X,'TernaryPredicate'),KB,Ctx,Proof),!.

% Valence #8
inferValence_util(KB,Ctx,X,4,Proof * inlinecode(valence,8)):-
	inferSurfaceGuarded(instance(X,'QuaternaryPredicate'),KB,Ctx,Proof),!.
	
% Valence #3
inferValence_util(KB,Ctx,X,2,Proof * inlinecode(valence,3)):-
	inferSurfaceGuarded(instance(X,'UnaryFunction'),KB,Ctx,Proof),!.

% Valence #4
inferValence_util(KB,Ctx,X,3,Proof * inlinecode(valence,4)):-
	inferSurfaceGuarded(instance(X,'BinaryFunction'),KB,Ctx,Proof),!.

% Valence #5
inferValence_util(KB,Ctx,X,4,Proof * inlinecode(valence,5)):-
	inferSurfaceGuarded(instance(X,'TernaryFunction'),KB,Ctx,Proof),!.


	


% Utility Predicates

isValenceInt(1).
isValenceInt(2).
isValenceInt(3).
isValenceInt(4).
isValenceInt(5).

	
% ================================================================================================================
% Rules for valence/2
% ================================================================================================================


/* 


Rule # 1 for valence   
  
Forms:
 1. "(=> 
         (instance ?REL VariableArityRelation) 
         (not 
            (exists (?INT) 
               (valence ?REL ?INT))))"
1a. (entails 
         (instance ?REL VariableArityRelation) 
         (not 
            (valence ?REL ?INT)))


Flags: r1([2nd(holds)])

*/

proof_line(incode('Merge_valence2', 1, ['REL'=A, 'INT'=B|C]), via(entails(holds(instance, A, 'VariableArityRelation'), not holds(valence, A, B)), ['REL'=A, 'INT'=B|C])*surf('Merge', 1628)).

/*
'Merge_~valence'( A, B, 'BASE ONTOLOGY', E, E*D*incode('Merge_valence2', 1, ['REL'=A, 'INT'=B|C])) :- 
	forwardchain_instance(true, holds(instance, A, 'VariableArityRelation'), 'BASE ONTOLOGY', D)
*/


/* 


Rule # 2 for valence   
  
Forms:
 2. "(=> 
         (instance ?REL QuintaryPredicate) 
         (valence ?REL 5))"
2a. (entails 
         (instance ?REL QuintaryPredicate) 
         (valence ?REL 5))


Flags: r1([2nd(holds)])

*/

proof_line(incode('Merge_valence2', 2, ['REL'=A|B]), via(entails(holds(instance, A, 'QuintaryPredicate'), holds(valence, A, 5)), ['REL'=A|B])*surf('Merge', 1622)).

/*
inferValence_util(KB,Ctx, A, 5, 'BASE ONTOLOGY', D, D*C*incode('Merge_valence2', 2, ['REL'=A|B])) :- 
	forwardchain_instance(true, holds(instance, A, 'QuintaryPredicate'), 'BASE ONTOLOGY', C)
*/


/* 


Rule # 3 for valence   
  
Forms:
 3. "(=> 
         (instance ?REL QuaternaryPredicate) 
         (valence ?REL 4))"
3a. (entails 
         (instance ?REL QuaternaryPredicate) 
         (valence ?REL 4))


Flags: r1([2nd(holds)])

*/

proof_line(incode('Merge_valence2', 3, ['REL'=A|B]), via(entails(holds(instance, A, 'QuaternaryPredicate'), holds(valence, A, 4)), ['REL'=A|B])*surf('Merge', 1614)).

/*
inferValence_util(KB,Ctx, A, 4, 'BASE ONTOLOGY', D, D*C*incode('Merge_valence2', 3, ['REL'=A|B])) :- 
	forwardchain_instance(true, holds(instance, A, 'QuaternaryPredicate'), 'BASE ONTOLOGY', C)
*/


/* 


Rule # 4 for valence   
  
Forms:
 4. "(=> 
         (instance ?REL TernaryPredicate) 
         (valence ?REL 3))"
4a. (entails 
         (instance ?REL TernaryPredicate) 
         (valence ?REL 3))


Flags: r1([2nd(holds)])

*/

proof_line(incode('Merge_valence2', 4, ['REL'=A|B]), via(entails(holds(instance, A, 'TernaryPredicate'), holds(valence, A, 3)), ['REL'=A|B])*surf('Merge', 1606)).

/*
inferValence_util(KB,Ctx, A, 3, 'BASE ONTOLOGY', D, D*C*incode('Merge_valence2', 4, ['REL'=A|B])) :- 
	forwardchain_instance(true, holds(instance, A, 'TernaryPredicate'), 'BASE ONTOLOGY', C)
*/


/* 


Rule # 5 for valence   
  
Forms:
 5. "(=> 
         (instance ?REL BinaryPredicate) 
         (valence ?REL 2))"
5a. (entails 
         (instance ?REL BinaryPredicate) 
         (valence ?REL 2))


Flags: r1([2nd(holds)])

*/

proof_line(incode('Merge_valence2', 5, ['REL'=A|B]), via(entails(holds(instance, A, 'BinaryPredicate'), holds(valence, A, 2)), ['REL'=A|B])*surf('Merge', 1598)).

/*
inferValence_util(KB,Ctx, A, 2, 'BASE ONTOLOGY', D, D*C*incode('Merge_valence2', 5, ['REL'=A|B])) :- 
	forwardchain_instance(true, holds(instance, A, 'BinaryPredicate'), 'BASE ONTOLOGY', C)
*/


/* 


Rule # 6 for valence   
  
Forms:
 6. "(=> 
         (instance ?FUNCTION TernaryFunction) 
         (valence ?FUNCTION 3))"
6a. (entails 
         (instance ?FUNCTION TernaryFunction) 
         (valence ?FUNCTION 3))


Flags: r1([2nd(holds)])

*/

proof_line(incode('Merge_valence2', 6, ['FUNCTION'=A|B]), via(entails(holds(instance, A, 'TernaryFunction'), holds(valence, A, 3)), ['FUNCTION'=A|B])*surf('Merge', 1580)).

/*
inferValence_util(KB,Ctx, A, 3, 'BASE ONTOLOGY', D, D*C*incode('Merge_valence2', 6, ['FUNCTION'=A|B])) :- 
	forwardchain_instance(true, holds(instance, A, 'TernaryFunction'), 'BASE ONTOLOGY', C)
*/


/* 


Rule # 7 for valence   
  
Forms:
 7. "(=> 
         (instance ?FUNCTION BinaryFunction) 
         (valence ?FUNCTION 2))"
7a. (entails 
         (instance ?FUNCTION BinaryFunction) 
         (valence ?FUNCTION 2))


Flags: r1([2nd(holds)])

*/

proof_line(incode('Merge_valence2', 7, ['FUNCTION'=A|B]), via(entails(holds(instance, A, 'BinaryFunction'), holds(valence, A, 2)), ['FUNCTION'=A|B])*surf('Merge', 1558)).

/*
inferValence_util(KB,Ctx, A, 2, 'BASE ONTOLOGY', D, D*C*incode('Merge_valence2', 7, ['FUNCTION'=A|B])) :- 
	forwardchain_instance(true, holds(instance, A, 'BinaryFunction'), 'BASE ONTOLOGY', C)
*/


/* 


Rule # 8 for valence   
  
Forms:
 8. "(=> 
         (instance ?FUNCTION UnaryFunction) 
         (valence ?FUNCTION 1))"
8a. (entails 
         (instance ?FUNCTION UnaryFunction) 
         (valence ?FUNCTION 1))


Flags: r1([2nd(holds)])

*/

proof_line(incode('Merge_valence2', 8, ['FUNCTION'=A|B]), via(entails(holds(instance, A, 'UnaryFunction'), holds(valence, A, 1)), ['FUNCTION'=A|B])*surf('Merge', 1536)).

/*
inferValence_util(KB,Ctx, A, 1, 'BASE ONTOLOGY', D, D*C*incode('Merge_valence2', 8, ['FUNCTION'=A|B])) :- 
	forwardchain_instance(true, holds(instance, A, 'UnaryFunction'), 'BASE ONTOLOGY', C)
*/


/* 


Rule # 9 for valence   
  
Forms:
 9. "(=> 
         (subrelation ?PRED1 ?PRED2) 
         (exists (?NUMBER) 
            (and 
               (valence ?PRED1 ?NUMBER) 
               (valence ?PRED2 ?NUMBER))))"
9a. (entails 
         (subrelation ?PRED1 ?PRED2) 
         (valence ?PRED2 
            (Valence8SkFn ?PRED2 ?PRED1)))


Flags: r1([zzskFn(Valence8SkFn), 2nd(holds)])

*/

proof_line(incode('Merge_valence2', 9, ['PRED1'=A, 'PRED2'=B, 'NUMBER'=C|D]), via(entails(holds(subrelation, A, B), holds(valence, B, zzskFn('Valence8SkFn', [B, A]))), ['PRED1'=A, 'PRED2'=B, 'NUMBER'=C|D])*surf('Merge', 183)).

/*
inferValence_util(KB,Ctx, B, zzskFn('Valence8SkFn', [B, A]), 'STRUCTURAL ONTOLOGY', E, F*E*incode('Merge_valence2', 9, ['PRED1'=A, 'PRED2'=B, 'NUMBER'=C|D])) :- 
	forwardchain(true, holds(subrelation, A, B), 'STRUCTURAL ONTOLOGY', E)
*/


/* 


Rule # 10 for valence   
  
Forms:
 10. "(=> 
         (subrelation ?PRED1 ?PRED2) 
         (exists (?NUMBER) 
            (and 
               (valence ?PRED1 ?NUMBER) 
               (valence ?PRED2 ?NUMBER))))"
10a. (entails 
         (subrelation ?PRED1 ?PRED2) 
         (valence ?PRED1 
            (Valence8SkFn ?PRED2 ?PRED1)))


Flags: r1([zzskFn(Valence8SkFn), 2nd(holds)])

*/

proof_line(incode('Merge_valence2', 10, ['PRED1'=A, 'PRED2'=B, 'NUMBER'=C|D]), via(entails(holds(subrelation, A, B), holds(valence, A, zzskFn('Valence8SkFn', [B, A]))), ['PRED1'=A, 'PRED2'=B, 'NUMBER'=C|D])*surf('Merge', 183)).

/*
inferValence_util(KB,Ctx, A, zzskFn('Valence8SkFn', [B, A]), 'STRUCTURAL ONTOLOGY', E, F*E*incode('Merge_valence2', 10, ['PRED1'=A, 'PRED2'=B, 'NUMBER'=C|D])) :- 
	forwardchain(true, holds(subrelation, A, B), 'STRUCTURAL ONTOLOGY', E)
*/


/* 
 Last Saved: Wed Jan 23 18:46:54 2002
*/


