:-include(sigma_header).


/*
predicates_declared_inline_HL('subAttibute',2).
predicates_declared_inline_HL('~subAttibute',2).

predicates_declared_inline_HL('attribute',2).
predicates_declared_inline_HL('~attribute',2).

predicates_declared_inline_HL('subset',2).
predicates_declared_inline_HL('~subset',2).

predicates_declared_inline_HL('element',2).
predicates_declared_inline_HL('~element',2).
*/
:-dynamic('instance'/2).
:-multifile('~instance'/2).

/*
predicates_declared_inline_HL('instance',2).
predicates_declared_inline_HL('~instance',2).
*/

'instance'(X,Y):-is_instance_of(X,Y).

'~instance'(X,Y):-is_instance_of(X,Y),!,fail.
'~instance'(X,Y):-isNonVar(Y),!.

predicates_declared_inline_HL('t_instance',2).
predicates_declared_inline_HL('~t_instance',2).



predicates_declared_inline_HL('~u',3).
predicates_declared_inline_HL('u',3).
%u(A,B,'Quantity'):-'equal'(X,Y).


%u(_,_,'Skolem'):-!,fail.
u('zzskFn'(A),B,'Skolem'):-nonvar(A),!,unify_with_occurs_check(A,B),!.


u(A,B,_):-!,A==B.

u(A,B,_):-!,unify_with_occurs_check(A,B),!.
%u(A,B,_):-catch(unify_with_occurs_check(A,B),E,fail),!.
%u(A,B,_):-sigmaCache(surface,'equal'(A,B):L,KB,Ctx,TN,Author,On).

predicates_declared_inline_HL('resolve_skolem',2).

:-dynamic('subclass'/2).
:-multifile('~subclass'/2).

/*
predicates_declared_inline_HL('subclass',2).
predicates_declared_inline_HL('~subclass',2).
predicates_declared_inline_HL('subrelation',2).
predicates_declared_inline_HL('~subrelation',2).
predicates_declared_inline_HL('inverse',2).
predicates_declared_inline_HL('~inverse',2).
predicates_declared_inline_HL('disjoint',2).
predicates_declared_inline_HL('~disjoint',2).
predicates_declared_inline_HL('range',2).
predicates_declared_inline_HL('~range',2).
predicates_declared_inline_HL('documentation',2).
predicates_declared_inline_HL('~documentation',2).
*/


'subclass'(X,Y):-is_subclass_of(X,Y).

'~subclass'(X,Y):-is_subclass_of(X,Y),!,fail.
'~subclass'(X,Y):-isNonVar(Y),!.

:-dynamic('domain'/2).
:-multifile('~domain'/2).

/*
predicates_declared_inline_HL('domain',3).
predicates_declared_inline_HL('~domain',3).
predicates_declared_inline_HL('domainSubclass',3).
predicates_declared_inline_HL('~domainSubclass',3).
*/

'domain'(X,Y,Z):-is_nth_domain_of(X,Y,Z).

'~domain'(X,Y,Z):-is_nth_domain_of(X,Y),!,fail.
'~domain'(X,Y,Z):-isNonVar(Y),!.

'domainSubclass'(X,Y,Z):-is_nth_domain_of(X,Y,Z).

'~domainSubclass'(X,Y,Z):-is_nth_domain_of(X,Y),!,fail.
'~domainSubclass'(X,Y,Z):-isNonVar(Y),!.

% Impliements  infer-same

% u/3 is the identity predicate it recognises 17 major classes and selects the correct inference mechanism for each type of unification
% based on the the_hash_set(
% ['Collection','Representation','Formula','Region','Process','Object','Attribute','FunctionQuantity','Function','Relation','Quantity','Proposition','Set','Class','Physical','Abstract', 'Entity']).




	

