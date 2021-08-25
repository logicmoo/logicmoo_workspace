
end_of_file.



%:-include('sigma_header.pl').
% ===================================================================
% File 'Sigma_equal.pl' 
% Authors: Douglas Miles
% Contact: dmiles@teknowledge.com ; apease@teknowledge.com
% Version: 'Sigma_equal.pl' 1.0.0 
% History:
% Purpose:  To allow evaluation of determinants before and after unification 
% ===================================================================


% Based On SUO-Merge assertions of http://127.0.0.1:8080/sigma/skb.jsp?req=SC&;skb=Merge&id=205

%  ('instance' MultiplicationFnFn RelationExtendedToQuantities)
%  ('instance' AdditionFnFn RelationExtendedToQuantities)
%  ('instance' SubtractionFnFn RelationExtendedToQuantities)
%  ('instance' ExponentiationFn RelationExtendedToQuantities)
%  ('instance' ReciprocalFn RelationExtendedToQuantities)
%  ('instance' MaxFn RelationExtendedToQuantities)
%  ('instance' MinFn RelationExtendedToQuantities)
%  ('instance' ModuloFn RelationExtendedToQuantities)
%  ('instance' RemainderFn RelationExtendedToQuantities)
%  ('instance' RoundFn RelationExtendedToQuantities)
%  ('instance' TruncateFn RelationExtendedToQuantities)
%  ('instance' lessThan RelationExtendedToQuantities)
%  ('instance' greaterThan RelationExtendedToQuantities)
%  ('instance' lessThanOrEqualTo RelationExtendedToQuantities)
%  ('instance' greaterThanOrEqualTo RelationExtendedToQuantities)
%  ('instance' equal RelationExtendedToQuantities)

% Maybe Ian should to add to Merge
%  ('instance' DivisionFn RelationExtendedToQuantities)


% ========================================
% Equality and Sameness
% ========================================
predicates_declared_inline_HL(equal,2).
predicates_declared_inline_HL('~equal',2).
predicates_declared_inline_HL(resolve_skolem,2).
predicates_declared_inline_HL('different',2).
predicates_declared_inline_HL('~different',2).

% ==========================================
% Equal/Evaluate
% ==========================================

% Success based equals
equal(A,B,incode(equal(A,B))):-
	equal(A,B).
equal(A,B,Proof):-
	ground((A,B)),
	inferSurfaceGuarded(equal,true,equal(A,B),_,_,Proof).

% Failure Based Equals
not_equal(A,B,incode(not(equal(A,B)))):-not(equal(A,B)).

equal(X,Y):-
	writeDebug(X=Y),
	equals_proc(X,Y),!,  % equal happens once
	writeDebug(found(X=Y)).

equal(X,Y):-unify_with_occurs_check(X,Y),!.

equals_proc(Var,NV):-var(Var),nonvar(NV),!,equals_var(NV,Var),!.

equals_proc(NV,Var):-var(Var),nonvar(NV),!,equals_var(NV,Var),!.

equals_proc(NV1,NV2):-!,equals_var(NV1,Var1),!,equals_var(NV2,Var2),!,Var1==Var2,!.


equals_var(X,Y):-not(compound(X)),!,unify_with_occurs_check(X,Y),!.
equals_var(X,Y):-eval_lr(X,Y).
equals_var('Fn'(F1, V1),Var):-!,eval_lr('Fn'(F1, V1),Var).

eval_lr(X,X):-number(X),!.
eval_lr(X,X):-not(compound(X)),!.


eval_lr('AdditionFn'(V1,V2),R):-
      eval_lr(V1,R1),
      eval_lr(V2,R2), 
      sandbox((R is R1 + R2)),!.

eval_lr('SubtractionFn'(V1,V2),R):-
      eval_lr(V1,R1),
      eval_lr(V2,R2),
      sandbox((R is R1 - R2)).

eval_lr('MultiplicationFn'(V1,V2),R):-
      eval_lr(V1,R1),
      eval_lr(V2,R2),
      sandbox((R is R1 * R2)),!.

eval_lr('DivisionFn'(V1,V2),R):-
      eval_lr(V1,R1),
      eval_lr(V2,R2),
      sandbox((R is R1 / R2)),!.

%eval_lr('QuantityConversionFn'('Percent', 'DivisionFn'(_G36802, 'MillionBarrelsPerDay'(69.86))),Result):-
eval_lr('QuantityConversionFn'('Percent', X),Result):-!,eval_lr('MultiplicationFn'(X,100),Result).

%goal(instance('MillionBarrelsPerDay'(69.86), 'FlowRate'))
eval_lr('MaxFn'(A,B),NumOut):-!,
		eval_lr(A,AO),
		eval_lr(B,BO),
		((catch(AO<BO,_fail),NumOut=A);NumOut=B),!.

eval_lr('MinFn'(A,B),NumOut):-!,
		eval_lr(A,AO),
		eval_lr(B,BO),
		((catch(AO>BO,_fail),NumOut=A);NumOut=B),!.

eval_lr('SignumFn'(A),NumOut):-!,
		eval_lr(A,AO),
		number(AO),
		once(((AO = 0,NumOut=0);(AO>0,NumOut=1);(AO<0,NumOut=(-1)))).

eval_lr('MagnitudeFn'('MeasureFn'(NumIn,_)),NumOut):-!,eval_lr(NumIn,NumOut).
eval_lr('MeasureFn'(NumIn,_),NumOut):-!,eval_lr(NumIn,NumOut).
eval_lr('Dollars'(NumIn),NumOut):-!,eval_lr(NumIn,NumOut).
eval_lr('BillionDollars'(NumIn),NumOut):-!,eval_lr(NumIn,Value),!,eval_lr_nvf('Fn'('MultiplicationFn',[100000,'Fn'('MultiplicationFn',[10000,Value])]),NumOut),!.
eval_lr('BarrelsPerDay'(Value),R):-!,eval_lr(Value,R),!.
eval_lr('MillionBarrelsPerDay'(Num),R):-!,eval_lr(Num,Value),!,eval_lr_nvf('Fn'('MultiplicationFn',[1000000,Value]),R),!.

eval_lr(X,Y):-sandbox((X=..[_,R],!,eval_lr(R,Y))),!.

eval_lr('Fn'(A,V),Z):-var(A),!,eval_lr_vf('Fn'(A,V),Z).
eval_lr('Fn'(Var,['Fn'(Var2,_)|_]),_):-  var(Var2),!, fail.  %TODO Algebra
eval_lr('Fn'(A,V),Z):-eval_lr_nvf('Fn'(A,V),Z).
eval_lr(X,Y):-unify_with_occurs_check(X,Y),!.
   


eval_lr_nvf(X,X):-(var(X)),!.
eval_lr_nvf(X,X):-(number(X)),!.

eval_lr_nvf('Fn'('YearFn',[Value]),Value):-!.

eval_lr_nvf('Fn'('QuantityConversionFn',['PercentFn',Value]),R):-!,eval_lr(Value,VR),!,eval_lr_nvf('Fn'('MultiplicationFn',[100,Value]),R).

eval_lr_nvf('Fn'('MeasureFn',[EXPR,Type]),'Fn'('MeasureFn',[R,Type])):-eval_lr(EXPR,R).

eval_lr_nvf('Fn'('AdditionFn',[BX,BY]),BZ):- eval_lr_nvf(BX,X),eval_lr_nvf(BY,Y),eval_lr_nvf(BZ,Z),(sandbox(plus(X,Y,Z));sandbox(Z is X+Y);sandbox(X is Z-Y);sandbox(Y is Z-X)),!.
eval_lr_nvf('Fn'('SubtractionFn',[BX,BY]),BZ):- eval_lr_nvf(BX,X),eval_lr_nvf(BY,Y),eval_lr_nvf(BZ,Z),(sandbox(plus(Z,X,Y));sandbox(Z is X-Y);sandbox(X is Z+Y);sandbox(Y is Z+X)),!.
eval_lr_nvf('Fn'('MultiplicationFn',[BX,BY]),BZ):- eval_lr_nvf(BX,X),eval_lr_nvf(BY,Y),eval_lr_nvf(BZ,Z),(sandbox(Z is X*Y);sandbox(X is Z/Y);sandbox(Y is Z/X)),!.
eval_lr_nvf('Fn'('DivisionFn',[BX,BY]),BZ):- eval_lr_nvf(BX,X),eval_lr_nvf(BY,Y),eval_lr_nvf(BZ,Z),(sandbox(Z is X/Y);sandbox(X is Z*Y);sandbox(Y is Z*X)),!.

eval_lr_nvf('Fn'('AssignmentFn',[OP|ARGS]),Z):- !,
                  'AssignmentFn'(OP,ARGS,Z),!.

eval_lr_nvf(X,Y):-unify_with_occurs_check(X,Y),!.

%eval_lr_nvf('Fn'(Op,X),Z):-eval_lr_list(X,RX),!,fun('Fn'(Op,RX),Method),eval_lr(Method,Z),!.

eval_lr_vf(X,Y):-unify_with_occurs_check(X,Y),!.

eval_lr_list([],[]).
eval_lr_list([H|T],[HO|TO]):-
            eval_lr(H,HO),!,
            eval_lr_list(T,TO),!.
   
fun('Fn'('PercentFn',[Value]),'Fn'('AssignmentFn',['DivisionFn',Value,100])).

fun(Have,Todo):-on_the_surface(equal(Have,Todo)).

on_the_surface(equal(Have,Todo)) :- sigmaCache(PredR,ASID,surface,(equal(X , Y)),Vars,KB,Ctx,TN,On),unnumbervars((X,Y),(Have,Todo)),!.





fun('Fn'('AdditionFn',[X,Y]),'Fn'('AssignmentFn',['AdditionFn',X,Y])).
fun('Fn'('Plus',[X,Y]),'Fn'('AssignmentFn',['AdditionFn',X,Y])).
'AssignmentFn'('AdditionFn',[X,Y],Z):-sandbox(plus(X,Y,Z)).

fun('Fn'('MultiplicationFn',[X,Y]),'Fn'('AssignmentFn',['MultiplicationFn',X,Y])).
fun('Fn'('TimesFn',[X,Y]),'Fn'('AssignmentFn',['MultiplicationFn',X,Y])).
'AssignmentFn'('MultiplicationFn',[X,Y],Z):- nonvar(X),nonvar(Y),!,sandbox(Z is X*Y).
'AssignmentFn'('MultiplicationFn',[X,Y],Z):- var(X),nonvar(Y),nonvar(Z),!,sandbox(X is Z/Y).
'AssignmentFn'('MultiplicationFn',[X,Y],Z):- var(Y),nonvar(X),nonvar(Z),!,sandbox(Y is Z/X).

fun('Fn'('DivisionFn',[X,Y]),'Fn'('AssignmentFn',['DivisionFn',X,Y])).
fun('Fn'('QuotientFn',[X,Y]),'Fn'('AssignmentFn',['DivisionFn',X,Y])).
'AssignmentFn'('DivisionFn',[X,Y],Z):- sandbox(Z is X/Y).

fun('Fn'('DifferenceFn',[X,Y]),'Fn'('AssignmentFn',['SubtractionFn',X,Y])).
fun('Fn'('SubtractionFn',[X,Y]),'Fn'('AssignmentFn',['SubtractionFn',X,Y])).
'AssignmentFn'('SubtractionFn',[X,Y],Z):- sandbox(Z is X-Y),!.
'AssignmentFn'('SubtractionFn',[X,Y],Z):- sandbox(plus(Y,Z,X)).


/*
eval_lr('Fn'(if,[C,X,Y]),Z):-
        eval_lr(C,C1),
        auxif(C1,X,Y,A),
        eval_lr(A,Z),!.
*/
auxif(true,X,_,X).
auxif(false,_,X,X).


/*
eval_lr(F,Lx):-
        atom(F),
        fun('Fn'(F,X),Y),
        make_lambda(X,Y,Lx).
*/
make_lambda([],Y,Y).
make_lambda([X|Xs],Y,'Fn'('LambdaFn',[X,Z])):-make_lambda(Xs,Y,Z).

/*eval_lr('Fn'(Fx,[A]),Z):-
        eval_lr(Fx,Lx),
        eval_lr(A,A1),
        copy_term(Lx,'Fn'('LambdaFn',[A1,Y])),
        eval_lr(Y,Z),!.

eval_lr('Fn'(Fx,[A|As]),Z):-
        eval_lr(Fx,Lx),
        eval_lr(A,A1),
        copy_term(Lx,'Fn'('LambdaFn',[A1,Y])),
        eval_lr('Fn'(Y,As),Z),!.
  */

%eval_lr([X|Xs],[Y|Ys]):-eval_lr(X,Y),eval_lr(Xs,Ys),!.

%eval_lr((X,Xs),(Y,Ys)):-eval_lr(X,Y),eval_lr(Xs,Ys),!.





%eval_lr(X,Y):-compound(X),
 %              sandbox((X=..[FX|AX],eval_lr(AX,AY),YY=..[FX|AY],eval_lr(YY,Y))).

/*eval_lr(X,Y):-compound(X),!,
               sandbox((X=..[FX|AX],!,eval_lr(AX,Y))).
  */

% Function definitions:

/*
fun('Fn'(inc,[X]),'Fn'('AssignmentFn',['AdditionFn',X,1])).
'AssignmentFn'(inc,X,1,Z):- Z is X +1.
*/





/*
fun('Fn'(lessThan,[X,Y]),'Fn'('AssignmentFn',[lessThan,X,Y])).
'AssignmentFn'(lessThan,X,Y,Z):-sandbox((X<Y,Z=true;Z=false)).

fun('Fn'(lessThanOrEqualTo,[X,Y]),'Fn'('AssignmentFn',[lessThanOrEqualTo,X,Y])).
'AssignmentFn'(lessThanOrEqualTo,X,Y,Z):-sandbox((X=<Y,Z=true;Z=false)).

fun('Fn'(greaterThan,[X,Y]),'Fn'('AssignmentFn',[greaterThan,X,Y])).
'AssignmentFn'(greaterThan,X,Y,Z):-sandbox((X>Y,Z=true;Z=false)).

fun('Fn'(greaterThanOrEqualTo,[X,Y]),'Fn'('AssignmentFn',[greaterThanOrEqualTo,X,Y])).
'AssignmentFn'(greaterThanOrEqualTo,X,Y,Z):-sandbox((X>=Y,Z=true;Z=false)).
*/
/*

fun('Fn'(and,[X,Y]),'Fn'('AssignmentFn',[and,X,Y])).
'AssignmentFn'(and,X,Y,Z):-sandbox((byrd_solve(X),byrd_solve(Y),Z=true;Z=false)).

fun('Fn'(exists,[X,Y]),'Fn'('AssignmentFn',[exists,X,Y])).
'AssignmentFn'(exists,X,Y,Z):-sandbox((holds(X,byrd_solve(Y)),Z)).

fun('Fn'(pred,[X]),'Fn'('AssignmentFn',[pred,X,Y])).
'AssignmentFn'(pred,X,Y):-sandbox((Y=pred(X);Y=false)).

fun('Fn'(equal,[X,Y]),'Fn'('AssignmentFn',[equal,X,Y])).
'AssignmentFn'(equal,X,Y,true):-!,equal(X,Y),!.
'AssignmentFn'(equal,_,_,false):-!.
*/




%fun('Fn'('logic-<=>',[A,B]),'Fn'('AssignmentFn',['logic-<=>',A,B])).
%'AssignmentFn'('logic-<=>',A,B,true):-getNegationForm(AA,A),getNegationForm(BB,B),A=B.
   /*
fun('Fn'(hd,[[X|_]],X)).

fun('Fn'(tl,[[_|T]]),T).

fun('Fn'(map,[F,L]),
                        'Fn'(if,[    'Fn'(equal,[L,[]]),
                                [],
                                ['Fn'(F,['Fn'(hd,[L])])|'Fn'(map,[F,'Fn'(tl,[L])]) ]
                        ] )
).


fun('Fn'(concatenate,[X,Y]),
                                'Fn'(if,[    'Fn'(equal,[X,[]]),
                                        Y,
                                        ['Fn'(hd,[X])|'Fn'(concatenate,['Fn'(tl,[X]),Y])]
                                ])
).

fun('Fn'(factorial,[N]),
        'Fn'(if,[    'Fn'(equal,[N,1]),
                1,
                'Fn'('MultiplicationFn',[N,'Fn'(factorial,['Fn'('SubtractionFn',[N,1])])])
        ])
).
      */



% ========================================
% Logical Equivalency
% ========================================
predicates_declared_inline_HL('logic-<=>',2).
predicates_declared_inline_HL('~logic-<=>',2).

'logic-<=>'(A,B):-getNegationForm(A,AA),!,getNegationForm(B,BB),!,unify_with_occurs_check(AA,BB).
'~logic-<=>'(A,B):-!,not('logic-<=>'(A,B)).

predicates_declared_inline_HL('domain-check',3).
'domain-check'(_,_,_).

% ========================================
% Skolem Unification
% ========================================
%:-multifile(equal/2).


:-dynamic(startsAfterEndingOf/2).
:-dynamic(laterThan/2).

% ========================================
% Greater/Less Than 
% ========================================

predicates_declared_inline_HL('greaterThan',2).
predicates_declared_inline_HL('~greaterThan',2).

predicates_declared_inline_HL('lessThan',2).
predicates_declared_inline_HL('~lessThan',2).

predicates_declared_inline_HL('greaterThanOrEqualTo',2).
predicates_declared_inline_HL('~greaterThanOrEqualTo',2).

predicates_declared_inline_HL('lessThanOrEqualTo',2).
predicates_declared_inline_HL('~lessThanOrEqualTo',2).

lessThan(X,Y):-greaterThan(Y,X),!.

lessThanOrEqualTo(X,Y):-!,greaterThanOrEqualTo(Y,X),!.

greaterThan(X,Y):-not((atom(X);atom(Y))),(ground((X,Y)),!,eval_lr(X,RX),!,eval_lr(Y,RY),!,sandbox(RX > RY)),!.

greaterThanOrEqualTo(X,Y):-(ground((X,Y)),not((atom(X);atom(Y))),!,eval_lr(X,RX),!,eval_lr(Y,RY),!,sandbox(RX >= RY)),!.

'~lessThan'(X,Y):-'~greaterThan'(Y,X).

'~lessThanOrEqualTo'(X,Y):-'~greaterThanOrEqualTo'(Y,X).

'~greaterThan'(X,Y):-!,not(greaterThan(X,Y)),!.
'~greaterThanOrEqualTo'(X,Y):-not(greaterThanOrEqualTo(X,Y)),!.

% Handbox is like Sandbox but makes sure turms are evaluatable
handbox(X):-catch(handbox1(X),_,fail).
handbox1(X > Y):-!,RX is X, RY is Y,RX > RY.
handbox1(X >= Y):- RX is X, RY is Y,RX >= RY.


% TODO Properly implement



:-arithmetic_function('BillionDollars'/1).
:-arithmetic_function('BillionDollars'/2).
:-arithmetic_function('Fn'/2).

'Fn'(F,A,R):-catch(eval_lr('Fn'(F,A),R),_,fail).


%:-include('sigma_header.pl').


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

/*'instance'(X,Y):-is_instance_of(X,Y).

'~instance'(X,Y):-is_instance_of(X,Y),!,fail.
'~instance'(X,Y):-isNonVar(Y),!.

predicates_declared_inline_HL('t_instance',2).
predicates_declared_inline_HL('~t_instance',2).
  */


predicates_declared_inline_HL('~u',3).
predicates_declared_inline_HL('u',3).
%u(A,B,'Quantity'):-'equal'(X,Y).


%u(_,_,'Skolem'):-!,fail.
u('zzskFn'(A),B,'Skolem'):-nonvar(A),!,unify_with_occurs_check(A,B),!.


u(A,B,_):-!,A==B.

u(A,B,_):-!,unify_with_occurs_check(A,B),!.
%u(A,B,_):-catch(unify_with_occurs_check(A,B),E,fail),!.
%u(A,B,_):-sigmaCache(PredR,surface,'equal'(A,B):L,KB,Ctx,TN,Author,On).

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
/*
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



	      */
	


/*
% =====================================================================================
% UNIFICATION
% =====================================================================================
iinfer_cache(true,Depth,Table,u(A,B,Class),Agent,Proof):-writeDebug(u(A,B,Class)),
		infer_cache_u(Table,A,B,Class,Agent,Proof).
		

infer_cache_u(Table,A,B,Class,Agent,Proof):-Class=='Class',!,fail.
infer_cache_u(Table,U,W,'Skolem',Agent,U):-!,unify_with_occurs_check(U,W),!.
infer_cache_u(Table,U,W,_,Agent,U):-!,unify_with_occurs_check(U,W).

%infer_cache_u(Table,A,B,Class,Agent,Proof):-var(A),!,infer_cache_u_var(Table,A,B,Class,Agent,Proof).
%infer_cache_u(Table,A,B,Class,Agent,Proof):-atomic(A),!,infer_cache_u_atom(Table,A,B,Class,Agent,Proof).
%infer_cache_u(Table,A,B,Class,Agent,Proof):-atomic(B),!,infer_cache_u_compound_atom(Table,B,A,Class,Agent,Proof).
%infer_cache_u(Table,A,B,Class,Agent,Proof):-infer_cache_u_compound_compound(Table,B,A,Class,Agent,Proof).

infer_cache_u_atom(Table,Class,B,Odd,Agent,Proof):-isa_class(Class),!,fail.
infer_cache_u_atom(Table,A,B,Class,Agent,Proof):-atomic(B),!,infer_cache_u_atom_atom(Table,A,B,Class,Agent,Proof).
infer_cache_u_atom(Table,A,B,Class,Agent,Proof):-infer_cache_u_atom_compound(Table,A,B,Class,Agent,Proof).

%TODO Reduce if Possible
infer_cache_u_var(Table,A,B,Class,Agent,instance(B,Class)):-unify_with_occurs_check(A,B),!.

infer_cache_u_atom_atom(Table,A,A,Class,Agent,instance(A,Class)).

%Analogy TODO
infer_cache_u_atom_compound(Table,A,B,Class,Agent,P):-!,equal(B,A,P).

infer_cache_u_compound_atom(Table,A,B,Class,Agent,P):-!,equal(A,B,P).


% More Analogy
infer_cache_u_compound_compound(Table,A,B,Class,Agent,instance(A,Class)):-unify_with_occurs_check(A,B),!.
infer_cache_u_compound_compound(Table,A,B,Class,Agent,Proof):-A=..[PA|AL],B=..[BA|BL],!,infer_cache_u_compound_compound(Table,A,PA,AL,B,PB,BL,Class,Agent,Proof).

infer_cache_u_compound_compound(Table,A,'E',[SK|LA],B,'E',[SK|LB],Class,Agent,Proof):-!,infer_cache_u_same_skolems(Table,A,B,SK,LA,LB,Class,Agent,Proof).
infer_cache_u_compound_compound(Table,A,'E',[AL|LA],B,'E',[BL|LB],Class,Agent,Proof):-!,fail,infer_cache_u_diff_skolems(Table,A,AL,LA,B,BL,LB,Class,Agent,Proof).

infer_cache_u_same_skolems(Table,A,B,SK,LA,LB,Class,Agent,and(instance('E'(SK, B),Class),nearIdent('E'(SK, B),A,P1,P2))):-
		writeDebug(deskolemize(u(A, B, Class))),
		sigmaCache(PredR,(u(A, 'E'(SK, B), Class)),Agent,ANTE,P1),
		writeDebug(deskolemize_rule(ANTE)),
		Depth2 is Depth-1,

		infer_backchain(true,Depth2,Table,ANTE,Agent,P2).


iinfer_cache(true,Depth,Table,'~u'(U,W,'Skolem'),Agent,deduced):-!,not((unify_with_occurs_check(U,W))),!.
iinfer_cache(true,Depth,Table,'~u'(U,W,FS),Agent,deduced):-not(unify_with_occurs_check(U,W)),!.
iinfer_cache(true,Depth,Table,'~u'(A,B,Class),Agent,sfind(instance(B,Class))):-!,not(unify_with_occurs_check(A,B)).


								       */
								       


