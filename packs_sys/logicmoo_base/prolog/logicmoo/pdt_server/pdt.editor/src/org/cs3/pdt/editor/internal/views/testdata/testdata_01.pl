/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Tobias Rho
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2013, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

/*
 * Structure of the file:
 *
 * - conditions used by CTs
 * - actions    used by CTs
 * - dynamic predicates
 * - auxiliary predicates
 *
 * Author:      Tobias Rho
 * Date:        30.08.02
 * Last Change: 13.09.05
 */

/* *** PUBLIC API **********************************************/

/* *** CONDITIONS **********************************************
 *
 * Predicates used in the CT conditions generation.
 * These predicates are marked with CONDITION 
 * in the documentation.
 *
 */
cond(matchParams(_ParametersOrArgs, _PatternList)).
cond(extract_class_name(_FQN,_PackageName, _ClassName)).

/* *** ACTIONS *************************************************
 *
 * Documentation copied from JTEngine/api/high_level_api.pl:
 *
 * Every predicate in the post action part of a
 * CT must be tagged with action/1.
 * Otherwise ct_apply/1 will throw an exception.
 * This design was choosen to be able to give actions
 * simple names which are be mapped to full names
 * when applied. Ambiguities with Prolog build-ins are avoided.
 */

action( before(JP, Statements,ForwMethod,ForwBody)) :-              before(JP, Statements,ForwMethod,ForwBody), !.
action( after(JP, Statements,ForwMethod,ForwBody)) :-               after(JP, Statements,ForwMethod,ForwBody), !.
action( around(JoinPoint, _aroundStmts,_forwMethod,_forwBody)):-    around(JoinPoint,_aroundStmts,_forwMethod,_forwBody).

action( add_unboxing_return(_return, _parent, _encl, _expr)):-      add_unboxing_return(_return, _parent, _encl, _expr).
%action( add_advice_param_ref(_pc,_adviceParam,_id,_parent,_encl)):- add_advice_param_ref(_pc,_adviceParam,_id,_parent,_encl).
%malte
%action( add_proceed_call(_pc,_call,_parent, _enclMethod,_adviceArgs,_proceedArgs)):-add_proceed_call(_pc,_call, _parent, _enclMethod,_adviceArgs,_proceedArgs).
action(add_advice_instance_method_parameters(AdviceInstanceMethod, JP, AdviceParametersAndKinds,Parameters)) :-
add_advice_instance_method_parameters(AdviceInstanceMethod, JP, AdviceParametersAndKinds,Parameters).

action(join_point_exceptions(JP,Exception)):-join_point_exceptions(JP,Exception).
action( concat_lists(A,B)):-    concat_lists(A,B).
action( addArgList(FnArgs, PcArgs, Idents, Parent, ForwMethod)):-   addArgList(FnArgs, PcArgs, Idents, Parent, ForwMethod).
action( addArg(FnArg,PcArgs,Ident,Parent,ForwMethod)):-             addArg(FnArg,PcArgs,Ident,Parent,ForwMethod).	    
action( addParamList(Params, Ids,Parent)) :-                        addParamList(Params, Ids,Parent).
action( addParamReferenceList(Refs, Params, Parent,Encl)) :-        addParamReferenceList(Refs, Params, Parent,Encl).
action( add_to_class_fq(Class,Member)):-             				add_to_class_fq(Class,Member).
action( copy_method_body(Method,BodyToCopy,Body)) :-                copy_method_body(Method,BodyToCopy,Body).
action( showError(Kind,ID,Msg)):-                                   showError(Kind,ID,Msg).


    


/*
 * addArgList(FnArgs, PcArgs, Ids, Parent, ForwMethod)
 * 
 * ACTION
 *
 * Recursive implementation of addArg/5, first parameter is a list
 * insead of on argument.
 * see addArg/5 documentation
 * for details.
 *
 */    
addArgList([], _, [], _,_).
addArgList([FnArg|FnArgs], PcArgs, [Ident|Idents], Parent, ForwMethod) :-
    addArg(FnArg,PcArgs,Ident,Parent,ForwMethod),
    addArgList(FnArgs, PcArgs, Idents, Parent, ForwMethod).

/*
 * addArg(+FnArg,+JpArgs,+Ident,+Parent,+ForwMethod)
 * 
 * ACTION
 *
 * adds a reference (identT) to an advice parameter.
 * Parameters:
 *   FnArgs: The selected advice parameter
 *   JpArgs: The arguments at the join point (including target and this)
 *   Ident:  The new Id
 *   Parent: The parent of the Id
 *   ForwMethod: The enclosing forwarding method
 * 
 */    
 
addArg(FnArg,PcArgs,Ident,Parent,ForwMethod):-
    methodT(ForwMethod,_,_,[_This,_Target|ArgParams],_,_,_),
    lookupForwParameter(ForwMethod, FnArg,PcArgs,ArgParams,Param, Name),
    add(identT(Ident,Parent,ForwMethod,Name,Param)).

/*
 * add_advice_instance_method_parameters(+AdviceInstanceMethod, +JP, +AdviceParametersAndKinds,ThisType, TargetType, Parameters)
 *
 * @param AdviceParametersAndKinds [(Param, Kind),...] : Kind = target, this, argument id at join point
 */
 add_advice_instance_method_parameters(AdviceInstanceMethod, JP, AdviceParametersAndKinds,
                                     [ThisParam,TargetParam|ArgParameters]) :-
    %get_join_point_arguments(JP, Args), 
    local_vars_of_jp(JP, ArgsTemp),
    lvar_ids(ArgsTemp,Args2),

    getReceiverTypeOrEnclosingIfInAnonymousClass_fq(JP,Type),    
  	enclClass(JP, EnclClass),
  	(
  	anonymousClass(EnclClass) ->
  		(
  		classT(EnclClass,Parent,_,_),
	  	identT(_,Parent,_,_,EnclClassTmp),
  		fullQualifiedName(EnclClassTmp,ClassName)
  		);
  		enclClass_fq(JP,ClassName)
  	),  		
    get_or_add_parameter(AdviceInstanceMethod,this,AdviceParametersAndKinds, ClassName, ThisParam),
    get_or_add_parameter(AdviceInstanceMethod,target,AdviceParametersAndKinds, Type, TargetParam),    
    get_or_add_parameter_if_afterThrowingOrReturning_param_exists(	AdviceParametersAndKinds,AfterParam),    
    get_or_add_argument_parameters(AdviceInstanceMethod, Args, -1, AdviceParametersAndKinds, CurrentArgParameters),
    (
    	nonvar(AfterParam)->
    		concat_lists([AfterParam,CurrentArgParameters,Args2],ArgParameters) ;
    		concat_lists([CurrentArgParameters,Args2],ArgParameters)
    ).
    

/*
 *
 */
get_or_add_argument_parameters(_, [],_, _, []).

get_or_add_argument_parameters(AdviceInstanceMethod, [Arg|Args], Num, 
                               AdviceParametersAndKinds, [ArgParameter|ArgParameters]) :-
    plus(Num, 1, Next),
    Arg = lvar(Argument,_),
	get_or_add_argument_parameter(AdviceInstanceMethod, Argument, Next, AdviceParametersAndKinds, ArgParameter),
	get_or_add_argument_parameters(AdviceInstanceMethod, Args, Next, AdviceParametersAndKinds, ArgParameters).

    get_or_add_argument_parameter(_, Argument, _,AdviceParametersAndKinds, ArgParameter):-
    memberchk((ArgParameter, Argument),AdviceParametersAndKinds),
    !.
    

%exeption for execution pc
get_or_add_argument_parameter(AdviceInstanceMethod, Arg, _Num, _, ArgParameter):-    
  	aopT(JP,_,AdviceInstanceMethod),
   	methodT(JP,_,_,_,_,_,_),
    new_id(ArgParameter),       
    paramT(Arg,  _, _, ArgName)    ,
    getType(Arg,Type),
  	add(paramT(ArgParameter,  AdviceInstanceMethod, Type, ArgName)).
    	 
    	 
get_or_add_argument_parameter(AdviceInstanceMethod, Arg, Num, _, ArgParameter):-
    new_id(ArgParameter),
    getType(Arg,Type),
    get_arg_name(Arg,Num, ArgName),
    add(paramT(ArgParameter,  AdviceInstanceMethod, Type, ArgName)). 
 


get_arg_name(ArgID,Num,ArgName):-    
    paramT(ArgID,  _, _, ArgName);
    atom_concat('_arg', Num, ArgName).


/*
  * get_or_add_parameter_if_afterThrowingOrReturning_param_exists(+AdviceParametersAndKinds, -Param)
  *
  * if the current advice is an afterThrowing oder afterReturning advice, the extra formal will be added to
  * the advice method parameters on the thrid position
  *
  */  
  
get_or_add_parameter_if_afterThrowingOrReturning_param_exists(AdviceParametersAndKinds, Param) :-
    member((Param, extraArg), AdviceParametersAndKinds),
    !.

get_or_add_parameter_if_afterThrowingOrReturning_param_exists( _, _).

 
/*
 *get_or_add_parameter(_,+Kind,+AdviceParametersAndKinds,_,-Param) :-
 */  
get_or_add_parameter(_,Kind,AdviceParametersAndKinds,_,Param) :-
    member((Param, Kind), AdviceParametersAndKinds),
    !.
    

get_or_add_parameter(AdviceInstanceMethod,'target', _, TargetType, Param) :-
	createTargetInstanceParam(TargetType, AdviceInstanceMethod, Param),
	!.
get_or_add_parameter(AdviceInstanceMethod,'this', _, TargetType, Param) :-
	createThisInstanceParam(TargetType, AdviceInstanceMethod, Param),
	!.

/*
 * get_join_point_arguments(JP, Args)
 *
 */
 get_join_point_arguments(JP, Args) :-
  methodT(JP,_,_,Args,_,_,_),
  !.
  
get_join_point_arguments(JP, Args) :-
  methodCall(JP,_,_,_,_,Method,_),
  methodT(Method,_,_,Args,_,_,_),
  !.
get_join_point_arguments(JP, []) :-
  getFieldT(JP,_,_,_,_,_),
  !.
get_join_point_arguments(JP, [Arg]) :-
  setField(JP,_,_,_,_,Arg),
  !.
  
 get_join_point_arguments(_, []).  
/*
  * join_point_exceptions(+JP, -Exceptions)
  *
  */

join_point_exceptions(JP,Exceptions):-
  methodT(JP,_,_,_,_,Exceptions,_).
  
join_point_exceptions(JP,Exceptions):-
	methodCall(JP,_,_,_,_,MID,_),
	methodT(MID,_,_,_,_,Exceptions,_).
 
join_point_exceptions(_,[]).




/*
 * matches_jp_exceptions(+ExceptionFQ,-ExceptionIDs)
 *
 * TESTED
 */
matches_exceptions(ExceptionFQ,ExceptionIDs):-    
    fullQualifiedNames(ExceptionIDs,ExceptionsFQ),
    memberchk(ExceptionFQ,ExceptionsFQ),
    !.
    
matches_exceptions(ExceptionFQ,[Head | Tail]):-    
    fullQualifiedName(Head,FQ),  
    (
    	subtype_name(FQ,ExceptionFQ); 
	    matches_exceptions(ExceptionFQ,Tail)
	).

matches_exceptions(_,[]):- fail.    
    

    
    

/*
 * getTargetType(+JpID,-RecieverType)
 *
 *
 */
getReceiverType(JpID,RecieverType):-       
  applyT(JpID,_,_,_,_,_,Member),
  enclClass(Member,EnclClass),
  getType(EnclClass,RecieverType).

getReceiverType(JpID,RecieverType):-           
  getFieldT(JpID,_,_,_,_,Member),
  enclClass(Member,EnclClass),
  getType(EnclClass,RecieverType).
      
getReceiverType(JpID,RecieverType):-             
  setField(JpID,_,_,_,Member,_),    	
  enclClass(Member,EnclClass),
  getType(EnclClass,RecieverType).
    
getReceiverType(JpID,RecieverType):-             
  methodT(JpID,_,_,_,_,_,_),
  enclClass(JpID,EnclClass),
  getType(EnclClass,RecieverType).
  
  
/*
 * add_unboxing_return(Return, _parent, EnclMethod, _expr)
 *
 * ACTION
 *
 * If EnclMethod has the return type void an empty returnT statment
 * with the id Return is created.
 * 
 * Otherwise the unboxingMethod ct is used to create a method 
 * in the enclosing class of EnclMethod which unboxes a 
 * the corresponding boxing type of PrimitiveType to
 * PrimitiveType, e.g. for the basic type int:
 *   public int intValue(Integer integer) { return ... }
 *
 * TESTED
 */
add_unboxing_return(Return, Parent, EnclMethod, Expr):-
    methodT(EnclMethod,_,_, _,type(basic,void,0),_,_),
    !,
    deleteTree(Expr), 
    add(returnT(Return,Parent,EnclMethod,null)).

add_unboxing_return(_return, _parent, _encl, _expr):-
    enclClass(_encl,_enclClass),
    methodT(_encl,_,_, _,type(basic,_primitiveType,0),_,_),
    !,
    apply_ct(unboxingMethod(_enclClass,_primitiveType)),
    atom_concat(_primitiveType, 'Value', _primitiveTypeValue),
    methodT(_unboxingMethod, _enclClass,_primitiveTypeValue,[_],type(basic, _primitiveType, 0),[],_),
    new_ids([_apply,_select,_ident]),
    atom_concat(_primitiveType,'Value',_methodName),
    add(returnT(_return,_parent,_encl,_apply)),
      add(applyT(_apply,_return,_encl,_ident,_methodName, [_expr],_unboxingMethod)),
          add(identT(_ident,_apply,_encl,'this',_enclClass)),
    set_parent(_expr,_apply).

add_unboxing_return(_return, _parent, _encl, _expr):-
    add(returnT(_return,_parent,_encl,_expr)).
  
%/*
% * add_advice_param_ref(JoinPoint,AdviceParam,Id,Parent,EnclMethod)
% *
% * ACTION
% * 
% * creates a reference (identT) to the advice 
% * parameter "AdviceParam" with the id "Id".
% */
%
%add_advice_param_ref(JP,_adviceParam,_id,_parent,_encl):-
%    getForwParam(_pc, _adviceParam, _param,_name),
%    add(identT(_id,_parent,_encl,_name,_param)).
    
    /*
 * showError(+Kind, +ID,+Msg)
 * 
 * ACTION
 */
showError(_,ID,Msg):-    
	var(ID),
	write('ID NOT BOUND: '),
	write(Msg),
	flush_output.

showError(Kind, ID,Mesg):-
    not(sourceLocation(ID, _,_,_)),
    enclMethod(ID,Meth),
    method(Meth,Class,Name,_,_,_,_),
    fullQualifiedName(Class,Fqn),
    format('~w in method ~w.~w: ~w~nCannot find source location: Probably generated by aspect.~n~n',[Kind,Fqn,Name,Mesg]).
    

showError(Kind,ID,Msg):-    
    enclMethod(ID,Meth),
    method(Meth,Class,Name,_,_,_,_),
    fullQualifiedName(Class,Fqn),
    format('~w in method ~w.~w  ',[Kind,Fqn,Name]),
    sourceLocation(ID, File,Start,_),
    format('(~w:~w)~n~n~w~n', [File,Start,Msg]),
    gen_tree(ID),
    flush_output,

    /*
    * Added Dec 20, 2004 to store all errors/warnings
    * and move it to the Eclipse Problems View. (AL)
    */
    assert(isErrorWarningMessage(Kind, ID, Msg)).
    
    
/*
 * addParamList(+Params, +Ids,+Parent)
 *
 * ACTION
 */

addParamList([],[],_).
addParamList([Param|Params], [Id|Ids],Parent) :-
    paramT(Param,_,Type,Name),
    add(paramT(Id,Parent,Type,Name)),
    addParamList(Params, Ids,Parent).    
    
/*
 * addParamReferenceList(+Refs, +Params, +Parent,+Encl)
 *
 * ACTION
 * Adds a list of parameter accesses.
 * Parent is the parent id and Encl the 
 * enclosing id of the created identT/5 facts.
 */
addParamReferenceList([], [], _Parent,_Encl).

addParamReferenceList([Ref|Refs], [Param|Params], Parent,Encl) :-
    paramT(Param,_,_,Name),
    add(identT(Ref,Parent,Encl,Name,Param)),
	addParamReferenceList(Refs, Params, Parent,Encl).
    
    
    
    
    
/*
 * add_to_class_fq(+Class,+Member|+MemberList)
 * 
 * ACTION
 * Adds Member(s) to the class, if the Member is not already in the 
 * member list.
 * Fails if Class or Member is not bound and if Class is not a
 * of type classT.
 *
 * TESTED
 */
add_to_class_fq(_, []):- !.
add_to_class_fq(Class, [Member|Rest]) :-
    add_to_class_fq(Class,Member),
    add_to_class_fq(Class,Rest).

add_to_class_fq(_class, _id) :-
    nonvar(_class),
    nonvar(_id),
    java_fq(classT(_class, _, _, _members)),
    member(_id, _members),
    !.
    
add_to_class_fq(_class, _id) :-
    nonvar(_class),
    nonvar(_id),
    java_fq(classT(_class, _p,_n,_members)),
    delete(java_fq(classT(_class, _p,_n,_members))),
    append(_members, [_id], _newMembers),
    add(java_fq(classT(_class, _p, _n, _newMembers))).
 
add_to_class_fq(_class, _id) :-
    sformat(Msg,'the class ~w could not be found in add_to_class',[_class]),
    t_i_based_error_handling(Msg).
    

/************************** CT CONDITIONS (cond/1) *************************/

/*
 * bindForwMethod(+JoinPoint,-ForwardingMethod,-Body)
 *
 * CONDITION
 *
 * binds forwarding method and body to
 * new ids.
 * In the case of a execution pointcut
 * on the body is bound.
 */
/*
bindForwMethod(_Method,_Method,_Body):-
% special case for the execution pointcut
    methodT(_Method,_,_,_,_,_,_),
    !,
    new_id(_Body).

bindForwMethod(_,_forwMethod,_forwBody):-
    new_ids([_forwMethod,_forwBody]).
*/
/*
 * extract_class_name(+FQN,-Package, -ClassName)
 *
 * CONDITION
 * Atom operation. Binds ClassName to 
 * atom part right from most right '.'.
 *
 * e.g.: class_name('pckg1.pckg2.Class1','pckg1.pckg2','Class1').
 *
 * TESTED
 */

extract_class_name(FQN, _PackageName, _ClassName) :-
	var(FQN),
	throw('INTERNAL ERROR: extract_class_name failed').

extract_class_name(FQN, Package, ClassName) :-
    extract_class_name_(FQN, FQN, Package, ClassName).
	
extract_class_name_(FQN, ClassNameTmp, Package, ClassName) :-
    concat(_,'.',ClassNameTmp2,ClassNameTmp),
    !,
    extract_class_name_(FQN, ClassNameTmp2, Package, ClassName).
    
extract_class_name_(FQN, ClassName, Package,ClassName):-
    concat(Package, '.', ClassName, FQN),
    !.
extract_class_name_(_, ClassName, '',ClassName).   
   
/*** DYNAMIC PREDICATES ***************************************/

:- multifile fieldAccess/2.
:- multifile action/1.

/*
 * forwarding(Forwarding, LastForwarding,Jp)
 * 
 * except for the first fact:
 * forwarding(Forwarding,Jp,Jp)
 */
:- dynamic forwarding/3.
:- dynamic forwarding/1.

/*
 * forwards(LastCall, LastForwarding, Kind, Jp)
 *
 * LastCall: the call at the real join point
 * LastForwarding: the method called by this call
 * Kind: getField | setField | execution | methodCall
 */
:- dynamic forwards/4.
:- dynamic pointcut/1.
:- dynamic visibility/3.
:- dynamic laj_ct_list/1.
:- multifile laj_ct_list/1.
:- dynamic laj_binding_transfer/3.

/*** AUXILIARY PREDICATES ***************************************/
  
/*
 * replaceStatementWithForwarding(JoinPoint) 
 *
 * see replaceStatementWithForwarding(JoinPoint,ForwMethod,ForwBody).
 */
 
replaceStatementWithForwarding(JP) :-
    replaceStatementWithForwarding(JP,_,_).

/*
 * replaceStatementWithForwarding(JoinPoint,ForwMethod,ForwBody) 
 *
 * Replaces a Joinpoint execution with the call of a forwarding method.
 * 
 * If ForwMethod and ForwBody are not bound they are bound to new ids.
 * Then, a new forwarding method is created
 * 
 */

replaceStatementWithForwarding(JP,ForwMethod,ForwBody) :-
    bindIdIfNeeded(ForwMethod),
    bindIdIfNeeded(ForwBody),
    (
        (forwards(RealJP, _, _,JP),!);
        JP = RealJP
    ),
    enclClass(RealJP, EnclClass),
    !,
    createForwardingMethod(RealJP, EnclClass,ForwMethod,ForwBody),
    add_encl_meth_params(RealJP).

/*
 * add_encl_meth_params(+Pc) 
 */
add_encl_meth_params(JP):-
    forwards(_,_,execution,JP),
    !.
    %throw('add_encl_meth_params not allowed for execution').
 
add_encl_meth_params(JP):-
    forwards(_call,LastForwMethod,Kind,JP),
    pc_param_num(JP,Kind,PN),
    add_encl_params_to_forw_if_need(JP,PN,LastForwMethod).
    
/*
 * add_encl_params_to_forw_if_need(+Pc,+PN,+ForwMethod)
 *
 * Add parameters of the enclosing method at the
 * real pointcut to the forwarding methods.
 * This predicate is TODO (X,X equiv. to. X)
 */

add_encl_params_to_forw_if_need(_,_,ForwMethod):-
 	not(methodT(ForwMethod,_,_,_,_,_,_)),
 	!.
 	
 	add_encl_params_to_forw_if_need(_,PN,ForwMethod):-
    methodT(ForwMethod,_,_,Params,_,_,_),
    length(Params,PN),
    !.
 	
add_encl_params_to_forw_if_need(Pc,PN,ForwMethod):-
    methodT(ForwMethod,Class,Name,Params,Type,Exc,Body),
	getRealEncl(Pc,ForwMethod,RealEncl),
	methodT(RealEncl,_,_,EnclParams,_,_,_),
	copy_params(EnclParams,CopiedParams,ForwMethod),
	concat_lists([Params,CopiedParams],NewParams),
	replace(methodT(ForwMethod,Class,Name,Params,Type,Exc,Body),
    	    methodT(ForwMethod,Class,Name,NewParams,Type,Exc,Body)),
% replace forwarding call
    applyT(Apply,Parent,Encl,Expr,Name,Args,ForwMethod),
	
    methodT(RealEncl, _,_,RealParams,_,_,_),
    length(RealParams,Len),
  
    create_refs_to_encl_params(Encl,Len,Refs),
	concat_lists([Args,Refs],NewArgs),
	replace(applyT(Apply,Parent,Encl,Expr,Name,Args,ForwMethod),
    	    applyT(Apply,Parent,Encl,Expr,Name,NewArgs,ForwMethod)),
    forwarding(ForwMethod,LastForwMethod,Pc),
    add_encl_params_to_forw_if_need(Pc,PN,LastForwMethod).


/*
 * 	copy_params(+EnclParams,?CopiedParams,+ForwMethod)
 * 
 *  ?f CopiedParams is not bound, new ids will be created for 
 *  the new parameters.
 */
copy_params([],[],_NewEncl).
copy_params([Param|Params],[Copy|Copies],NewEncl):-
    (var(Copy)-> 
       new_id(Copy);true),
    paramT(Param,_,Type,Name),
    add(paramT(Copy,NewEncl,Type,Name)),
    copy_params(Params,Copies,NewEncl).

/*
 * create_refs_to_encl_params(+Encl,+Len,-Refs)
 *
 * create idents which reference the Len-length tail 
 * of parameters of the method Encl.
 */

create_refs_to_encl_params(Encl,Len,Refs) :-
    methodT(Encl, _,_,Params,_,_,_),
    tail(Params,Len,Tail),
	create_ref_idents(Tail,Refs).

create_ref_idents([],[]).
create_ref_idents([Param|Params],[Ref|Refs]):-
	paramT(Param,Parent,_,_),
	createIdentRefParam(Param,Parent,Ref),
	create_ref_idents(Params,Refs).
   
/*
 * pc_param_num(+Pc,+Kind,?PN)
 */
pc_param_num(_Pc,getField,2).
pc_param_num(_Pc,setField,3).
pc_param_num(Pc,methodCall,PN) :-
    (applyT(Pc,_,_,_,_,Args,_);
     newClassT(Pc,_,_,_,Args,_,_,_)),
    getRealEncl(Pc,_,RealEncl),
    methodT(RealEncl,_,_,Params,_,_,_),
    length(Params,ParamsLen),
    length(Args,ArgsLen),
    plus(2,ArgsLen,Tmp),
    plus(Tmp,ParamsLen,PN).

pc_param_num(Pc,execution,PN) :-
    methodT(Pc,_,_,Args,_,_,_),
    length(Args,ArgsLen),
    plus(2,ArgsLen,PN).
      

/*
 * Encloses current body with a try finally block and inserts 
 * 
 * 
 Umschlie�e aktuellen Body mit einem try .. finally block und f�ge _insert in den finally Block ein,
 * wenn _insert ein Block ist wird insert als finally block eingef�gt.
c */

addTryFinallyBlockStmts(_,_, []).
addTryFinallyBlockStmts(_forwMethod,_finallyBlock, _stmts) :-
    addTryFinallyBlock(_forwMethod,_finallyBlock),
    prependBlockStatments(_finallyBlock, _stmts).

addTryFinallyBlock(_forwMethod,_finallyBlock):-
    new_ids([_try, _tryBlock]),
    methodT(_forwMethod,_,_,_,_,_,_block),
    blockT(_block, _parent, _encl, _stats), % die id des blocks wird dem try zugewiesen
    set_parent(_stats, _tryBlock),
    delete(blockT(_block, _parent, _encl, _stats)),
    add(blockT(_block, _parent,_encl,[_try])),
        add(tryT(_try, _block,_encl,_tryBlock, [],_finallyBlock)),
            add(blockT(_tryBlock, _try,_encl,_stats)),
            add(blockT(_finallyBlock, _try, _encl, [])).

/*
aroundTryFinallyBlock(_block, _finallyBlock) :-
    blockT(_finallyBlock,_,_,_),
    !,
    new_ids([_try, _tryBlock]),
    blockT(_block, _parent, _encl, _stats), % die id des blocks wird dem try zugewiesen
    set_parent(_stats, _tryBlock),
    retractall(blockT(_block, _,_,_)),
    assert(blockT(_block, _parent,_encl,[_try])),
        assert(tryT(_try, _block,_encl,_block, [],_finallyBlock)),
            assert(blockT(_tryBlock, _try,_encl,_stats)),
            set_parent(_finallyBlock, _try),
            set_encl_method(_finallyBlock, _encl).

aroundTryFinallyBlock(_block, _insert) :-
    new_ids([_try, _tryBlock, _finallyBlock]),
    blockT(_block, _parent, _encl, _stats), % die id des blocks wird dem try zugewiesen
    set_parent(_stats, _tryBlock),
    retractall(blockT(_block, _,_,_)),
    assert(blockT(_block, _parent,_encl,[_try])),
        assert(tryT(_try, _block,_encl,_tryBlock, [],_finallyBlock)),
            assert(blockT(_tryBlock, _try,_encl,_stats)),
            assert(blockT(_finallyBlock, _try, _encl, [])),
%    set_parent(_insert,_finallyBlock),
    prependBlockStatment(_finallyBlock, _insert).
*/


createAdviceMethod(JP,Statements,ForwMethod,ForwBody):-
   enclClass(JP,Class),
   add(methodT(ForwMethod,Class,'advice',_,_,_,ForwBody)),
   add(blockT(ForwBody,ForwMethod,ForwMethod,Statements)).


/*
 * createThisOrGetReceiver(OldParent, NewParent, Encl, OldReceiver, NewReceiver, DeclaringType) 
 */

createThisOrGetReceiver(Parent, _newParent, _encl, null, Receiver,DeclaringType) :-
%    getFieldT(_ident, Parent, _encl, 'null', _name, _sym),
    enclClass(_encl,Anonym),
    classT(Anonym,NewClass,_,_),
    newClassT(NewClass,_,NewClassEncl,_,_,TypeExpr,_,_),
    getType(TypeExpr,type(class, Type,0)),
    not(subtype(Type,DeclaringType)),
    !,
    enclClass(NewClassEncl, Outer),
    
    (var(Receiver) -> new_id(Receiver);true),
    fullQualifiedName(Outer,FQN),
    new_id(SelectedTypeExpr),
    add(selectT(Receiver,Parent,EnclMethod,'this', SelectedTypeExpr, Outer)),  
    add(identT(SelectedTypeExpr, Receiver, EnclMethod, FQN,Outer)).



createThisOrGetReceiver(_Parent, NewParent, Encl, null, Receiver,_DeclaringType) :-
    !,
    enclClass(Encl, EnclClass),
    new_id(Receiver),
    create_this_or_null_if_static(Receiver,NewParent, Encl, EnclClass).

createThisOrGetReceiver(_Parent, NewParent, _encl, Receiver, Receiver,_DeclaringType) :-
%createThisOrGetReceiver(GetField, NewParent, Receiver,_DeclaringType) :-
    !,
    set_parent(Receiver, NewParent).

/*
 * create_this_or_null_if_static(ID,Parent,EnclMethod,EnclClass)
 *
 * Will create a reference to this, if EnclMethod is not a static method.
 * Otherwise a null ident will be created.
 * Will create a new ID, if ID is not bound!
 */ 
create_this_or_null_if_static(ID, Parent, EnclMethod, _EnclClass):-
    modifierT(EnclMethod,'static'),
    !,
    (var(ID) -> new_id(ID);true),
    add(identT(ID, Parent, EnclMethod, 'null', 'null')).

create_this_or_null_if_static(ID, Parent, EnclMethod, EnclClass):-
    (var(ID) -> new_id(ID);true),
    add(identT(ID, Parent, EnclMethod, 'this', EnclClass)).


/** BEFORE helper - START*/
createForwBody(_get, _forwMethod, _forwBody, _ForwName, [_thisParam,_recvParam],/* _Type*/type(basic, void, 0), _execReturn) :-
    getFieldT(_get, _parent, _enclMethod, _origReceiver,  _origName, _field),
    !,
    new_ids([_forwCall, _forwMethIdent]),
    fieldT(_field, DeclaringType, _Type, _origName, _),
    forwardingMethodName(_get, 'get$', _origName, _ForwName),
    enclClass(_enclMethod,_enclClass), %neu
    createForwMethParams(_enclClass,_forwMethod,DeclaringType, _origReceiver, [],[_thisParam,_recvParam]),
    createIdentRefParam(_recvParam,_callSelect, _forwReceiver),
%createThisOrGetReceiver(OldParent, NewParent, Encl, OldReceiver, NewReceiver, DeclaringType)     
    createThisOrGetReceiver(_parent, _forwCall, _enclMethod,_origReceiver,_receiver,DeclaringType),
    replaceId(_parent, _get, _forwCall),
    delete(getFieldT(_get, _parent, _enclMethod, _origReceiver,  _origName, _field)),
    create_this_or_null_if_static(_this,_forwCall, _enclMethod, _enclClass), %neu
    add(getFieldT(_get, _execReturn, _forwMethod, _forwReceiver, _origName, _field)),
    add(applyT(_forwCall, _parent,_enclMethod, 'null', _ForwName, [_this,_receiver],_forwMethod)),
    updateForwardsFact(_get,getField,_forwCall,_forwMethod).

createForwBody(_set, _forwMethod, _forwBody, _ForwName, [_thisParam,_recvParam,_valueParam],  /*FieldType*/type(basic, void, 0), _execReturn) :-
    setField(_set, _parent, _enclMethod, _origReceiver, _field,_value),
    !,
    new_ids([_forwCall, _forwMethIdent, _selectField,_valueParam]),
    assignT(_set, _, _enclMethod, _lhs, _),
    fieldT(_field, DeclaringType, _FieldType, _origName, _),
    getType(_value,_Type),
    forwardingMethodName(_set,'set$', _origName, _ForwName),
    enclClass(_enclMethod,_enclClass), %neu
    createForwMethParams(_enclClass,_forwMethod,DeclaringType,_origReceiver, [],[_thisParam,_recvParam]),
    createIdentRefParam(_recvParam,_callSelect, _forwReceiver),
 %createThisOrGetReceiver(OldParent, NewParent, Encl, OldReceiver, NewReceiver, DeclaringType)     
    createThisOrGetReceiver(_parent, _forwCall, _enclMethod,_origReceiver,_receiver,DeclaringType),
    replaceId(_parent, _set, _forwCall),
    set_parent(_value, _forwCall),
    deleteTree(_lhs),
    add(paramT(_valueParam, _forwMethod, _Type, '_value')),
    createIdentRefParam(_valueParam,_set, _forwValue),
    create_this_or_null_if_static(_this,_forwCall, _enclMethod, _enclClass), %neu
    action(replace(assignT(_set, _execReturn, _forwMethod, _selectField, _forwValue))),
      add(getFieldT(_selectField, _set, _forwMethod, _forwReceiver, _origName, _field)),
    add(applyT(_forwCall, _parent,_enclMethod, 'null',_ForwName, [_this,_receiver,_value],_forwMethod)),
    updateForwardsFact(_set,setField,_forwCall,_forwMethod).

createForwBody(_call, _forwMethod, _forwBody, _ForwName, [_thisParam|[_recvParam|_forwParams]], Type, _execReturn) :- %neu
    (
    	(applyT(_call,_parent,_enclMethod,_expr,_origName,_args, _method_constr),
    	 method(_method_constr,_,_,_,Type,_,_));
	    (newClassT(_call,_parent,_enclMethod,_method_constr,_args,_,_,_),
	    _expr = null,
	    constructor(_method_constr,ConstrClass,_,_,_),
	    Type = type(class,ConstrClass,0),
	     _origName = 'init')
	),
    !,
    debugme,
    getOrigArgs(_call,_args,_origArgs),
    new_ids([_forwCall, _forwMethIdent]),
%    applyT(_call, _parent, _enclMethod, _origReceiver, _),
    forwardingMethodName(_call, 'call$',_origName, _ForwName),
    enclClass(_enclMethod,_enclClass), %neu
    methodT(_method_constr,DeclaringType,_,_,_,_,_),
    createForwMethParams(_enclClass,_forwMethod,DeclaringType,_expr,_origArgs,[_thisParam|[_recvParam|_forwParams]]),
    replaceId(_parent, _call, _forwCall),
    getOrigParams(_call,[_thisParam|[_recvParam|_forwParams]],_origParams),
    replaceForwCall(_call, _parent, _method_constr, _origName, _execReturn, _forwMethod,_origParams,_recvParam),
    createForwArgs(_call,_enclMethod,_enclClass,_forwCall,_expr,_args,_forwArgs,DeclaringType),
    set_parent(_forwArgs,_forwCall),
    add(applyT(_forwCall, _parent,_enclMethod, 'null', _ForwName, _forwArgs,_forwMethod)),
    updateForwardsFact(_call,methodCall,_forwCall,_forwMethod).

% execution
createForwMethodExecution(_method, _forwBody,_forwStmts) :- %neu
    methodT(_method, _class, _origName, _origParams, _type, _exceptions, _body),
    (interfaceT(_class)->
    	(fullQualifiedName(_class,FQN),sformat(Msg,'execution pointcut on interfaces not legal in LAJ: ~w',[FQN]),
    	 throw(Msg));true
    ),	
    new_ids([_forwCall, _forwMethIdent,_forwMethod]),

%    applyT(_method, _parent, _method, _origReceiver, _),
    forwardingMethodName(_method, 'exec$',_origName, _forwName),
    enclClass(_method,_class), %neu

% ersetzte alte Methode mit neuem Namen
    rec_set_encl_method(_body,_forwMethod),
    set_parent(_body,_forwMethod),
    
    cloneParams(_method, _origParams,_newParams),
    createThisInstanceParam(_class,_forwMethod,_thisVarDef),   
%    createTargetInstanceParam(_class, _forwMethod, 'null',_targetVarDef),
    createTargetInstanceParam(_class, _forwMethod, _targetVarDef),
    set_parent(_origParams,_forwMethod),
    rec_set_encl_method(_origParams,_forwMethod),

    delete(methodT(_method, _class, _origName, _origParams, _type, _exceptions, _body)),

    add(methodT(_method, _class, _origName, _newParams, _type, _exceptions, _forwBody)),
    add(methodT(_forwMethod, _class, _forwName, [_thisVarDef | [_targetVarDef |_origParams]], _type, _exceptions, _body)),

    cloneModifier(_method,_forwMethod),
    add_to_class(_class, _forwMethod),

    % add forwarding call
    ((_forwStmts == []) -> (
        new_ids([_execReturn,_thisIdent,_targetIdent]),

        reccreateVarDefIdents(_forwCall, _newParams,_forwArgs),
        rec_set_encl_method(_forwArgs,_method),
        add(applyT(_forwCall, _execReturn,_method, 'null', _forwName, [_thisIdent| [_targetIdent | _forwArgs]], _forwMethod)),
        create_this_or_null_if_static(_thisIdent,   _forwCall, _method,  _class),
        create_this_or_null_if_static(_targetIdent, _forwCall, _method,  _class),
        createReturnOrExec(_forwBody, _forwMethod, _type, _forwCall, _execReturn),
        _bodyStmts = [_execReturn]
    );
        _bodyStmts = _forwStmts
    ),
    add(blockT(_forwBody, _method,_method, _bodyStmts)),%neu
    updateForwardsFact(_method,execution,_method,_forwMethod).



createAroundBody(_call, _forwMethod, _forwBody, _ForwName,_forwParams, _Type) :-
    applyT(_call,_parent,_enclMethod,_expr,_origName,_args, _method),
    !,
    getOrigArgs(_call,_args,_origArgs),
    new_ids([_forwCall, _forwMethIdent]),
%    applyT(_call, _parent, _enclMethod, _origReceiver, _),
    method(_method, DeclaringType, _origName, _, _Type, _exc, _),
    forwardingMethodName(_call, 'call$',_origName, _ForwName),
    enclClass(_enclMethod,_enclClass),
    createForwMethParams(_enclClass,_forwMethod,DeclaringType,_expr,_origArgs,_forwParams),
    replaceId(_parent, _call, _forwCall),
    createForwArgs(_call,_enclMethod,_enclClass,_forwCall,_origReceiver,_args,_forwArgs,DeclaringType),
    set_parent(_forwArgs,_forwCall),
    add(applyT(_forwCall, _parent,_enclMethod, 'null', _ForwName, _forwArgs, _forwMethod)),
%    retractall(applyT(_call, _,_,_,__,_)), %new: remove old forwarding Appl
    updateForwardsFact(_call,methodCall,_forwCall,_forwMethod).%new: update

