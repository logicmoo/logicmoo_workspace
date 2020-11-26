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
action( add_proceed_call(_pc,_call,_parent, _enclMethod,_adviceArgs,_proceedArgs)):-add_proceed_call(_pc,_call, _parent, _enclMethod,_adviceArgs,_proceedArgs).
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


replaceForwCall(_call, _parent, _method, _origName, _execReturn,_forwMethod,_forwParams,_recvParam):-
    applyT(_call,_,_,_,_,_,_),
      createIdentRefParam(_recvParam,_callSelect, _forwReceiver),
      createVarDefIdents(_call, _forwParams, _argsInForw),
    action(replace(applyT(_call, _execReturn, _forwMethod, _forwReceiver,_origName, _argsInForw,_method))).

replaceForwCall(_call, _parent, _method, _origName, _execReturn,_forwMethod,_forwParams,_recvParam):-
    newClassT(_call,_,_,Constructor,_,TypeExpr,Def,Enclosing),
    createVarDefIdents(_call, _forwParams, _argsInForw),
    action(replace(newClassT(_call, _execReturn, _forwMethod, Constructor,_argsInForw,TypeExpr,Def,Enclosing))).			    


createForwArgs(_call,_,_,_,_,_Args,_Args,_):-
    forwards(_call,_,_,_),
    !.
createForwArgs(_call,_enclMethod,_enclClass,_forwCall,_origReceiver,_Args,[_This|[Receiver|_Args]],DeclaringType):-
    create_this_or_null_if_static(_This,_forwCall, _enclMethod, _enclClass),
    ( 
      (_origReceiver = null,
   %createThisOrGetReceiver(OldParent, NewParent, Encl, OldReceiver, NewReceiver, DeclaringType)     
      createThisOrGetReceiver(_parent, _forwCall, _enclMethod,_origReceiver,Receiver,DeclaringType)
      
%       create_this_or_null_if_static(Receiver,_forwCall, _enclMethod, _enclClass)
      );
      _origReceiver = Receiver
    ).
    %createThisOrGetReceiver(_origReceiver, _forwCall,_Receiver).


getOrigParams(_call,_Params,_Params):-
    forwards(_call,_,_,_),
    !.

getOrigParams(_,[_|[_|_ForwParams]],_ForwParams).


getOrigArgs(_call,_args,_OrigArgs) :-
    forwards(_call,_,_,_),
    !,
    _args = [_|[_|_OrigArgs]].

getOrigArgs(_call,_Args,_Args).
/*
 * updateForwardsFact(+Call,+Kind,+ForwCall,+ForwMethod)
 */
updateForwardsFact(_call,_,_forwCall,_forwMethod):-
    forwards(_call,_lastForwMethod,_kind,_pc),
    !,
    delete(forwards(_call,_lastForwMethod,_kind,_pc)),
    add(forwards(_forwCall, _forwMethod, _kind, _pc)),
    add(forwarding(_forwMethod, _lastForwMethod,_pc)).
%    add(forwarding(_forwCall)).

updateForwardsFact(_stmt,_kind, _forwCall,_forwMethod):-
    add(forwards(_forwCall, _forwMethod, _kind, _stmt)),
    add(forwarding(_forwMethod, _stmt,_stmt)).
%    add(forwarding(_forwCall)).

/*
 * createForwardingMethod(Stat,Class,ForwMethod,ForwBody)
 *
 * Creates a forwarding method 
 *
 */
createForwardingMethod(_method, _class,_,_forwBody) :-
    methodT(_method,_class,_,_,_,_,_),
    !,
    createForwMethodExecution(_method, _forwBody,[]).

createForwardingMethod(Stat,Class,ForwMethod,ForwBody) :-
    new_id(_execReturn),
    enclosing(Stat,EnclMethod),
    createForwBody(Stat, ForwMethod, ForwBody, ForwName, _params, Type, _execReturn),
    createReturnOrExec(ForwBody, ForwMethod, Type, Stat, _execReturn),
    add_to_class(Class, ForwMethod),
    add(methodT(ForwMethod, Class, ForwName, _params, Type, [], ForwBody)),
	share_static_modifier(EnclMethod,ForwMethod),
    add(modifierT(ForwMethod, 'private')),
    add(blockT(ForwBody, ForwMethod, ForwMethod, [_execReturn])).



createAroundMethod(Stat, Class,ForwStats,ForwMethod,ForwBody) :-
%    new_ids([ForwMethod, ForwBody]),
    enclosing(Stat,EnclMethod),
    createAroundBody(Stat, ForwMethod, ForwBody, ForwName, _params, Type),
%    createReturnOrExec(ForwBody, ForwMethod, Type, Stat, _execReturn),
    add_to_class(Class, ForwMethod),
    add(methodT(ForwMethod, Class, ForwName, _params, Type, [], ForwBody)),
	share_static_modifier(EnclMethod,ForwMethod),
    add(modifierT(ForwMethod, 'private')),
    add(blockT(ForwBody, ForwMethod, ForwMethod, ForwStats)).


share_static_modifier(EnclMethod,ForwMethod):-
    modifierT(EnclMethod,'static'),
    !,
    add(modifierT(ForwMethod, 'static')).
share_static_modifier(_EnclMethod,_ForwMethod):-
    true.

debugme(_id):-
    format('~ndebug: ~w',[_id]),
    flush_output.

forwardingMethodName(_stat, _prefix, _origName, _ForwName) :-
    new_id(_aNumber),
    int2string(_aNumber, _aString),
    stringAppend(_origName,'$',_aString,_forwName),
    appendForwPrefix(_stat,_prefix, _forwName,_ForwName).

appendForwPrefix(_stat,_, _forwName,_forwName):-
    forwards(_stat,_,_,_),
    !.

appendForwPrefix(_stat,_prefix, _forwName,_ForwName) :-
    stringAppend(_prefix, _forwName, _ForwName).
%    stringAppend('forward$', _prefix, _forwName, _ForwName).

    %    add(_Receiver, _apply, _encl, 'this', _enclClass).

/*
 * prependBlockStatments(+Block, +Statements)
 *
 * prepends the id list Statements to block Block.
 */

prependBlockStatments(_, []).

prependBlockStatments(_block, _stmts) :-
    blockT(_block, _parent, _encl, _oldStmts),
    append(_stmts,_oldStmts,_newStmts),
    delete(blockT(_block, _parent, _encl, _oldStmts)),
    add(blockT(_block, _parent, _encl, _newStmts)).

prependBlockStatment(_block, _pre) :-
    blockT(_block, _parent, _encl, _stats),
    prepend(_stats, _pre, _newStats),
%    rec_set_encl_method(_pre, _encl),
%    rec_set_parent(_pre, _block),
    delete(blockT(_block, _parent, _encl, _stats)),
    add(blockT(_block, _parent, _encl, _newStats)).


appendBlockStatment(_block, _post) :-
    blockT(_block, _parent, _encl, _stats),
    append(_stats, [_post], _newStats),
    rec_set_encl_method(_post, _encl),
    rec_set_parent(_post, _block),
    delete(blockT(_body, _p, _e, _stats)),
    add(blockT(_body, _p, _e, _newStats)).


/*
 * matchParams(ParametersOrArgs, PatternList)
 * 
 * matches a parameter/expression list with a pattern list or
 * with the types of another list of parameters/expressions.
 *
 * The PatternList elements of the following terms:
 *
 * id(FQN):     
 *            FQN is a full qualified name of a object or basic type.
 * list(PList):  
 *            PList is a list of parameters.
 *            If PList is not bound it will be bound to a list of parameters, 
 *            otherwise the types of the parameters list are compared, see tests for examples.
 *
 * TESTED?
 */
matchParams([], []):-!.

matchParams([], [id([Term])]) :-
	var(Term).	


matchParams([], [Term]) :-
	var(Term).

matchParams([VdHead | VdTail], [Term | PatTail]) :-
    nonvar(Term),
    Term = type(Type),
    getType_fq(VdHead,Type),
    matchParams(VdTail,  PatTail).



matchParams([VdHead | VdTail], [Term | PatTail]) :-
    nonvar(Term),
    Term = params([Head|Tail]),
    (
      var(Head) ->
        Head = VdHead;
        (getType(Head,Type),getType(VdHead,Type))
    ),
    matchParams(VdTail, [params(Tail) | PatTail]).

matchParams(VdHead, [Term | PatTail]) :-
    nonvar(Term),
    Term = params([]),
    matchParams(VdHead, PatTail).




matchParams([VdHead | VdTail], [List | PatTail]) :-
    nonvar(List),
    List = [_pattern , VdHead],
    !,
    matchParams([VdHead | VdTail], [_pattern | PatTail]).

matchParams([VdHead | VdTail], [TypePattern | PatTail]) :-
    nonvar(TypePattern),
    TypePattern = typePattern(_pattern, _dim),
    getType(VdHead, type(_kind, _id,_dim)),
    getTypeName(type(_kind, _id,_dim), _name),
    matchPatterns(_name, _pattern),
    matchParams(VdTail, PatTail).

matchParams([_VdHead | VdTail], [Term | PatTail]) :-
	var(Term),
    matchParams(VdTail,PatTail).
    
/*
 * matchLMVPattern(ParametersOrArgs, PatternList)
 * 
 * matches a PEF-list with a pattern list or
 * 
 * The PatternList elements of the following terms:
 *
 * id(FQN):     
 *            FQN is a full qualified name of a object or basic type.
 * list(PList):  
 *            PList is a list of parameters.
 *            If PList is not bound it will be bound to a list of parameters, 
 *            otherwise the types of the parameters list are compared, see tests for examples.
 *
 * TESTED
 */



matchLMVPattern([], []):-!.

matchLMVPattern(_Head, [Term | _Tail]) :-
    var(Term),
    throw('Elements of the right list must be id/1 or list terms/1.').

matchLMVPattern([VdHead | VdTail], [Term | PatTail]) :-
    Term = id(VdHead),
    matchLMVPattern(VdTail,  PatTail).

matchLMVPattern([Head | VdTail], [Term | PatTail]) :-
    Term = list([Head|Tail]),
    matchLMVPattern(VdTail, [list(Tail) | PatTail]).

matchLMVPattern(VdHead, [Term | PatTail]) :-
    Term = list([]),
    matchLMVPattern(VdHead, PatTail).

	
    
matchParamTypeNameList([],[]).
matchParamTypeNameList([Head | Tail],[FNHead|FNTail]):-
    paramT(Head,_,Type,_),
    getTypeName(Type,FNHead),
	matchParamTypeNameList(Tail,FNTail).
	
matchPatterns(_, []).
matchPatterns(_name, (_pat1;_pat2)) :-
    !,(
    matchPatterns(_name, _pat1);
    matchPatterns(_name, _pat2)
    ).

matchPatterns(_name, (_pat1,_pat2)) :-
    !,
    matchPatterns(_name, _pat1),
    matchPatterns(_name, _pat2).

matchPatterns(_name, _pat) :-
    pattern(_pat, _, _name).


weave(before, _pc,_exec):-
    before(_pc, _exec).

weave(after, _pc,_exec):-
    after(_pc, _exec).






callToAdviceMethod(_pc, _adviceMeth, _adviceArgs,_exec,_forwMethod,_forwBody) :-
    (replaceStatementWithForwarding(_pc,_forwMethod,_forwBody); true),
    methodT(_adviceMeth,Aspect,_adviceMethName, _,_,_,_),
    fieldT(_adviceInstanceVar,Aspect,_,'aspectInstance',_),
    classT(Aspect,_,AdviceName,_),
    new_ids([_exec, _apply, _selectAdviceMethod,_selectInstField, _ident]),
    enclMethod(_pc,_enclMethod),
    createIdentsReferencingAdviceParams(_pc, _enclMethod,_adviceArgs, _adviceCallArgs),
    add(execT(_exec, 0,0, _apply)),
    add(applyT(_apply, _exec,0, _selectInstField,_adviceMethName, _adviceCallArgs,_adviceMeth)),
    add(getFieldT(_selectInstField, _apply, 0, _ident, 'aspectInstance',_adviceInstanceVar)),
    add(identT(_ident, _selectInstField, 0, AdviceName, Aspect)).


createIdentsReferencingAdviceParams(_,_,[],[]).

createIdentsReferencingAdviceParams(_call,_enclMethod,[_adviceArg|_adviceArgs],[_Ident|_Idents]):-
    new_id(_Ident),
    getForwParam(_call, _adviceArg, _param,_name),
    add(identT(_Ident, _call, _enclMethod, _name, _param)),
    createIdentsReferencingAdviceParams(_call,_enclMethod,_adviceArgs,_Idents).
    
/*
 * boxing_class(+BasicType, ?BoxingClass)
 *
 * Binds BoxingClass to the corresponding
 * boxing class of the basic type BasicType.
 */

boxing_class(int, _class):-
    packageT(_pckg, 'java.lang'),
    classT(_class,_pckg,'Integer',_).
    
boxing_class(double, _class):-
    packageT(_pckg, 'java.lang'),
    classT(_class,_pckg,'Double',_).
boxing_class(float, _class):-
    packageT(_pckg, 'java.lang'),
    classT(_class,_pckg,'Float',_).
boxing_class(char, _class):-
    packageT(_pckg, 'java.lang'),
    classT(_class,_pckg,'Character',_).
boxing_class(byte, _class):-
    packageT(_pckg, 'java.lang'),
    classT(_class,_pckg,'Byte',_).
boxing_class(short, _class):-
    packageT(_pckg, 'java.lang'),
    classT(_class,_pckg,'Short',_).
boxing_class(long, _class):-
    packageT(_pckg, 'java.lang'),
    classT(_class,_pckg,'Long',_).
boxing_class(boolean, _class):-
    packageT(_pckg, 'java.lang'),
    classT(_class,_pckg,'Boolean',_).
boxing_class(Kind, _class):-
	sformat(Msg, 'ERROR: Could not find boxing class for ~w~n', [Kind]),
	throw(Msg).


add_proceed_call_idents(_,_,_,_,[],[],_,[]).
add_proceed_call_idents(_pc,_call,_enclMethod, _adviceArgs, [_param|_params], [_forwParam|_forwParams], _proceedArgs,[_proceedArg|_Args]):-
    getCompareElement(_pc,_param,_forwParam, _compare),
    findProceedArg(_pc,_compare,_adviceArgs, _proceedArgs,_proceedArg),
%    getForwParam(_pc, _adviceArg, _compare,_),
    !,
    add_proceed_call_idents(_pc,_call,_enclMethod, _adviceArgs, _params,_forwParams, _proceedArgs,_Args).

add_proceed_call_idents(_pc,_call,_enclMethod, _adviceArgs, [_param|_params], [_forwParam|_forwParams], _proceedArgs,[_Arg|_Args]):-
    new_id(_Arg),
    !,
    getCompareElement(_pc,_param,_forwParam, _ref),
    getRefIdentName(_ref,_name),
    add(identT(_Arg, _call, _enclMethod, _name, _ref)),
    add_proceed_call_idents(_pc,_call,_enclMethod, _adviceArgs, _params,_forwParams, _proceedArgs,_Args).


getRefIdentName(_ref,_name):-
    localT(_ref, _, _, _, _name, _).
getRefIdentName(_ref,_name):-
    paramT(_ref, _, _, _name).
getRefIdentName(_ref,'this'):-
    classT(_ref, _, _, _).


getCompareElement(_method,_param, _, _class):-
     method(_method, _class, _, _, _, _, _),
     forwards(_, _forwMethod, _, _method),
     method(_forwMethod, _, _, _params, _, _, _),
    (
        _params = [_param|_];
        _params = [_|[_param|_]]
    ),
    !.

getCompareElement(_method,_, _forwParam,_forwParam):-
    method(_method, _, _, _, _, _, _),
    !.

getCompareElement(_pc,_param,_, _param).


findProceedArg(_pc,_compare,[_adviceArg|_adviceArgs], [_proceedArg|_proceedArgs],_proceedArg) :-
    getForwParam(_pc, _adviceArg, _compare,_),
    !.

findProceedArg(_pc,_compare,[_|_adviceArgs], [_|_proceedArgs],_proceedArg) :-
    findProceedArg(_pc,_compare,_adviceArgs, _proceedArgs,_proceedArg).


createForwMethParams(_enclClass,_forwMethod,_DeclaringType,_origReceiver, _args,
                     [_InstanceVarDef|[_ReceiverVarDef|_Params]]):-
    validThisType(_enclClass,ValidEnclClass),
    createThisInstanceParam(ValidEnclClass, _forwMethod, _InstanceVarDef),
    (_origReceiver == null ->
	  outerOrEnclClass(_enclClass,ValidTargetClass);
	  getType(_origReceiver,type(class, ValidTargetClass,_))
	),
    createTargetInstanceParam(ValidTargetClass,_forwMethod, _ReceiverVarDef),
%    createTargetInstanceParam(_enclClass, _forwMethod, _origReceiver, _ReceiverVarDef),
    createForwParams(_forwMethod, _args, _Params).

validThisType(EnclClass,Type):-
    classT(EnclClass, Parent,_,_),
    newClassT(Parent,_,_,_,_,TypeExpr,_,_),
    getType(TypeExpr, type(class,Type,0)),
%    extendsT(EnclClass,Super),
%    enclClass(Parent,EnclOuterClass),
    !.
validThisType(EnclClass,EnclClass).

/*
 * outerOrEnclClass(+EnclClass,OuterClass)
 *
 * bind outer class if available, otherwise
 * the second arg is bound to the first arg
 */
 
outerOrEnclClass(EnclClass,OuterClass):-
    classT(EnclClass,NewClass,_,_),
    newClassT(NewClass,_,NewClassEncl,_,_,_,_,_),
    enclClass(NewClassEncl,OuterClass),
    !.
outerOrEnclClass(EnclClass,EnclClass).



createThisInstanceParam(_enclClass,_forwMethod,_InstanceVarDef):-
    new_id(_InstanceVarDef),
    add(java_fq(paramT(_InstanceVarDef, _forwMethod, _enclClass, '_this'))).


createTargetInstanceParam(DeclaringType, _forwMethod, _ReceiverVarDef):-
    new_id(_ReceiverVarDef),
    add(java_fq(paramT(_ReceiverVarDef,  _forwMethod, DeclaringType, '_target'))),
    !.

/*
createTargetInstanceParam(_enclClass, _forwMethod, 'null',_ReceiverVarDef):-
    new_id(_ReceiverVarDef),
    add(paramT(_ReceiverVarDef,  _forwMethod, type(class,_enclClass,0), '_target')),
    !.
createTargetInstanceParam(_enclClass, _forwMethod, _origReceiver,_ReceiverVarDef):-
    new_id(_ReceiverVarDef),
    getType(_origReceiver, _type),
    add(paramT(_ReceiverVarDef,  _forwMethod, _type, '_target')).
*/

createForwParams(_forwMethod, _args, _Params) :-
    createForwParams(_forwMethod, _args, _Params,0).

createForwParams(_, [],[],_).
createForwParams(_forwMethod, [_arg|_args], [_Param | _Params],_counter) :-
    createForwParam(_forwMethod,_arg, _Param,_counter),
    plus(_counter, 1, _next),
    createForwParams(_forwMethod, _args, _Params,_next).

createForwParam(_forwMethod, _arg, _Param,_counter) :-
    getType(_arg,type(basic,null,0)),
    !,
    fullQualifiedName(JLO, 'java.lang.Object'),
    new_id(_Param),
    append_num('x', _counter, _name),
    add(paramT(_Param, _forwMethod,type(class,JLO,0), _name)),
    !.

createForwParam(_forwMethod, _arg, _Param,_counter) :-
    getType(_arg,_type),
    new_id(_Param),
    append_num('x', _counter, _name),
    add(paramT(_Param, _forwMethod, _type, _name)),
    !.

% DEBUG commented

getForwParam(_method, _adviceArg, _IdentRef,_IdentName):-
    % Ausnahme fuer den execution pointcut
    methodT(_method, _class, _, _params, _, _, _),
    !,
    forwards(_, _forwMethod, _, _method),
    methodT(_forwMethod, _, _, [_|[_|_origParams]], _, _, _),
    findParamExecution(_adviceArg,_class,_params,_origParams,_IdentName,_IdentRef).


getForwParam(Pc, _adviceArg, _forwParam,_forwParamName):-
    forwards(_forwCall, _forwMethod, _, Pc),
    methodT(_forwMethod, _, _, [_thisParam|[_targetParam|_params]], _, _, _),
    applyT(_forwCall, _,_, _, _,[_|[_|_args]],_),
    getRealEncl(Pc,_,RealEncl),
    methodT(RealEncl,_,_,EnclParams,_,_,_),
    length(EnclParams,NumEnclParams),
    remove_tail(_args,NumEnclParams,ArgListWithoutEnclParams),
    concat_lists([ArgListWithoutEnclParams,EnclParams],SearchList),
    findParam(_adviceArg,_thisParam,_targetParam,_params,SearchList,_forwParam),
    paramT(_forwParam, _,_type, _forwParamName).

findParamExecution('_this',_class, _,_,'this',_class):- !.
findParamExecution('_target',_class,_,_,'this',_class):- !.
findParamExecution(_param,_class,_params,_origParams, _forwParamName,_Param):-
    _param \= '_this',
    _param \= '_target',
    findParam(_param,_,_,_params,_origParams,_Param),
    paramT(_Param, _, _type, _forwParamName).


findParam('_this',_ThisParam,_,_,_,_ThisParam):-
    !.
findParam('_target',_,_TargetParam,_,_,_TargetParam):-
    !.
findParam(_arg,_,_,[_Param|_params],[_arg1|_args],_Param):-
    _arg \= '_this',
    _arg \= '_target',
    _arg == _arg1,
    !.
findParam(_arg,_thisParam,_targetParam,[_param|_params],[_a|_args],_ForwParam):-
    !,
    findParam(_arg,_thisParam,_targetParam,_params,_args,_ForwParam).

/*
  pc_visible(?Encl,?Class, ?Package)

  Is true when a join point (pointcut) is visible
  */

pc_visible(_encl,_,_):-
    enclClass(_encl,_enclClass),
    modifierT(_enclClass,'aspect'),
    !,
    fail.

pc_visible(_encl,_,_):-
    not(visibility(_encl,_,_)),
    !.

pc_visible(_encl,_,_):-
    visibility(_encl,'hidden',_),
    !,
    fail.

pc_visible(_encl,_,_pckg):-
    visibility(_encl,'package',_pckg),
    !.
    
pc_visible(_encl,_currentAspectClass,_):-
    visibility(_encl,'protected',_aspectClass),
    !,
    subtype(_currentAspectClass,_aspectClass).

pc_visible(_encl,_aspectClass,_):-
    visibility(_encl,'private',_aspectClass),
    !.


set_visibility(_pc, _type, _ref):-
    forwards(_,_forwMethod,_,_pc),
    !,
    add(visibility(_forwMethod, _type, _ref)).

set_visibility(_var, _type, _ref):-
    add(visibility(_var, _type, _ref)).


%    current_aspect(_aspectClass,_).

%TODO: erzeuge Argument-Listen: TESTEN

create_ref_idents(_pc, _apply, _encl, [], []).

create_ref_idents(_pc, _apply, _encl, [_param_id|_rest], [_ident_id | _rest_ids]) :-
    paramT(_param_id,_,_, _name),
    !,
    add(identT(_ident_id, _apply, _encl, _name, _param_id)),
    create_ref_idents(_pc, _apply, _encl, _rest, _restids).

% leere Parameter oder Argumentliste
create_ref_idents(_pc, _apply, _encl, [[]|_rest], [[] | _rest_ids]) :-
    !,
    create_ref_idents(_pc, _apply, _encl, _rest, _restids).

create_ref_idents(_pc, _apply, _encl, [_fn |_rest], [GetField | _rest_ids]) :-
    not(tree(_fn,_,_)),
    encl_class(_encl,_encl_class),
    resolve_field(_fn,_encl_class,_field),
    add(getFieldT(GetField, _apply, _encl, 'null',_fn, _field)),
    create_ref_idents(_pc, _apply, _encl, _rest, _restids).

create_ref_idents(_pc, _apply, _encl, [_arg_id|_rest], [GetField | _rest_ids]) :-
    add_advice_param_ref(_pc, _arg_id,GetField,_apply, _encl),
    create_ref_idents(_pc, _apply, _encl, _rest, _restids).

create_ref_idents(_pc, _apply, _encl, [[_h |_t] | _rest], [ _param_ids | _rest_ids]) :-
    !,
    create_ref_idents(_pc, _apply, _encl, [_h|_t], _param_ids),
    create_ref_idents(_pc, _apply, _encl, _rest, _restids).



% leere Parameter oder Argumentliste
extract_types(_encl_class, [], []).
extract_types(_encl_class, [[]|_rest], [[] | _Rest]) :-
    !,
    create_ref_idents(_encl, _rest, _Rest).

extract_types(_encl, [_fn|_rest], [_Type | _Rest]) :-
    not(tree(_fn,_,_)),
    !,
    encl_class(_encl,_encl_class),
    resolve_field(_fn,_encl_class,_field),
    fieldT(_field,_,_Type, _,_),
    create_ref_idents(_encl, _rest, _Rest).

extract_types(_encl, [_arg|_rest], [_Type | _Rest]) :-
    get_type(_arg,_Type),
    create_ref_idents(_encl, _rest, _Rest).

extract_types(_encl, [[_h |_t] | _rest], _Rest) :-

    !,
    create_ref_idents(_encl, [_h | _t], _rest_1),
    create_ref_idents(_encl, _rest, _rest2),
    append(_rest_1,_rest_2, _Rest).
    

constructor(_constructor,_class,_params):-
    methodT(_constructor,_class,'<init>', _paramsConstructor,_,[],_),
    matchParams(_params, _paramsConstructor).


lookupForwParameter(Arg,_,[],[],_, _):-
    format('forwarding parameter lookup failed: ~w~n',[Arg]).
lookupForwParameter(_,FnArg,[FnArg|_],[Param|_],Param, Name):-
    paramT(Param,_,_,Name).
lookupForwParameter(ForwMethod,FnArg,[_|PcArgs],[_|ArgParams],Param, Name):-
    lookupForwParameter(ForwMethod,FnArg,PcArgs,ArgParams,Param, Name).
   
    
copy_method_body(Method,BodyToCopy,Body):-
    cloneTree(BodyToCopy, Method, Method, Body).


/*
 * bindIdIfNeeded(ID) 
 *
 * Binds ID with new_id/1 if ID is a variable.
 */
bindIdIfNeeded(ID) :-
    var(ID),
    !,
    new_id(ID).
bindIdIfNeeded(_ID).

/*
 * apply_aj_cts.
 * 
 * debugging predicate
 * applies all cts in the laj_ct_list 
 * fact in the given order.
 */
apply_aj_cts :-
    rollback,
    apply_ct(change_aspect_class_member_visibility),
    laj_ct_list(A),
    apply_ctlist(A)
    %apply_ct(resolve_no_call_invocations)
    .
    
getReceiverTypeOrEnclosingIfInAnonymousClass_fq(ID,RecieverType_fq):-
    java_fq(methodT(ID,RecieverType_fq,_,_,_,_,_)).


%--ma statements 
getReceiverTypeOrEnclosingIfInAnonymousClass_fq(ID,RecieverType_fq):-
    statement(ID),
    enclClass_fq(ID,RecieverType_fq).
   
    

getReceiverTypeOrEnclosingIfInAnonymousClass_fq(ID,RecieverType_fq):-
    getReceiver(ID, Rec),
    (
    Rec = 'null' ->
       getNonAnonymousEnclosingClass_fq(ID,RecieverType_fq);
       getType_fq(Rec,RecieverType_fq)
    )    
   	.
   	
% getReceiverTypeOrEnclosingIfInAnonymousClass_fq(ID,RecieverType_fq):-
%    applyT(ID, _parent, _encl, _Receiver, 'super', _args,_method),
%
%    (
%    Rec = 'null' ->
%       getNonAnonymousEnclosingClass_fq(ID,RecieverType_fq);
%       getType_fq(Rec,RecieverType_fq)
%    )    
%   	.
      
getNonAnonymousEnclosingClass_fq(Id,Class_fq):- 
    enclClass(Id,Encl),
	(
		anonymousClass(Encl) ->
		(
			classT(Encl,Parent,_,_),
			enclClass(Parent,ParentOfParent),
			getNonAnonymousEnclosingClass_fq(ParentOfParent,Class_fq)
		);
		fullQualifiedName(Encl,Class_fq)
	)
	.
	

/*
 * getReturnType(+ID, -TypeString)
 *
 * Returns the return type
 *
 */     
    
getReturnType(ID,Type):-
    getType_fq(ID,Type).

    %mappeltauer: why did i use this definition before?
	% enclClass(ID,Class),
	% fullQualifiedName(Class,Type).   
      
anonymousClass(ID):-
	classT(ID,Parent,_,_),     
  	newClassT(Parent,_,_,_,_,_,_,_).
  	



/*
 * local_vars_of_jp_rek(+[IDs],-[LocalVars])
 *
 * collects all local variables of a joinpoint, which could
 * be a whole block since LAJ2, in a list
 * see LAJ-87
 */          	
local_vars_of_jp(Jp,Vars):-
 methodT(Jp,_,_,Args,_,_,_),
 local_defs_of_jp([Jp],Blacklist),
 %local_vars_of_jp([Jp],LocalVars,Blacklist),
 local_vars_of_jp(Args,Vars,[]).
 %concat_lists([LocalVars,LocalVars2],Vars).

local_vars_of_jp(Jp,LocalVars):-
 local_defs_of_jp([Jp],Blacklist),
 local_vars_of_jp([Jp],LocalVars,Blacklist).


 
local_vars_of_jp([],[],Blacklist):-!.
local_vars_of_jp([ID|IDs],LocalVars,Blacklist) :-
    local_var(ID,Ref),
    not(memberchk(Ref,Blacklist)),
        
    tree_name(ID,Name),
    local_vars_of_jp(IDs,List,Blacklist),
    !,
    concat_lists([lvar(Ref,Name)|List],LocalVarsTmp),
    list_to_set_save(LocalVarsTmp,LocalVars)
    .    

local_vars_of_jp([ID|IDs],LocalVars,Blacklist) :-
    local_var(ID,Ref),
    memberchk(Ref,Blacklist),        
    local_vars_of_jp(IDs,LocalVarsTmp,Blacklist),
    !,
    list_to_set_save(LocalVarsTmp,LocalVars)
    .    
    
    
    
local_vars_of_jp([H|T],List,Blacklist) :-
    not( local_var(H,_)),
    sub_trees(H,Subtrees),
    concat_lists([T|Subtrees],VarList),
    local_vars_of_jp(VarList,List,Blacklist)
    .

local_defs_of_jp([],[]).
local_defs_of_jp([Id|Ids],List) :-
    localT(Id,_,_,_,_,_),
    local_defs_of_jp(Ids, NewList),    
    !,
    concat_lists([Id,NewList],List)
   
    .

local_defs_of_jp([H|T],List) :-
    sub_trees(H,Subtrees),
    concat_lists([T,Subtrees],VarList),
    local_defs_of_jp(VarList,List)
    .

lvar_ids([],[]).
lvar_ids([Lvar|Lvars],Ids) :-
    Lvar = lvar(Id,_),
    lvar_ids(Lvars,OtherIds),    
    concat_lists([Id,OtherIds],Ids)   
    .

%----------
% The following predecates sould be better placed into JTransformer or st.java
% since they are very general queries

    
local_var(ID,Ref):-
    identT(ID,_,_,_,Ref),
    localT(Ref,_,_,_,_,_).
  

local_var(ID,Ref):-
    identT(ID,_,_,_,Ref),
    paramT(Ref,_,_,_).
 

local_var(Ref,Ref):-    
    paramT(Ref,_,_,_).
 

   		
/*
 * tree_names(+[IDS],-[Names])
 *
 * returns the corresponding string values of the tree elements
 */
 
tree_names([],[]).
    
tree_names([Arg|Args],Names):-
    tree_name(Arg,Name),
    tree_names(Args,NewNames),
    concat_lists([Name,NewNames],Names).   

/*
 * tree_name(+ID,-Name)
 *
 * returns the corresponding string value of a tree element
 */
  
tree_name(Arg,Name):-
    paramT(Arg,_,_,Name).
      	
tree_name(ID,Name):-
    identT(ID,_,_,_,Ref),
    localT(Ref,_,_,_,Name,_).  

tree_name(ID,Name):-
    localT(ID,_,_,_,Name,_).  

tree_name(ID,Name):-
    identT(ID,_,_,_,Ref),
    paramT(Ref,_,_,Name).  	
  
tree_name(ID,_):-    
    treeSignature(ID,Signature),
    stringAppend('cannot assign a string to id: ',Signature,Message),
    throw(Message).
 


block_stmt(Id):-  blockT(Id,_,_,_). 
block_stmt(Id):-  forLoopT(Id,_,_,_,_,_,_).
block_stmt(Id):-  doLoopT(Id,_,_,_,_).
block_stmt(Id):-  whileLoopT(Id,_,_,_,_).
block_stmt(Id):-  ifT(Id,_,_,_,_,_).
block_stmt(Id):-  switchT(Id,_,_,_,_).
block_stmt(Id):-  tryT(Id,_,_,_,_,_).
block_stmt(Id):-  catchT(Id,_,_,_,_).
 
 
 

 
 
 
 
 
 
 
 
    
/*
 * advice(ID, Name, [Arg,...])
 */
 
/*
 * created_by_advice(AdviceId, ForwId)
 */ 

 /* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++not used anymore*/
 
/*
 * add_proceed_call(+JoinPoint,+Id,+Parent,+EnclMethod,+AdviceArgs,+ProceedArgs)
 *
 * ACTION
 *
 * inserts a proceed call with the id "Id" to the last forwarding method
 * created for "JoinPoint".
 *
 */
/*add_proceed_call(_method,_call, _parent, _enclMethod,_adviceArgs,_proceedArgs):-
    method(_method, _, _, _forwParams, _, _, _),
    !,
    forwards(_method,_methodToCall,_,_),
    method(_methodToCall, _, Name, [_this|[_target|_params]], _, _, _),
    add_proceed_call_idents(_method,_call,_enclMethod,_adviceArgs,  [_this|[_target|_params]],  [_this|[_target|_forwParams]],_proceedArgs,_args),
    add(applyT(_call, _parent,_enclMethod, _expr, Name,_args,_methodToCall)),
    add(forwarding(_call)).

add_proceed_call(_pc,_call, _parent, _enclMethod,_adviceArgs,_proceedArgs):-
    forwarding(_enclMethod,_methodToCall,_),
    method(_enclMethod, _, _, _params, _, _, _),
    method(_methodToCall, _, _name, _, _, _, _),
    add_proceed_call_idents(_pc,_call,_enclMethod,_adviceArgs, _params, _params,_proceedArgs,_args),
    add(applyT(_call, _parent,_enclMethod, 'null',_name, _args, _methodToCall)),
    add(forwarding(_call)).
    
    
    */
 
/*around(Method,AroundStmts,_,ForwBody) :-
% special case: execution pointcut
    methodT(Method,_,_,_,_,_,_),
    createForwMethodExecution(Method, ForwBody, AroundStmts).

around(_stat,_aroundStmts,_forwMethod,_forwBody) :-
    (
        (
            not(forwards(_,_,_,_stat)),
            replaceStatementWithForwarding(_stat)
        );
        true
    ),
    getRealStat(_stat,_realStat),
    enclClass(_realStat, _enclClass),
    createAroundMethod(_realStat, _enclClass, _aroundStmts,_forwMethod,_forwBody).
  */ 
  /*
 * around(Joinpoint,Statements,ForwardingMethod,ForwardingBody)
 *
 * If this is the first advice for Joinpoint,
 * the Joinpoint is moved to a forwarding method.
 *
 * The following is done in both cases:
 * A new forwarding method is created and the list Statements
 * is added to the body of the method.
 *
 * ForwardingMethod and ForwardingBody were bound 
 * in ct the condition part by the predicate bindForwMethod/3.
 */



%around(JP,Statements,ForwMethod, ForwBody) :-
%   createAdviceMethod(JP, Statements, ForwMethod, ForwBody),
%  add(aopT(JP,'around', ForwMethod)).
    
   
/*
 *  before(Joinpoint, Statements, ForwardingMethod, ForwardingBody)
 *
 * ACTION
 *
 * If this is the first advice for Joinpoint,
 * the Joinpoint is moved to a forwarding method.
 *
 * The following is done in both cases:
 * A new forwarding method to the last created forwarding 
 * method (lets call it "forw_last") is created.
 * Before the call to forw_last the 
 * the list "Statements" is inserted into the method body.
 *
 * ForwardingMethod and ForwardingBody were bound 
 * in the ct condition part by the predicate bindForwMethod/3.
 */
    
/*before(JP, Statements,ForwMethod,ForwBody) :-
    (replaceStatementWithForwarding(JP,ForwMethod,ForwBody);true),
    prependBlockStatments(ForwBody, Statements).
*/
%before(JP, Statements,ForwMethod,ForwBody) :-    
%  createAdviceMethod(JP, Statements,ForwMethod,ForwBody),
%  add(aopT(JP,'before',ForwMethod)).




/*
 *  after(Joinpoint, Statements, ForwardingMethod, ForwardingBody)
 *
 * ACTION
 *
 * Documentation see before/4, except the statements
 * are inserted after the call to forw_last.
 * Precisely: A try finally block is inserted around
 * the forw_last call and the "Statements" are inserted into the
 * finally block.
 */
/*
after(_stat, _insertList,_forwMethod,_finallyBlock) :-
    new_id(_forwBody),
    (replaceStatementWithForwarding(_stat,_forwMethod,_forwBody);true),
    addTryFinallyBlockStmts(_forwMethod, _finallyBlock, _insertList).
*/    
    
%after(JP,Statements,ForwMethod,ForwBody) :-
%  createAdviceMethod(JP, Statements,ForwMethod,ForwBody),
%  add(aopT(JP,'after',ForwMethod)).
  
