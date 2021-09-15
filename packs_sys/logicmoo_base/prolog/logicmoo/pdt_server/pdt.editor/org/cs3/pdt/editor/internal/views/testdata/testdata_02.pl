/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/


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
  


