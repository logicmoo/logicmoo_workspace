% ===================================================================
% File 'logicmoo_module_aiml_cxt_path.pl'
% Purpose: An Implementation in SWI-Prolog of AIML
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'logicmoo_module_aiml_cxt_path.pl' 1.0.0
% Revision:  $Revision: 1.7 $
% Revised At:   $Date: 2002/07/11 21:57:28 $
% ===================================================================

%:-module()
%:-include('logicmoo_utils_header.pl'). %<?
%:- style_check(-singleton).
%%:- style_check(-discontiguous).
/*
:- if((current_prolog_flag(version,MMmmPP),MMmmPP<70000)).
:- style_check(-atom).
:- style_check(-string).
:- endif.
*/

% ===============================================================================================
% get / set  Coversational Variables
% ===============================================================================================

getAliceMemOrSetDefault(CtxIn,ConvThread,SYM,Name,ValueO,OrDefault):-
   checkSym(SYM),
   getAliceMemOrSetDefault0(CtxIn,ConvThread,Name,Value,OrDefault),!,
   (Value=[]-> ValueO=OrDefault ; ValueO=Value).

%%getAliceMemOrSetDefault0(CtxIn,ConvThread,Name,Value,_OrDefault):- hotrace(current_value(CtxIn,ConvThread:Name,Value)),!.
getAliceMemOrSetDefault0(CtxIn,ConvThread,Name,Value,_OrDefault):-
   notrace(getIndexedValue(CtxIn,ConvThread,Name,[],Value)),!.
getAliceMemOrSetDefault0(CtxIn,ConvThread,Name,Value,OrDefault):-
   setAliceMem(CtxIn,ConvThread,Name,OrDefault),!,OrDefault=Value.

% ===============================================================================================
% get / set  Global Variables
% ===============================================================================================
:-dynamic(dict/3).
:-multifile(dict/3).

getAliceMemElse(Ctx,Dict,Name,ValueO):-getAliceMemComplete(Ctx,Dict,Name,ValueO),!.
getAliceMemElse(_Ctx,Dict,Name,[Dict,(s),unknown,Name]):-atrace.

getAliceMem(Ctx,Dict,DEFAULT,ValueOut):- compound(DEFAULT),DEFAULT=default(Name,Default),!, 
     (getAliceMemComplete(Ctx,Dict,Name,ValueO) -> xformOutput(ValueO, ValueOut)  ; xformOutput(Default, ValueOut)). 

getAliceMem(Ctx,IDict,NameI,ValueO):-
     dictNameDictNameC(Ctx,IDict,NameI,Dict,Name),!,
     getAliceMem(Ctx,Dict,Name,ValueO).
getAliceMem(Ctx,Dict,Name,ValueO):- var(ValueO), !, getAliceMemElse(Ctx,Dict,Name,ValueO),!.

getAliceMem(Ctx,Dict,Name,'OM'):- !, \+((getAliceMemComplete(Ctx,Dict,Name,ValueO),ValueO\=='OM')).
%%getAliceMem(Ctx,Dict,Name,ValueI):- %%unresultifyC(ValueI,ValueM),!,getAliceMem(Ctx,Dict,Name,ValueO),!,sameBinding(ValueI,ValueO).%%prolog_must(nonvar(ValueI)),!.
getAliceMem(Ctx,Dict,Name,ValueI):- getAliceMemComplete(Ctx,Dict,Name,ValueO),!,sameBinding(ValueI,ValueO).

getAliceMemComplete(Ctx,Dict,Name,ValueO):-getInheritedStoredValue(Ctx,Dict,Name,ValueO),!.

dictNameKey(Dict,Name,Key):-dictNameKey0(Dict,Name,Key).

dictNameKey0([Dict],Name,Key):-nonvar(Dict),!,dictNameKey0(Dict,Name,Key).
dictNameKey0(Dict,[Name],Key):-nonvar(Name),!,dictNameKey0(Dict,Name,Key).
dictNameKey0(Dict,Name,Name):-nonvar(Dict),neverDictName(Dict),!.
dictNameKey0(Dict,Name,Dict:Name):-nonvar(Dict),!.
dictNameKey0(_Dict,DictName,Key):- nonvar(DictName),DictName=Dict:Name,!,dictNameKey0(Dict,Name,Key).
dictNameKey0(_Dict,NameKey,NameKey).
dictNameKey0(Dict,Name,Key):-var(Dict),nonvar(Name),!,Key=Name.

neverDictName(Var):-var(Var),!.
neverDictName([_=_|_]).
neverDictName([]).
neverDictName(_=_).


getStoredValue(Ctx,Dict,Name,Value):-prolog_must(var(Value)),getContextStoredValue(Ctx,Dict,Name,Value).

% ===============================================================================================
% named context via inheritance
% ===============================================================================================
getInheritedStoredValueOrDefault(Ctx,Scope,Name,ValueOut,_Default):- getInheritedStoredValue(Ctx,Scope,Name,ValueO) , xformOutput(ValueO, ValueOut).
getInheritedStoredValueOrDefault(_Ctx,_Scope,_Name,ValueOut,Default):- xformOutput(Default, ValueOut). 

getInheritedStoredValue(Ctx,Scope,DEFAULT,ValueOut):- compound(DEFAULT),DEFAULT=default(Name,Default),!,getInheritedStoredValueOrDefault(Ctx,Scope,Name,ValueOut,Default).
getInheritedStoredValue(Ctx,IScope,NameI,Value):-dictNameDictNameC(Ctx,IScope,NameI,Scope,Name),!,getInheritedStoredValue(Ctx,Scope,Name,Value).
getInheritedStoredValue(Ctx,Scope,Name,Value):- getStoredValue(Ctx,Scope,Name,Value).
getInheritedStoredValue(Ctx,Scope,Name,Value):- inheritedDictsOrdered(Scope,InHerit),getStoredValue(Ctx,InHerit,Name,Value).


% ===============================================================================================
% inheritance control
% ===============================================================================================
addInherit( SYM0,SYMPREV0):-ifChanged(convert_dictname(_Ctx),[SYM0,SYMPREV0],[SYM,SYMPREV]),!,addInherit( SYM,SYMPREV).

addInherit(_SYM,SYMPREV):-autoInheritDict(SYMPREV),!.
addInherit( SYM,SYMPREV):-dict(SYM,inheritdict,SYMPREV),!.
addInherit( SYM,SYMPREV):- asserta_dict(SYM,inheritdict,SYMPREV).

% asserta_dict(catefallback, template, ['[]']):- !,trace.
asserta_dict(Ctx,N,V):-  asserta(dict(Ctx,N,V)).

ifChanged(Pred,List,ListO):-maplist_safe(Pred,List,ListMid),ListMid\=List,prolog_must(ListMid=ListO),!.

remInherit(_SYM,SYMPREV):-autoInheritDict(SYMPREV),!.
remInherit( SYM,SYMPREV):-retractall(dict(SYM,inheritdict,SYMPREV)),!.

inheritedDictsOrdered(Scope,InHerit):-inheritedFrom2(Scope,InHerit), \+(Scope=InHerit).

inheritedFrom2(Scope,InHerit):-inheritedFrom(Scope,InHerit).
inheritedFrom2(Scope,InHerit):-inheritedFrom(Scope,InHeritMid),inheritedFrom(InHeritMid,InHerit).

inheritedFrom([],_):-!,fail.
inheritedFrom([Scope],To):-!,inheritedFrom(Scope,To).
inheritedFrom([D|LIST],To):-!,member(Scope,[D|LIST]),inheritedFrom(Scope,To).
%inheritedFrom(Compound,_):-compound(Compound),!,fail.
inheritedFrom(Scope,Dict):-dict(Scope,inheritdict,Dict), \+(autoInheritDict(Dict)).
inheritedFrom(Auto,_):-autoInheritDict(Auto),!,fail.
inheritedFrom(Atom,_):- \+(atom(Atom)),!,fail.
inheritedFrom(Scope,defaultValue(Scope)).
inheritedFrom(_Scope,Dict):-autoInheritDict(Dict).

autoInheritDict(user).
autoInheritDict(default).

% ===============================================================================================
% getIndexedValue
% ===============================================================================================

getIndexedValue(Ctx,IDict,Name,MajorMinor,Value):-unresultifyC(IDict,Dict),!,
    getIndexedValue(Ctx,Dict,Name,MajorMinor,Value),!.

getIndexedValue(Ctx,Dict,Name,[],Value):-!,
    getIndexedValue(Ctx,Dict,Name,[1],Value).

getIndexedValue(Ctx,Dict,Name,Major,Value):-atomic(Major),!,
    getIndexedValue(Ctx,Dict,Name,[Major],Value).

getIndexedValue(Ctx,Dict,Name,[Minor],Value):-atomic(Minor),!,
    getIndexedValue(Ctx,Dict,Name,[1,Minor],Value).

getIndexedValue(Ctx,Dict,Name,[Major,'\b',SEP,'\b'|Minor],Value):-!,
    getIndexedValue(Ctx,Dict,Name,[Major,SEP|Minor],Value).

getIndexedValue(Ctx,Dict,Name,[Major,SEP|Minor],Value):- member(SEP,[',',':']),!,
    getIndexedValue(Ctx,Dict,Name,[Major|Minor],Value).

getIndexedValue(Ctx,Dict,Name,MajorMinor,ValueO):- numberFyList(MajorMinor,MajorMinorM),MajorMinor\==MajorMinorM,!,
   getIndexedValue(Ctx,Dict,Name,MajorMinorM,ValueO).

getIndexedValue(Ctx,Dict,DEFAULT,MajorMinor,ValueOut):- compound(DEFAULT),DEFAULT=default(Name,Default),!,
    (getIndexedValue(Ctx,Dict,Name,MajorMinor,ValueO)  -> xformOutput(ValueO, ValueOut)  ; xformOutput(Default, ValueOut)). 

getIndexedValue(Ctx,Dict,Name,MajorMinor,ValueO):-
    hotrace(getIndexedValue0(Ctx,Dict,Name,MajorMinor,Value)),
    xformOutput(Value,ValueO).
   
getIndexedValue(Ctx,Dict,Name,MajorMinor,ValueO):- fail,   
    unify_listing(getContextStoredValue(Ctx,Dict,_N,_V)),
    %%unify_listing(getContextStoredValue(Ctx,_,Name,_)),
    getIndexedValue0(Ctx,Dict,Name,MajorMinor,Value),
    xformOutput(Value,ValueO).


% ===============================================================================================
% getMajorIndexedValue
% ===============================================================================================
getMajorIndexedValue(Ctx,Dict,Name,Major,ValueS):- isLinearMemStore,!,
   getMajorIndexedValueLinear(Ctx,Dict,Name,Major,ValueS),!.

getMajorIndexedValue(Ctx,Dict,Name,Major,ValueS):-
   indexOntoKey(Name,Major,Item),
   getInheritedStoredValue(Ctx,Dict,Item,ValueS),!.



% ===============================================================================================
% getMajorIndexedValueLinear
% ===============================================================================================
getMajorIndexedValueLinear(Ctx,[D|List],Name,Major,ValueS):-
   member(Dict,[D|List]),
   notrace(getMajorIndexedValueLinear0(Ctx,Dict,Name,Major,ValueS)),!.

getMajorIndexedValueLinear(Ctx,[D|List],Name,Major,ValueS):-!,fail,
   unify_listing(dict(_,Name,_)),
   member(Dict,[D|List]),
   atrace,
   getMajorIndexedValueLinear0(Ctx,Dict,Name,Major,ValueS),!.

getMajorIndexedValueLinear(Ctx,Dict,Name,Major,ValueS):-
   getMajorIndexedValueLinear(Ctx,[Dict],Name,Major,ValueS),!.


getMajorIndexedValueLinear0(Ctx,Dict,Name,Major,ValueS):-
   subscriptZeroOrOne(Major),!,
   getInheritedStoredValue(Ctx,Dict,Name,ValueS),!.

getMajorIndexedValueLinear0(Ctx,Dict,Name,Major,ValueS):-
   nthResult(Major,getInheritedStoredValue(Ctx,Dict,Name,ValueS)),!.

getMajorIndexedValueLinear0(_Ctx,Dict,Name,Major,ValueS):-
   nthResult(Major,dict(Dict,Name,ValueS)),!,atrace.

nthResult(N,Call):-flag(nthResult,_,N),nthResult0(Call,Rs),!,Rs=1.
nthResult0(Call,N):-Call,flag(nthResult,N,N-1),N=1.
nthResult0(_,0):-flag(nthResult,_,0),!.

% ===============================================================================================
% getIndexedValue0
% ===============================================================================================
getIndexedValue0(Ctx,Dict,Name,[Major|Minor],Value):-
   getMajorMinorIndexedValue(Ctx,Dict,Name,Major,Minor,Value),!.

getMajorMinorIndexedValue(Ctx,Dict,Name,Major,Minor,Value):-
   getMajorIndexedValue(Ctx,Dict,Name,Major,ValueS),!,
   getMajorMinorIndexedValue0(Ctx,Dict,Name,Major,Minor,ValueS,Value).

getMajorMinorIndexedValue0(_Ctx,_Dict,_Name,_Major,Minor,ValueS,Value):-
   getMinorSubscript(ValueS,Minor,Value),!.

getMajorMinorIndexedValue0(Ctx,Dict,Name,Major,[M|Minor],ValueS,Value):-
   prolog_must(is_list(ValueS)),
   length(ValueS,ValueSLen),
   MajorN is Major + 1,
   N is M - ValueSLen,
   getMajorMinorIndexedValue(Ctx,Dict,Name,MajorN,[N|Minor],Value),!.


numberFyList([],[]).
numberFyList([A|MajorMinor],[B|MajorMinorM]):-
  atom(A),atom_to_number(A,B),
  numberFyList(MajorMinor,MajorMinorM),!.
numberFyList([A|MajorMinor],[A|MajorMinorM]):-numberFyList(MajorMinor,MajorMinorM).

isStarValue(Value):-ground(Value), \+([_,_|_]=Value),member(Value,[[ValueM],ValueM]),!,member(ValueM,['*','_']),!.
isEmptyValue([]):-atrace.

xformOutput(Value,ValueO):-isStarValue(Value),!,atrace,Value=ValueO.
xformOutput(Value,ValueO):-listify(Value,ValueL),Value\==ValueL,!,xformOutput(ValueL,ValueO).
xformOutput(Value,Value).

subscriptZeroOrOne(Major):-nonvar(Major),member(Major,[0,1,'0','1']).



%% getMinorSubscript(Items,Minor,Value).
getMinorSubscript(ItemsO,Index,Value):-  \+(is_list(ItemsO)),answerOutput(ItemsO,Items),prolog_must(is_list(Items)),getMinorSubscript(Items,Index,Value),!.
getMinorSubscript(Items,'*',Value):- !,prolog_must(flatten(Items,Value)),!.
getMinorSubscript(Items,',',Value):- throw_safe(getMinorSubscript(Items,',',Value)), !,prolog_must(=(Items,Value)),!.
getMinorSubscript(Items,[A|B],Value):-!,getMinorSubscript(Items,A,ValueS),!,getMinorSubscript(ValueS,B,Value),!.
getMinorSubscript(Items,[],Value):-!,xformOutput(Items,Value),!.
getMinorSubscript(Items,ANum,Value):- \+ number(ANum),!,prolog_must(atom_to_number(ANum,Num)),!,getMinorSubscript(Items,Num,Value).
%%%
getMinorSubscript(Items,Num,Value):- prolog_must(is_list(Items)),length(Items,Len),Index is Len-Num,nth0(Index,Items,Value),is_list(Value),!.
getMinorSubscript([],1,[]):-!.
getMinorSubscript(Items,1,Value):- last(Items,Last), (is_list(Last)->Value=Last;Value=Items),!.
getMinorSubscript(Items,1,Value):- xformOutput(Items,Value),!,atrace.
getMinorSubscript(Items,Num,Value):-debugFmt(getMinorSubscriptFailed(Items,Num,Value)),fail.

getUserDicts(User,Name,Value):-isPersonaUser(User),isPersonaPred(Name),once(getInheritedStoredValue(_Ctx,User,Name,Value)).

isPersonaUser(User):-findall(User0,getContextStoredValue(_Ctx,User0,'is_type','agent'),Users),sort(Users,UsersS),!,member(User,UsersS).
isPersonaPred(Name):-findall(Pred,(getContextStoredValue(_Ctx,_Dict,Pred,_Value),atom(Pred)),Preds),sort(Preds,PredsS),!,member(Name,PredsS).



% ===============================================================================================
% substs dictionaries
% ===============================================================================================
addReplacement(Ctx,IDict,Find,Replace):-dictNameDictNameC(Ctx,IDict,before,Dict,before),!,addReplacement(Ctx,Dict,Find,Replace).
addReplacement(Ctx,SubstsNameI,Find,Replace):-
      convert_dictname(Ctx,SubstsNameI,SubstsName),SubstsNameI \== SubstsName,
      addReplacement(Ctx,SubstsName,Find,Replace).
addReplacement(Ctx,SubstsName,Find,Replace):-
      convert_substs(Find,FindM),
      convert_replacement(Ctx,Replace,ReplaceM),
      (Replace\==ReplaceM;Find\==FindM),!,
      addReplacement(Ctx,SubstsName,FindM,ReplaceM).
addReplacement(Ctx,Dict,Find,Replace):- immediateCall(Ctx,addReplacement(Dict,Find,Replace)),fail.
addReplacement(_Ctx,Dict,Find,Replace):- assertz(dict(substitutions(Dict),Find,Replace)),!.

addReplacement(Dict,Find,Replace):-currentContext(addReplacement(Dict,Find,Replace),Ctx), addReplacement(Ctx,Dict,Find,Replace).


% ===============================================================================================
% context/name cleanups
% ===============================================================================================
dictNameDictNameC(Ctx,IDict,NameI,Dict,Name):-dictNameDictName(Ctx,IDict,NameI,Dict,Name),!, IDict+NameI \==Dict+Name, nop(debugFmt(IDict+NameI is Dict+Name)).

dictNameDictName(Ctx,IDict,NameI,Dict,Name):- traceIf(IDict=[_,_,_]),hotrace(dictNameDictName0(Ctx,IDict,NameI,Dict,Name)).
dictNameDictName0(Ctx,_Dict,D:NameI,Dict,Name):- nonvar(D),!,dictNameDictName(Ctx,D,NameI,Dict,Name).
dictNameDictName0(Ctx,IDict,NameI,Dict,Name):- convert_dictname(Ctx,IDict,Dict),unresultifyL(Ctx,NameI,Name).

unresultifyL(Ctx,NameI,Name):-unresultifyLL(Ctx,NameI,NameU),toLowerIfAtom(NameU,Name),!.

unresultifyLL(Ctx,NameI,NameO):-unresultify(NameI,Name),NameI \== Name,!,unresultifyLL(Ctx,Name,NameO).
unresultifyLL(Ctx,NameI,NameO):-is_list(NameI),lastMember(Name,NameI),!,unresultifyLL(Ctx,Name,NameO).
unresultifyLL(_Ctx,Name,Name).
toLowerIfAtom(Dict,Down):-atom(Dict),downcase_atom(Dict,Down),!.
toLowerIfAtom(Dict,Dict).



ensureValue(ValueO,ValueO):-!. %%TODO: remove this line
ensureValue(ValueO,['$value'(ValueO)]).

% ===============================================================================================
% Add/Setting globals
% ===============================================================================================
withValueAdd(Ctx,Pred,IDict,NameI,Value):- dictNameDictNameC(Ctx,IDict,NameI,Dict,Name),!,withValueAdd(Ctx,Pred,Dict,Name,Value),!.
withValueAdd(Ctx,Pred,IDict,Name,Value):-is_list(IDict),!,trace,foreach(member(Dict,IDict),withValueAdd(Ctx,Pred,Dict,Name,Value)),!.
withValueAdd(Ctx,_Pred:Print,Dict,Name,Var):- neverActuallyAdd(Ctx,Print,Dict,Name,Var),!.

withValueAdd(Ctx,Pred,Dict,Name,Var):-var(Var),!,withValueAdd(Ctx,Pred,Dict,Name,['$var'(Var)]).
withValueAdd(Ctx,Pred,Dict,Name,Atomic):-atomic(Atomic),Atomic\==[],!,withValueAdd(Ctx,Pred,Dict,Name,[Atomic]).

withValueAdd(_Ctx,_Pred:_Print,Dict,Name,Value):-uselessNameValue(Dict,Name,Value),!.
withValueAdd(Ctx,_Pred:Print,Dict,Name,Value):-immediateCall(Ctx,call(Print,Ctx,Dict,Name,Value)),fail.

withValueAdd(Ctx,Pred,Dict,Name,Value):-isStarValue(Value),!,nop(debugFmt(withValueAdd(Ctx,Pred,Dict,Name,Value))),traceIf(nonStarDict(Dict)).
%%withValueAdd(Ctx,Pred,Dict,default(Name),DefaultValue):-getAliceMem(Ctx,Pred,Dict,Name,'OM')->setAliceMem(Ctx,Dict,Name,DefaultValue);true.
withValueAdd(Ctx,Pred,Dict,Name,NonList):-( \+(is_list(NonList))),!,withValueAdd(Ctx,Pred,Dict,Name,[NonList]).
withValueAdd(Ctx,Pred:_Print,Dict,Name,Value):-checkDictIn(Value,ValueO),call(Pred,Ctx,Dict,Name,ValueO).

nonStarDict(catefallback):-!,fail.
neverActuallyAdd(Ctx,Pred,Dict,Name,Var):-var(Var),debugFmt(neverActuallyAdd(Ctx,Pred,Dict,Name,Var)),!.
neverActuallyAdd(Ctx,Pred,Dict,topic,[TooGeneral]):-member(TooGeneral,[general]),debugFmt(neverActuallyAdd(Ctx,Pred,Dict,topic,TooGeneral)),!.
neverActuallyAdd(Ctx,Pred,Dict,Name,Var):- \+(ground(var(Var))),debugFmt(maybeNeverActuallyAdd(Ctx,Pred,Dict,Name,Var)),!.


uselessNameValue(_Dict,srcfile,_):-!.
uselessNameValue(_Dict,srcinfo,[nosrc]):-!.



% ===============================================================================================
% Setting globals
% ===============================================================================================
setAliceMem(Dict,X,E):-currentContext(setAliceMem(Dict,X,E),Ctx), prolog_must(setAliceMem(Ctx,Dict,X,E)).

% OLD VERSION OF NEXT STATMENT 
setAliceMem(Ctx,Dict,Name,Value):-withValueAdd(Ctx,setAliceMem0:setAliceMem,Dict,Name,Value),!.
setAliceMem(Ctx,Dict,default(Name),DefaultValue):- (getAliceMem(Ctx,Dict,Name,'OM')->setAliceMem(Ctx,Dict,Name,DefaultValue);true),!.
setAliceMem(Ctx,Dict,Name,Value):- prolog_must(setAliceMem_fallback(Ctx,Dict,Name,Value)),!.

setAliceMem_fallback(Ctx,Dict,Name,Value):- setAliceMem0(Ctx,Dict,Name,Value),!.
setAliceMem_fallback([[fctx]],Dict,Name,Value):- asserta_dict(Dict,Name,Value),!.

setAliceMem0(Ctx,Dict,Name,Value):- prolog_must((resetAliceMem0(Ctx,Dict,Name,Value))),!.

% ===============================================================================================
% Inserting globals
% ===============================================================================================
insert1StValue(Ctx,IDict,Name,Value):-withValueAdd(Ctx,insert1StValue0:insert1StValue,IDict,Name,Value).
insert1StValue0(_Ctx,Dict,Name,Value):- asserta_dict(Dict,Name,Value),!.

% ===============================================================================================
%    AIML Runtime Database
% ===============================================================================================
%%checkDictValue(_Value):-!.
checkDictValue(Value):-prolog_must(nonvar(Value)),atomic(Value),Value==[],!.
checkDictValue(Value):-prolog_must(dictValue(Value)),!.

checkDictIn(Value,Value):-var(Value),!.
checkDictIn(Value,Value):-prolog_must(ground(Value)),(Value=['ERROR'|_];Value=[['ERROR'|_]|_]),!.

checkDictIn(Value,Value):-warnIf( \+(checkDictValue(Value))).

dictValue(V):-ground(V),dictValue0k(V),!.
dictValue(Value):-valuePresent(Value).
dictValue0k(['ERROR',understanding|_]):-!.
dictValue0k([X|_]):-dictValue0k(X).


resetAliceMem0(Ctx,IDict,NameI,ValueIn):- dictNameDictName(Ctx,IDict,NameI,Dict,Name),
   % for printing
   checkDictIn(ValueIn,Value),
   %%%traceIf(Dict==filelevel),
   currentContextValue(Ctx,Dict,Name,B),   
   debugFmt('/* ~q. */',[dict(Dict,Name,B->Value)]),
   % for cleaning
   clearContextValues(Ctx,Dict,Name),
   % for setting
   addNewContextValue(Ctx,Dict,Name,Value),!.

%%getContextStoredValue(Ctx,Dict,Name,Value):-dictNameKey(Dict,Name,Key),debugOnError(current_value(Ctx,Key,Value)),dictValue(Value).
currentContextValue(Ctx,Scope,Name,Value):- dictNameKey(Scope,Name,Key),getCtxValueND(Ctx,Key,Value).
currentContextValue(Ctx,Dict,Name,Value):- debugOnError((getContextStoredValue(Ctx,Dict,Name,Value))),!.
currentContextValue(_Ctx,_Dict,_Name,OMValue):- omOrNil(OMValue).

omOrNil([]):-!.
omOrNil('OM').
omOrNil(['Nothing']).


expire1Cache:-dict(N,_,_),number(N),retractall(dict(N,_,_)),fail.
expire1Cache:-dict(_,_,E),atom(E),atom_concat(evalsrai,_,E),retractall(dict(_,_,E)),fail.
expire1Cache:-dict(E,_,_),atom(E),atom_concat(evalsrai,_,E),retractall(dict(E,_,_)),fail.

getContextStoredValue(Ctx,IDict,NameI,Value):-dictNameDictNameC(Ctx,IDict,NameI,Dict,Name),!,getContextStoredValue(Ctx,Dict,Name,Value).
getContextStoredValue(_Ctx,Dict,Name,ValueO):- copy_term(ValueO,ValueI),dict(Dict,Name,ValueI),
   checkDictValue(ValueI),
   ValueI=ValueO.
   %%prolog_must(unwrapValue(ValueI,ValueO)).

removeContextValue(Ctx,IDict,NameI,Value):-dictNameDictName(Ctx,IDict,NameI,Dict,Name),checkDictValue(Value),copy_term(Value,Kill),ignore(retract(dict(Dict,Name,Kill))).
clearContextValues(Ctx,IDict,NameI):-dictNameDictName(Ctx,IDict,NameI,Dict,Name),retractall(dict(Dict,Name,_Value)).

addNewContextValue(Ctx,IDict,NameI,Value):-dictNameDictNameC(Ctx,IDict,NameI,Dict,Name),!,addNewContextValue(Ctx,Dict,Name,Value).
addNewContextValue(Ctx,Dict,Name,OM):- OM=='OM',!,clearContextValues(Ctx,Dict,Name),!.
addNewContextValue(Ctx,Dict,Name,Value):- 
   prolog_must((dictNameKey(Dict,Name,Key), addNewContextValue(Ctx,Dict,Key,Name,Value))),!.

addNewContextValue(Ctx,Dict,Key,Name,ValueIn):- 
   checkDictIn(ValueIn,Value),
   ifThen(nonvar(Key),addCtxValue(Ctx,Key,Value)),   
   ifThen(nonvar(Dict),ifThen(nonvar(Value),asserta_dict(Dict,Name,Value))),
   ifThen( \+(ground(Value)),debugFmt(addCtxValue(Ctx,Key,Value))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% pushInto1DAnd2DArray(Ctx,Tall,Wide,Ten,MultiSent,ConvThread)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
isLinearMemStore:-true.

pushInto1DAnd2DArray(Ctx,Tall,Wide,_Ten,MultiSent,ConvThread):- isLinearMemStore,!,
   %%atrace,
   splitSentences(MultiSent,Elements),
   maplist_safe(insert1StValue(Ctx,ConvThread,Tall),Elements),!,
   insert1StValue(Ctx,ConvThread,Wide,Elements),!.

pushInto1DAnd2DArray(Ctx,Tall,Wide,Ten,MultiSent,ConvThread):-
   %%atrace,
   splitSentences(MultiSent,Elements),
   previousVars(Tall,TallPrevVars,Ten),
   maplist_safe(setEachSentenceThat(Ctx,ConvThread,Tall,TallPrevVars),Elements),!,
   
   previousVars(Wide,WidePrevVars,Ten),
   setEachSentenceThat(Ctx,ConvThread,Wide,WidePrevVars,Elements),
   !.

setEachSentenceThat(Ctx,User,VarName,Vars,SR0):- cleanSentence(SR0,SR3),setEachSentenceThat0(Ctx,User,VarName,Vars,SR3),!.

setEachSentenceThat0(_Ctx,_User,_VarName,_Vars,[]):-!.
setEachSentenceThat0(Ctx,User,_VarName,[Var],SR0):- 
   setAliceMem(Ctx,User,Var,SR0),!.
setEachSentenceThat0(Ctx,User,VarName,[PrevVar,Var|MORE],SR0):-
   getAliceMem(Ctx,User,default(Var,'Nothing'),Prev),
   setAliceMem(Ctx,User,PrevVar,Prev),
   setEachSentenceThat0(Ctx,User,VarName,[Var|MORE],SR0).


previousVars(That,[That],0):-!.
previousVars(That,[That],1):-!.
previousVars(That,[Item|Prevs],N):-indexOntoKey(That,N,Item), NN is N-1,previousVars(That,Prevs,NN).

indexOntoKey(That,N,That):-subscriptZeroOrOne(N),!.
indexOntoKey(That,N,Item):-prolog_must(atomic(That)),atomic_list_concat_aiml([That,'(',N,')'],Item).

