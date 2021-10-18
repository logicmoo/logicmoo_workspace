% ===================================================================
% File 'logicmoo_util_ctx_frame.pl'
% Purpose: An Implementation in SWI-Prolog of Unwindable context frames
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'logicmoo_util_ctx_frame.pl' 1.0.0
% Revision:  $Revision: 1.1 $
% Revised At:   $Date: 2002/07/11 21:57:28 $
% ===================================================================
% ===================================================================
%  LocalContexts
%   They hold name-values in
%     -- assoc/1 lists
%     -- open tailed lists
%     -- frame/1 contains one or more of the above

%% v/3s 
%  = v(Value,Setter,KeyDestructor)

%% frame/3s
%  = frame(Named,Destructor,Ctx)

% well i played with a couple few differnt environment impls.. they have their pros cons.. one impl.. 
% that was unique is that an array of "binding pairs" live in an arraylist.. to be "in" an environment 
% it meant that you held an "index" into the arry list that as you went backwards you'd find your bindings.. each symbol had a java int field "lastBindingIndex" 
% .. that was a "hint" to where you could fastforward the backwards search .. end named binding context also had a "index" to when you leave a named block.. 
% you could quickly reset the top of an index.

:-module(ctx_frame,[ctxHideIfNeeded/3,
         lastMember/2,
         lastMember/3,
         pushCtxFrame/3,
         getCtxValue/3,
         makeLocalContext/2,
         appendAttributes/4,
         currentContext/2]).

:-ensure_loaded('../logicmoo/logicmoo_util_library.pl').
:-use_module(library('logicmoo/logicmoo_util_library.pl')).
:-ensure_loaded(library('logicmoo/logicmoo_util_bugger.pl')).

:-ensure_loaded('../logicmoo/logicmoo_util_library.pl').
:-use_module(library('logicmoo/logicmoo_util_library.pl')).
:-use_module(library('logicmoo/logicmoo_util_ctx_frame.pl')).



currentContext(Name,X):-hotrace(makeLocalContext(Name,X)),!.

% ===================================================================
:-dynamic(no_cyclic_terms/0).

no_cyclic_terms.

makeLocalContext(Name,Ctx):-makeLocalContext1(Name,Ctx),!,setCtxValue(ctx,Ctx,Name),!.

makeLocalContext1(Gensym_Key, [frame(Gensym_Key,no_destructor,[assoc(AL)|_])|_]):-    
   list_to_assoc([
    a-v(is_a,set_assoc,no_destructor(a)),
    a-v(is_a2,set_assoc,no_destructor(a)),
    b-v(is_b,set_assoc,no_destructor(b))],AL).


unwrapValue(HValue,TValue):-TValue==deleted,!,not(unwrapValue1(HValue,_)),!.
unwrapValue(HValue,TValue):-unwrapValue1(HValue,Value),!,TValue=Value.

unwrapValue1(v(ValueHolder,_SetterFun,_KeyDestroyer),Value):-!,unwrapValue1(ValueHolder,Value).
unwrapValue1(deleted,_):-!,fail.
unwrapValue1(Value,Value):-!.

bestSetterFn(v(_,Setter,_),_OuterSetter,Setter):-!.
bestSetterFn(_Value,OuterSetter,OuterSetter).

getCtxValue(Name,Ctx,Value):-checkCtx(Ctx), hotrace(( get_ctx_holder(Ctx,Holder),get_o_value(Name,Holder,HValue,_Setter),!, unwrapValue(HValue,Value))),!.
getCtxValue(Name,CtxI,Value):-checkCtx(CtxI),lastMember(Ctx,CtxI),hotrace(( get_ctx_holder(Ctx,Holder),get_o_value(Name,Holder,HValue,_Setter),!, unwrapValue(HValue,Value))),!.

setCtxValue(Name,Ctx,Value):-checkCtx(Ctx),get_ctx_holder(Ctx,Holder),get_o_value(Name,Holder,HValue,Setter),unwrapValue(HValue,CurrentValue),!,(CurrentValue=Value;call(Setter,Value)),!.
setCtxValue(Name,Ctx,Value):-checkCtx(Ctx),addCtxValue1(Name,Ctx,Value),!.

addCtxValue(Name,Ctx,Value):-checkCtx(Ctx),addCtxValue1(Name,Ctx,Value),!.
addCtxValue1(Name,Ctx,Value):-get_ctx_holderFreeSpot(Ctx,Name=v(Value,Setter,Destructor),Destructor),!,ignore(Setter=no_setter(Name)).

remCtxValue(Name,Ctx,_Value):-checkCtx(Ctx),setCtxValue(Name,Ctx,deleted),!.


pushCtxFrame(Name,Ctx,NewValues):-checkCtx(Ctx),get_ctx_holderFreeSpot(Ctx,Holder,GuestDest),!,Holder=frame(Name,GuestDest,NewValues).

popCtxFrame(Name,Ctx,PrevValues):-checkCtx(Ctx),get_ctx_frame_holder(Ctx,Name,Frame),Frame = frame(Name,Destructor,PrevValues),Destructor,!.

checkCtx(Ctx):-nonvar(Ctx),!.
checkCtx(Ctx):-makeLocalContext(broken,Ctx),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% get the frame holder
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_ctx_frame_holder(Ctx,Name,R):-compound(Ctx),get_ctx_frame_holder1(Ctx,Name,R).
get_ctx_frame_holder1(v(_,_,_),_Name,_R):-!,fail.
get_ctx_frame_holder1(frame(Name,Dest,Ctx),Name,R):- R = frame(Name,Dest,Ctx),!.
get_ctx_frame_holder1([H|T],Name,R):- nonvar(H), !, ( get_ctx_frame_holder(T,Name,R);get_ctx_frame_holder1(H,Name,R)) .
%%get_ctx_frame_holder1(Ctx,Name,Ctx):-!,get_ctx_frame_holder1(Ctx,Name,R).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% get the holders areas last in first out %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% get_ctx_holder(+Ctx, -PlaceToSearch),

get_ctx_holder(Ctx,R):-compound(Ctx),get_ctx_holder1(Ctx,R).
get_ctx_holder1([H|T],R):- nonvar(H), !, ( get_ctx_holder(T,R);get_ctx_holder1(H,R)) .
get_ctx_holder1(v(_,_,_),_R):-!,fail.%% get_ctx_holder(Ctx,R).
get_ctx_holder1(frame(_N,_Dest,Ctx),R):-!,get_ctx_holder(Ctx,R).
%get_ctx_holder1(Ctx,R):- functor(Ctx,F,A),A<3,!,fail.
get_ctx_holder1(assoc(Ctx),assoc(Ctx)):-!.
get_ctx_holder1(Ctx,Ctx).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% find a free area to place a: vv(name,val) %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% get_ctx_holderFreeSpot(+Ctx, -Put_NV, -CallToRemoveNV)

get_ctx_holderFreeSpot(Ctx,NamedValue,no_destructor(holder)):-no_cyclic_terms,!,get_ctx_holderFreeSpot0(Ctx,NamedValue,_NO_Destruct),!.
get_ctx_holderFreeSpot(Ctx,NamedValue,Destruct):-get_ctx_holderFreeSpot0(Ctx,NamedValue,Destruct).

get_ctx_holderFreeSpot0(Ctx,NamedValue,Destruct):-compound(Ctx),get_ctx_holderFreeSpot1(Ctx,NamedValue,Destruct).

get_ctx_holderFreeSpot1(assoc(_Ctx),_,_):-!,fail.
get_ctx_holderFreeSpot1(frame(Key,_Inner_Dest,Ctx),NamedValue,Destruct):- nonvar(Key), !, get_ctx_holderFreeSpot1(Ctx,NamedValue,Destruct).
get_ctx_holderFreeSpot1(Ctx,NamedValue,Destruct):-functor(Ctx,F,A),!,get_ctx_holderFreeSpot1(Ctx,F,A,NamedValue,Destruct).

get_ctx_holderFreeSpot1(Ctx,'.',2,NamedValue,nb_setarg(Ctx,2,NEXT)):-arg(2,Ctx,Try1), var(Try1),!, Try1 = [NamedValue|NEXT].
get_ctx_holderFreeSpot1(Ctx,'.',2,NamedValue,Destruct):-arg(2,Ctx,Try2),get_ctx_holderFreeSpot0(Try2,NamedValue,Destruct).

%%get_ctx_holderFreeSpot1(Ctx,_,_,NamedValue,_):-!,fail.
%%get_ctx_holderFreeSpot1(Ctx,_,_,NamedValue,nb_setarg(Ctx,N,NEXT)):-arg(N,Ctx,Try3),var(Try3),!, Try3 = [NamedValue|NEXT].
%%get_ctx_holderFreeSpot1(Ctx,_,_,NamedValue,Destruct):-arg(N,Ctx,Try4),get_ctx_holderFreeSpot0(Try4,NamedValue,Destruct).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% find the value holder associated with a keyname
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_ctx_value(Name,Ctx,Value,Setter):-nonvar(Name),var(Value),get_o_value(Name,Ctx,Value,OuterSetter),bestSetterFn(Value,OuterSetter,Setter).

get_o_value(Name,Ctx,Value,no_setter(Name)):-no_cyclic_terms,!,get_o_value0(Name,Ctx,Value,_HIDE_Setter),!.
get_o_value(Name,Ctx,Value,Setter):-hotrace(get_o_value0(Name,Ctx,Value,Setter)),!.

get_o_value0(Name,Ctx,Value,Setter):-compound(Ctx),get_o_value1(Name,Ctx,Value,Setter).
get_o_value1(Name,assoc(Ctx),Value,set_assoc):- get_assoc(Name,Ctx,Value),!.
get_o_value1(Name,frame(Key,_Inner_Dest,Ctx),Value,Setter):- nonvar(Key), get_o_value0(Name,Ctx,Value,Setter),!.
get_o_value1(Name,[H|T],Value,Setter):- !,(get_o_value0(Name,T,Value,Setter);get_o_value1(Name,H,Value,Setter)).
get_o_value1(Name,Pred,Value,Setter):-functor(Pred,F,A),!,get_n_value(Name,Pred,F,A,Value,Setter).

get_n_value(Name,Name,_F,_A,_Value,_):-!,fail.
get_n_value(Name,Pred,Name,1,Value,nb_setarg(1,Pred)):-arg(1,Pred,Value).
get_n_value(Name,Pred,Name,_,Value,Setter):- arg(1,Pred,Value),!,arg(2,Pred,Setter). %% value can actually be 'Pred'
get_n_value(Name,Pred,Dash,2,Value,nb_setarg(2,Pred)):-arg(1,Pred,Name),member(Dash,[=,-,vv]),!, arg(2,Pred,Value).
%%get_n_value(Name,Pred,'.',2,Value,Setter):-arg(2,Pred,Try1), get_o_value0(Name,Try1,Value,Setter);(arg(1,Pred,Try2),get_o_value0(Name,Try2,Value,Setter)).
%%get_n_value(Name,Pred,_,_,Value,Setter):- !, arg(_,Pred,Try2),get_o_value0(Name,Try2,Value,Setter).


lastMember(_E,List):-var(List),!,fail.
lastMember(E,[H|List]):-lastMember(E,List);E=H.

lastMember(E,List,Rest):-lastMember(E,List),!,delete_safe(List,E,Rest),!.

delete_safe(List,_E,Rest):-var(List),!,Rest=List.
delete_safe(List,E,Rest):-is_list(List),!,delete(List,E,Rest).
delete_safe([H|List],E,Rest):- H==E,!,delete_safe(List,E,Rest).
delete_safe([H|List],E,[H|Rest]):-delete_safe(List,E,Rest).


getKeyValue(FullList,N=V):-lastMember(N=V,FullList),!.
%%addKeyValue(FullList,N=V):-nonvar(N),!,append(_Closed,[N=V|_],FullList),!.
addKeyValue(FullList,NV):- prolog_must((not(ground(FullList)),nonvar(NV))),append(_Closed,[NV|_],FullList),!.


lastMember2(E,List):-to_open_list(_,Closed,_Open,List),reverse(Closed,Rev),member(E,Rev).

%lastMember(End,List) :- append(_,[End|_],List).

ctxHideIfNeeded(_Ctx,Before,After):-hideIfNeeded(Before,After),!.

hideIfNeeded(I,I):- (var(I);atomic(I)),!.
hideIfNeeded([I|_],ctx):-nonvar(I),I=frame(_,_,_),!.
hideIfNeeded([I|_],ctx):-nonvar(I),functor(I,frame,_),!.
hideIfNeeded([I|N],[I0|N0]):-!,hideIfNeeded(I,I0),hideIfNeeded(N,N0),!.
hideIfNeeded(Comp,Comp2):-compound(Comp),Comp=..[L,I|ST],hideIfNeeded([I|ST],[OI|OIST]),Comp2=..[L,OI|OIST],!.
hideIfNeeded(I,I):-!.


to_open_list(FullList,Closed,Open,FullList) :- append(Closed,Open,FullList),var(Open),!.
to_open_list(Closed,Closed,Open,FullList) :- append(Closed,Open,FullList),!.


revappend([], Ys, Ys).
revappend([X|Xs], Ys, Zs) :- revappend(Xs, [X|Ys], Zs).

reverseA(Xs,Ys) :- revappend(Xs,[],Ys).

appendAttributes(_Ctx,L,R,AA):-hotrace((mergeAppend0(L,R,A),list_to_set_safe(A,AA))),!.

mergeAppend0(L,R,R):-var(L),!,var(R),!.
mergeAppend0(L,R,A):-var(R),append(L,R,A),!.
mergeAppend0(L,R,A):-var(L),append(L,R,A),!.
mergeAppend0(L,[R|RR],A):-eqmember(R,L),mergeAppend0(L,RR,A).
mergeAppend0([L|LL],R,A):-eqmember(L,R),mergeAppend0(LL,R,A).
mergeAppend0(L,R,A):-append(L,R,A).

eqmember(E,List):-copy_term_numvars(E:List,E0:List0),member(E0,List0).
copy_term_numvars(OLD,NEW):-copy_term(OLD,NEW),numbervars(NEW,0,_).


