% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_terms.pl
:- if(current_prolog_flag(xref,true)).
:- module(mpred_terms,
          [ 
          any_to_number/2,
          is_ftText/1,
          any_to_value/2,
          atom_to_value/2
          ]).


/** <module> mpred_terms
% Provides a common set of operators in translation between the several logical languages
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
:- include('mpred_header.pi').
:-endif.


:- export(any_to_number/2).
%% any_to_value( ?Var, ?Var) is semidet.
%
% Any Converted To Value.
%
any_to_value(Var,Var):-var(Var),!.
any_to_value(V,Term):-atom(V),!,atom_to_value(V,Term).
any_to_value(A,V):-any_to_number(A,V).
any_to_value(A,A).


%% any_to_number( :TermN, ?N) is semidet.
%
% Any Converted To Number.
%
any_to_number(N,N):- number(N),!.
any_to_number(ftDiceFn(A,B,C),N):- ground(A),if_defined(roll_dice(A,B,C,N)),!.
any_to_number(A,N):-atom(A),atom_to_value(A,V),A\=V,any_to_number(V,N).
any_to_number(A,N):- catch(number_string(N,A),_,fail).

%% atom_to_value( ?V, :TermTerm) is semidet.
%
% Atom Converted To Value.
%
atom_to_value(V,Term):-not(atom(V)),!,any_to_value(V,Term).
% 56
atom_to_value(V,Term):- catch((read_term_from_atom(V,Term,[variable_names([])])),_,fail),!.
% 18d18+4000
atom_to_value(V,ftDiceFn(T1,T2,+T3)):- atomic_list_concat_safe([D1,'d',D2,'+',D3],V), atom_to_value(D1,T1),atom_to_value(D2,T2),atom_to_value(D3,T3),!.
atom_to_value(V,ftDiceFn(T1,T2,-T3)):- atomic_list_concat_safe([D1,'d',D2,'-',D3],V), atom_to_value(D1,T1),atom_to_value(D2,T2),atom_to_value(D3,T3),!.



%% is_ftText( ?Arg) is semidet.
%
% If Is A Format Type Text.
%
is_ftText(Arg):- string(Arg),!.
is_ftText(Arg):- \+ compound(Arg),!,fail.
is_ftText(Arg):- safe_functor(Arg,s,_),!.
is_ftText([Arg|_]):-string(Arg),!.
is_ftText(Arg):- is_ftVar(Arg),!,fail.
is_ftText(Arg):- text_to_string_safe(Arg,_),!.
is_ftText(Arg):- safe_functor(Arg,S,_), ereq(resultIsa(S,ftText)).

:- kb_global(baseKB:ftText/1).
baseKB:ftText(A):- !, if_defined(term_is_ft(A, ftText),is_ftText(A)),!.

% =======================================================
% term utils
% =======================================================

:- was_export(inverse_args/2).



%% inverse_args( ?AR, ?GS) is semidet.
%
% Inverse Arguments.
%
inverse_args([AR,GS],[GS,AR]):-!.
inverse_args([AR,G,S],[S,G,AR]):-!.
inverse_args([A,R,G,S],[S,R,G,A]):-!.
inverse_args([P,A,R,G,S],[S,A,R,G,P]):-!.

:- was_export(same_vars/2).



%% same_vars( ?T1, ?T2) is semidet.
%
% Same Variables.
%
same_vars(T1,T2):-term_variables(T1,V1),term_variables(T2,V2),!,V1==V2.




%% replace_arg( ?C, :PRED3A, ?VAR, ?CC) is semidet.
%
% Replace Argument.
%
replace_arg(C,A,_VAR,_CC):-sanity((is_ftCompound(C),integer(A))),fail.
replace_arg((C:-B),A,VAR,(CC:-B)):-!,replace_arg(C,A,VAR,CC).
replace_arg(~ (C),A,VAR,~(CC)):-!,replace_arg(C,A,VAR,CC).
replace_arg( \+ (C),A,VAR,~(CC)):-!,replace_arg(C,A,VAR,CC).
replace_arg(M:(C),A,VAR,M:(CC)):-!,replace_arg(C,A,VAR,CC).
replace_arg(C,0,VAR,CC):-!, C=..[_|ARGS],CC=..[VAR|ARGS].
replace_arg(C,1,VAR,CC):-!, C=..[F,_|ARGS],CC=..[F,VAR|ARGS].
replace_arg(C,2,VAR,CC):-!, C=..[F,A,_|ARGS],CC=..[F,A,VAR|ARGS].
replace_arg(C,3,VAR,CC):-!, C=..[F,A,B,_|ARGS],CC=..[F,A,B,VAR|ARGS].
% replace_arg(C,A,VAR,CO):- dupe_term(C,CC),setarg(A,CC,VAR),!,CC=CO.
replace_arg(C,A,VAR,CC):- C=..FARGS,replace_nth_arglist(FARGS,A,VAR,FARGO),!,CC=..FARGO.

% :- mpred_trace_nochilds(replace_arg/4).

%% replace_nth_arglist(+List, +Index, +Element, -NewList) is det[private]
% Replace the Nth (1-based) element of a list.
% :- mpred_trace_nochilds(replace_nth_arglist/4).



%% replace_nth_arglist( :TermARG1, ?VALUE2, ?VAR, :TermVAR) is semidet.
%
% Replace Nth Arglist.
%
replace_nth_arglist([],_,_,[]):- !.
replace_nth_arglist([_|ARGO],0,VAR,[VAR|ARGO]):- !.
replace_nth_arglist([T|FARGS],A,VAR,[T|FARGO]):- 
    A2 is A-1,replace_nth_arglist(FARGS,A2,VAR,FARGO).





%% replace_nth_ref( :TermARG1, ?N, ?OldVar, ?NewVar, :TermARG5) is semidet.
%
% Replace Nth Ref.
%
replace_nth_ref([],_N,_OldVar,_NewVar,[]):- !,trace_or_throw_ex(missed_the_boat).
replace_nth_ref([OldVar|ARGS],1,OldVar,NewVar,[NewVar|ARGS]):- !.
replace_nth_ref([Carry|ARGS],Which,OldVar,NewVar,[Carry|NEWARGS]):- 
 Which1 is Which-1,
 replace_nth_ref(ARGS,Which1,OldVar,NewVar,NEWARGS),!.


% :- mpred_trace_nochilds(update_value/3).



%% update_value( ?OLD, ?NEW, ?NEXT) is semidet.
%
% Update Value.
%
update_value(OLD,NEW,NEXT):- var(NEW),!,trace_or_throw_ex(logicmoo_bug(update_value(OLD,NEW,NEXT))).
update_value(OLD,NEW,NEWV):- var(OLD),!,compute_value_no_dice(NEW,NEWV).
update_value(OLD,X,NEW):- is_list(OLD),!,list_update_op(OLD,X,NEW),!.
update_value(OLDI,+X,NEW):- compute_value(OLDI,OLD),number(OLD),catch(NEW is OLD + X,_,fail),!.
update_value(OLDI,-X,NEW):- compute_value(OLDI,OLD),number(OLD),catch(NEW is OLD - X,_,fail),!.
update_value(OLDI,X,NEW):- number(X),X<0,compute_value(OLDI,OLD),number(OLD),catch(NEW is OLD + X,_,fail),!.
update_value(_,NEW,NEWV):- compute_value_no_dice(NEW,NEWV),!.




%% flatten_append( ?First, ?Last, ?Out) is semidet.
%
% Flatten Append.
%
flatten_append(First,Last,Out):-flatten([First],FirstF),flatten([Last],LastF),append(FirstF,LastF,Out),!.




%% list_update_op( ?OLDI, :TermX, ?NEW) is semidet.
%
% List Update Oper..
%
list_update_op(OLDI,+X,NEW):-flatten_append(OLDI,X,NEW),!.
list_update_op(OLDI,-X,NEW):-flatten([OLDI],OLD),flatten([X],XX),!,list_difference_eq(OLD,XX,NEW),!.




%% compute_value_no_dice( ?NEW, ?NEW) is semidet.
%
% Compute Value No Dice.
%
compute_value_no_dice(NEW,NEW):- compound(NEW),functor_catch(NEW,ftDiceFn,_),!.
compute_value_no_dice(NEW,NEW):- compound(NEW),functor_catch(NEW,ftDice,_),!.
compute_value_no_dice(NEW,NEWV):-compute_value(NEW,NEWV).




%% compute_value( ?NEW, ?NEWV) is semidet.
%
% Compute Value.
%
compute_value(NEW,NEWV):-catch(NEWV is NEW,_,fail),!.
compute_value(NEW,NEWV):-catch(any_to_value(NEW,NEWV),_,fail),!.
compute_value(NEW,NEW).




%% insert_into( :TermARGS, ?VALUE2, ?Insert, :TermInsert) is semidet.
%
% Insert Converted To.
%
insert_into(ARGS,0,Insert,[Insert|ARGS]):- !.
insert_into([Carry|ARGS],After,Insert,[Carry|NEWARGS]):- 
   After1 is After - 1,
   insert_into(ARGS,After1,Insert,NEWARGS).



% ========================================
% is_holds_true/is_holds_false
% ========================================


:- was_export(into_plist/2).

%= 	 	 

%% into_plist( ?In, ?Out) is semidet.
%
% Converted To Plist.
%
into_plist(In,Out):-into_plist_arities(2,12,In,Out).

:- was_export(into_plist_arities/4).

%= 	 	 

%% into_plist_arities( ?Min, ?Max, ?PLIST, ?PLISTO) is semidet.
%
% Converted To Plist Arities.
%
into_plist_arities(Min,Max,PLIST,PLISTO):- var(PLIST),!,between(Min,Max,X),length(PLIST,X),PLISTO=PLIST.
into_plist_arities(_,_,[P|LIST],[P|LIST]):-var(P),!.
into_plist_arities(_,_,[(t)|PLIST],PLIST):-!.  % t is our versuion of '$holds' or call/N
into_plist_arities(_,_,plist(P,LIST),[P|LIST]):-!.
into_plist_arities(_,_,Call,PLIST):- Call=..PLIST. % finally the fallthrue



%= 	 	 

%% never_mpred_tcall( ?VALUE1) is semidet.
%
% Never Managed Predicate Managed Predicate.
%

never_mpred_tcall(mpred_prop).
never_mpred_tcall(isa).
never_mpred_tcall(arity).


local_qh_mpred_prop(M,F,A,C):- call_u(mpred_prop(M,F,A,C)).


% :- setup_mpred_ops.


                   
%= 	 	 

:- meta_predicate(if_result(0,0)).

%= 	 	 

%% if_result( :GoalTF, :Goal) is semidet.
%
% If Result.
%
if_result(TF,Call):-(TF->Call;true).




%= 	 	 

%% mpred_plist_t( ?P, :TermLIST) is semidet.
%
% Managed Predicate Plist True Stucture.
%
/* mpred_plist_t(P,[]):-!,t(P). */
mpred_plist_t(P,LIST):-var(P),!,is_list(LIST),CALL=..[t,P|LIST],on_x_debug((CALL)).
mpred_plist_t(t,[P|LIST]):-!, mpred_plist_t(P,LIST).
%mpred_plist_t(mpred_isa,[C,_A,I]):-!,ground(I:C),local_qh_mpred_isa(C,I).
mpred_plist_t(isa,[I,C]):-!,call(call,t,C,I).
mpred_plist_t(P,_):-never_mpred_tcall(P),!,fail.
mpred_plist_t(P,[L|IST]):-is_holds_true(P),!,mpred_plist_t(L,IST).
mpred_plist_t(P,LIST):-is_holds_false(P),!,call_u(mpred_f(LIST)).
mpred_plist_t(P,LIST):- CALL=..[t,P|LIST],on_x_debug(CALL).


:- meta_predicate(mpred_fa_call(?,?,0)).



%= 	 	 

%% mpred_fa_call( ?F, ?UPARAM2, :Goal) is semidet.
%
% Managed Predicate Functor-arity Call.
%
mpred_fa_call(F,A,Call):- var(F),!,
 no_repeats(F,(clause_b(support_hilog(F,A));clause_b(arity(F,A)))), 
   once((F\==t, 
   \+ a(rtNotForUnboundPredicates,F),current_predicate(F,M:_OtherCall))),
    on_x_debug(M:Call).
mpred_fa_call(M:F,A,Call):- nonvar(M),!,mpred_fa_call(F,A,M:Call).
mpred_fa_call(F,_,Call):-F\==t,current_predicate(F,M:_OtherCall),!,M:Call.


%= 	 	 

%% mpred_fact_arity( ?VALUE1, ?VALUE2) is semidet.
%
% Managed Predicate Fact Arity.
%
mpred_fact_arity(F,A):- call_u(arity(F,A)),
  suggest_m(M),
  once(local_qh_mpred_prop(M,F,A,prologHybrid);
     local_qh_mpred_prop(M,F,A,pfcControlled);
     local_qh_mpred_prop(M,F,A,prologPTTP);
     local_qh_mpred_prop(M,F,A,prologKIF)),!.


%= 	 	 

%% prologHybridFact( ?G) is semidet.
%
% Prolog Hybrid Fact.
%
prologHybridFact(G):- (var(G)->(mpred_fact_arity(F,A),safe_functor(G,F,A));true),into_mpred_form(G,M),!,no_repeats(call_u(M)).





%:- fixup_exports.


