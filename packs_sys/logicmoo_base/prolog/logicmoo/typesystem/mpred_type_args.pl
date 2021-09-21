% =======================================================
/* 
% This is mainly used by the moo_loader but also needed everywhere
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
% =======================================================
% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_type_args.pl
:- module(mpred_type_args,
          [ any_to_relation/2,
            argIsa_op_call/4,
            as_one_of/2,
            show_count/1,
            assert_argIsa/3,
            argIsa_known/3,
            assert_predArgTypes/1,
            assert_predArgTypes_fa/2,
            assert_predArgTypes_from_left/3,
            assert_predArgTypes_from_right/3,
            assert_predArgTypes_l/3,
            atom_to_value/2,
            checkAnyType/4,
            coerce/3,
            correctAnyType/4,
            correctAnyTypeOrFail/4,
            correctArgsIsa/2,
            correctArgsIsa/3,
            %correctArgsIsa/4,
            correctArgsIsa/3,
            correctArgsIsa00/3,
            correctFormatType/4,
            correctType/4,
            correctType0/4,
            correctTypeArg/4,
            correctType_gripe/4,
            % decl_coerce/3,
            %deduceFromArgTypes/1,
            deduced_is_tCol/1,
            discoverAndCorrectArgsIsa/5,
            discoverAndCorrectArgsIsa_from_left/5,
            discoverAndCorrectArgsIsa_from_right/5,
            evaluatableArg/2,
            evaluatableFunctor/1,
            %compoundSpecs/2,
            %meta_argtypes/1,
            % coerce_hook/3,
            is_boolean/1,
            is_declarations/1,
            is_ephemeral/1,
            is_id/1,
            is_list_of/2,
            is_renamed_to/2,
            is_rest/1,
            is_rest_of/2,
            is_spec/1,
            is_valuespec/1,
            list_to_callform/3,
            is_declarations/1,
            %mpred_arity_pred/1,
            must_equals/2,
            must_equals_correct/3,
            pl_arg_type/2,
            roll_dice/4,
            term_is_ft/2,
            term_is_ft_how/2,
            to_format_type/2,
            trans_subft/2,
            mpred_type_args_file/0
          ]).

%:- include('mpred_header.pi').

% autoloading user:portray_clause_pi/2 from /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/logicmoo_util_first

:- set_how_virtualize_file(bodies).



/*
:- dynamic((
        coerce/3,
        decl_coerce/3,
        deduceFromArgTypes/1,
   coerce_hook/3)).
*/

%% to_format_type( ?COL, ?FT) is semidet.
%
% Converted To Format Type.
%
% genls-GenlDenotesSpecInstances
%
to_format_type(FT,FT):-a(ttExpressionType,FT),!.
to_format_type(COL,FT):- clause_b(formatted_resultIsa(FT,COL)),!.
to_format_type(COL,FT):- clause_b(resultIsa(FT,COL)),a(ttExpressionType,FT),!.
to_format_type(COL,ftTerm(COL)).

% genlsSpecDenotesGenlInstances(FT,COL):-to_format_type(COL,FT).



%= 	 	 

%% assert_argIsa( ?Prop, ?N, ?Type) is semidet.
%
% assert Argument  (isa/2).
%

assert_argIsa(Prop,N,Type):-show_failure(why,ain_fast(argIsa(Prop,N,Type))).



%% assert_predArgTypes( ?ArgTs) is semidet.
%
% assert Predicate Argument  Types.
%
assert_predArgTypes(ArgTs):-not(compound(ArgTs)),!.
assert_predArgTypes(ArgTs):- numbervars(ArgTs,0,_,[functor_name(ftTerm),attvar(skip)]),get_functor(ArgTs,F),assert_predArgTypes_fa(F,ArgTs).


%= 	 	 

%% assert_predArgTypes_fa( ?VALUE1, ?ArgTs) is semidet.
%
% assert Predicate Argument  Types Functor-Arity.
%
assert_predArgTypes_fa(_,ArgTs):- nonvar(ArgTs),ArgTs=(_/_),!.
assert_predArgTypes_fa(F,ArgTs):- not(is_list(ArgTs)),ArgTs=..[_|ArgsL],!,assert_predArgTypes_fa(F,ArgsL).
%assert_predArgTypes_fa(F,ArgsList):- clause_b(ftAction(F),true),!,show_call(why,must(assert_predArgTypes_from_left(F,1,ArgsList))).
assert_predArgTypes_fa(F,ArgsList):- length(ArgsList,L),assert_predArgTypes_l(F,L,ArgsList).

%assert_predArgTypes_l(F,L,ArgsList):- arity_no_bc(F,A),!,must( (A>=L) -> assert_predArgTypes_from_right(F,A,ArgsList);true).




%% assert_predArgTypes_l( ?F, ?L, ?ArgsList) is semidet.
%
% assert Predicate Argument  Types (List version).
%
assert_predArgTypes_l(F,L,ArgsList):- must(assert_predArgTypes_from_right(F,L,ArgsList)).



%= 	 	 

%% assert_predArgTypes_from_right( ?F, ?A, :TermArgsList) is semidet.
%
% assert Predicate Argument  Types Converted From right.
%
assert_predArgTypes_from_right(_,_,[]):-!.
assert_predArgTypes_from_right(_,_,(_/_)):-!.
assert_predArgTypes_from_right(F,A,ArgsList):-append(Left,[Last],ArgsList),
   assert_argIsa(F,A,Last),!,Am1 is A -1, assert_predArgTypes_from_right(F,Am1,Left).


generateArgVars(P,ArgPred,Else):-
 (ground(P)->true;
  (arg(N,P,E),var(E),(call_u(call(ArgPred,N,Arg))-> Arg=E, call_u(call(Else,E))),
     nonvar(E),generateArgVars(P,ArgPred,Else))).
   



%% assert_predArgTypes_from_left( ?F, ?A, :TermType) is semidet.
%
% assert Predicate Argument  Types Converted From left.
%
assert_predArgTypes_from_left(_,_,[]):-!.
assert_predArgTypes_from_left(F,A,[Type|ArgsList]):-assert_argIsa(F,A,Type),!,Ap1 is A + 1,assert_predArgTypes_from_left(F,Ap1,ArgsList).



%= 	 	 

%% term_is_ft( :TermTerm, :TermType) is semidet.
%
% Term If Is A Format Type.
%
term_is_ft(Term,Type):- is_ftVar(Term),!,member(Type,[ftVar,ftProlog]).
term_is_ft(_ANY,Type):- Type==ftVar,!,fail.
term_is_ft([T|Term],ftListFn(Type)):-is_list_of(Type,[T|Term]).
term_is_ft(_ANY,Type):- nonvar(Type),(ttExpressionType==Type;(\+ a(ttExpressionType,Type))),!,fail.
term_is_ft(Term,Type):- nonvar(Type), term_is_ft_how(Term,Type),!.
term_is_ft(Term,Type):- no_repeats(Type,(term_is_ft_how(Term,Was),trans_subft(Was,Type))).



%= 	 	 

%% term_is_ft_how( ?Term, ?Type) is semidet.
%
% Term If Is A Format Type How.
%
term_is_ft_how(Term,Type):- clause_b(quotedDefnIff(Type,Info)),nonvar(Info),
   (show_success(why,(Info='uSubLQuoteFn'(LISPSYMBOL),nop(dmsg(Term+Type+LISPSYMBOL))))-> 
                 fail;
                 (append_term(Info,Term,CALL),call_u(CALL))),!.

term_is_ft_how(Term,Type):- compound(Term),functor(Term,F,A),functor(Type,F,A),
  once((a(meta_argtypes,Type),Type=..[_|Types],Term=..[_|Args],
     maplist(call_as_t,Types,Args))).

call_as_t(T,A):-append_term(T,A,Call),call_u(Call).

%= 	 	 

%% trans_subft( ?FT, ?FT) is semidet.
%
% Trans Subft.
%
trans_subft(FT,FT).
trans_subft(FT,Sub):-clause_b(subFormat(FT,Sub)).
trans_subft(FT,Sub):-clause_b(subFormat(FT,A)),clause_b(subFormat(A,Sub)).
trans_subft(FT,Sub):-clause_b(subFormat(FT,A)),clause_b(subFormat(A,B)),clause_b(subFormat(B,Sub)).


%= 	 	 

%% is_id( ?ID) is semidet.
%
% If Is A Id.
%
is_id(ID):-atom(ID)->true;(compound(ID),arg(1,ID,A),is_id(A)).

%= 	 	 

%% is_boolean( ?VALUE1) is semidet.
%
% If Is A Boolean.
%
is_boolean(isMissing):-!,fail.
is_boolean(vTrue).
is_boolean(vFalse).


%= 	 	 

%% is_declarations( ?C) is semidet.
%
% If Is A Declarations.
%
is_declarations(TypesIn):- strip_module(TypesIn,_,Types), compound(Types), ground(Types), Types\=(_/_), Types\=(_:_/_), Types\='$VAR'(_),!, (\+ (arg(_,Types,T), \+ is_spec(T))).


%= 	 	 

%% is_spec( ?T) is semidet.
%
% If Is A Spec.
%
is_spec(T):- call_u(tCol(T))->true;is_declarations(T).




%% is_rest( :TermARG1) is semidet.
%
% If Is A Rest.
%
is_rest([_|Term]):-not(is_list(Term)).

%= 	 	 

%% is_rest_of( ?Type, :TermARG2) is semidet.
%
% If Is A Rest Of.
%
is_rest_of(_Type,[_|Term]):-not(is_list(Term)).

%= 	 	 

%% is_list_of( ?Type, :TermTerm) is semidet.
%
% If Is A List Of.
%
is_list_of(Type,Term):- is_rest(Term),!,Type=ftTerm.
is_list_of(Type,[T|Term]):-term_is_ft(T,Type),maplist(is_list_of(Type),Term).

/*
pl_arg_type_or_functor(Arg,Type):- pl_arg_type(Arg,T) , 
 (T==ftCompound -> functor(Arg,Type,_); 
  ( (T==ftListFn(_),Arg=[_|_])-> T=[Type|_] ;
         Type=T)) .

sameArgTypes(A,C):-same(A,C);(pl_arg_type(C,CT),pl_arg_type(A,AT),!,colsOverlap(AT,CT)).
colsOverlap(AT,AT).

*/


%= 	 	 

%% pl_arg_type( ?Arg, ?Type) is semidet.
%
% Pl Argument Type.
%
pl_arg_type(Arg,Type):- 
      var(Arg) -> Type =ftVar;
      integer(Arg) -> Type =ftInt;
      number(Arg) -> Type =ftFloat;
      string(Arg) -> Type =ftString;
      is_ftText(Arg) -> Type =ftText;
      is_list(Arg) -> Type = ftList;
  /* is_list(Arg) -> Type =ftListFn(_); */
      atom(Arg) -> Type =ftAtom;
      atomic(Arg) -> Type =ftAtomic;
      compound(Arg) -> Type =ftCompound;
         Arg = Type.





% :- was_dynamic(coerce/3).
:- was_export(coerce/4).

%= 	 	 

%% coerce( ?VALUE1, ?VALUE2, ?NewThing, ?Else) is semidet.
%
% Coerce.
%
coerce(What,Type,NewThing,_Else):- call_u(coerce(What,Type,NewThing)),!.
coerce(_ ,_,     NewThing,Else):- NewThing = Else.

coerce(A,B,C):-no_repeats(call_u(coerce_hook(A,B,C))),nop((sanity(show_failure(call_u(isa(C,B))))->!;true)).

baseKB:coerce(A,B,C):-coerce(A,B,C).



%= 	 	 

%% as_one_of( ?Type, ?TypeO) is semidet.
%
% Converted To One Of.
%
as_one_of(Types,Type):-nonvar(Type),tCol(Type),!,member(Type,Types).
as_one_of([Type],TypeO):-!,same_arg(same_or(genls),Type,TypeO).
as_one_of(Types,isOneOf(Types)).



%= 	 	 

%% argIsa_op_call( ?Op, :TermFunc, ?N, ?Type) is semidet.
%
% Argument  (isa/2) Oper. call.
%
argIsa_op_call(Op,_:F,N,Type):-!,argIsa_op_call(Op,F,N,Type),!.
argIsa_op_call(Op,F/_,N,Type):- !,argIsa_op_call(Op,F,N,Type),!.
argIsa_op_call(Op,Func,N,Type):- compound(Func),!,functor(Func,F,_),argIsa_op_call(Op,F,N,Type),!.
argIsa_op_call(_,F,N,Type):-quietly((loop_check((call_u(argIsa(F,N,Type)),!),Type=ftTerm),must(nonvar(Type)))).

argIsa_known(F,N,Type):-argIsa_op_call(_,F,N,Type).


:- was_export(correctArgsIsa/2).

%= 	 	 

%% correctArgsIsa( ?In, ?Out) is semidet.
%
% correct Arguments  (isa/2).
%
correctArgsIsa(In,Out):- correctArgsIsa(query(must,t),In,Out),!.

:- was_export(correctArgsIsa/3).

%= 	 	 

%% correctArgsIsa( ?VALUE1, :TermNC, :TermNC) is semidet.
%
% correct Arguments  (isa/2).
%
% correctArgsIsa0(_,G,G):- (\+ t_l:infMustArgIsa), (is_release; bad_idea; skipWrapper;  t_l:infSkipArgIsa),!.
% correctArgsIsa(_,G,G):-!. 
correctArgsIsa(_,G,GG):- t_l:infSkipArgIsa, !,must_equals(G,GG).
correctArgsIsa(_,G,GG):- sanity(var(GG)), ( \+ compound(G)), !,must(G=GG).
% correctArgsIsa(Op,G,GG):- correctArgsIsa0(Op,G,GG),var(GG),!,break,correctArgsIsa0(Op,G,GG),break,break.
correctArgsIsa(Op,G,GG):- must((correctArgsIsa0(Op,G,GG),sanity(nonvar(GG)))).

correctArgsIsa0(_,NC,NC):- \+ compound(NC),!.
correctArgsIsa0(Op,G,GG):- is_list(G),!,must_maplist(correctArgsIsa0(Op),G,GG).
correctArgsIsa0(Op,M:G,MAA):- nonvar(M),!,correctArgsIsa0(Op,G,GG),M:GG=MAA.
correctArgsIsa0(_,ISA,GG):- was_isa(ISA,_,_),!,must_equals(ISA,GG).
correctArgsIsa0(Op,(A,B),(AA,BB)):-!,correctArgsIsa0(Op,A,AA),correctArgsIsa0(Op,B,BB).
correctArgsIsa0(Op,(A;B),(AA;BB)):-!,correctArgsIsa0(Op,A,AA),correctArgsIsa0(Op,B,BB).
correctArgsIsa0(_,G,GG):- get_functor(G,F),functor_no_correct(F),!,must_equals(G,GG).
correctArgsIsa0(Op,A,RESULTC):-A=..[PRED|ARGS],correctArgsIsa00(Op,[PRED|ARGS],RESULT), list_to_callform(RESULT,t,RESULTC).

functor_no_correct(F):-lookup_u(functorDeclares(F)).
functor_no_correct(agent_text_command).


:- was_export(correctArgsIsa/4).

%= 	 	 

%% correctArgsIsa( ?Op, ?A, ?Type, ?AA) is semidet.
%
% correct Arguments  (isa/2).
%
% correctArgsIsa(Op,A,Type,AA):- trace_or_throw(warn(not(correctArgsIsa(Op,A,Type,AA)))).


%= 	 	 

%% list_to_callform( ?ARGS, ?Functor, ?CALL) is semidet.
%
% List Converted To Callform.
%
list_to_callform([P|ARGS],_,CALL):-atom(P),!,CALL=..[P|ARGS].
list_to_callform(ARGS,Functor,CALL):-CALL=..[Functor|ARGS].



show_count(F/A):- functor(P,F,A), predicate_property(M:P,number_of_clauses(N)),dmsg(F=M:N).

%= 	 	 

%% correctArgsIsa00( ?VALUE1, :TermProp, :TermAA) is semidet.
%
% correct Arguments  (isa/2) Primary Helper Primary Helper.
%
correctArgsIsa00(_ ,[Prop|Args],AA):-stack_check(1000), var(Prop),!,AA=[Prop|Args].
correctArgsIsa00(Op,[KP,Prop|Args],AA):-is_holds_true(KP),!,correctArgsIsa00(Op,[Prop|Args],AA).
correctArgsIsa00(Op,[KP,Prop|Args],[KP|AArgs]):-logical_functor_ft(KP),!,correctAnyType(Op,[Prop|Args],ftListFn(ftAskable),AArgs).
correctArgsIsa00(Op,[KP,Prop|Args],[KP|AA]):-is_holds_false(KP),!,correctArgsIsa00(Op,[KP,Prop|Args],AA).
%correctArgsIsa00(_ ,[Prop,Arg],[Prop,Arg]):- !.
correctArgsIsa00(Op,[Prop,ArgI],[Prop,ArgO]):- a(tCol,Prop),!, correctAnyType(query(ftID,Op),ArgI,Prop,ArgO).
correctArgsIsa00(Op,[Prop|Args],[Prop|AArgs]):- discoverAndCorrectArgsIsa(Op,Prop,1,Args,AArgs).


%= 	 	 

%% discoverAndCorrectArgsIsa( ?Op, ?Prop, ?VALUE3, ?ArgsIn, ?ArgsOut) is semidet.
%
% discover and correct Arguments  (isa/2).
%
discoverAndCorrectArgsIsa(Op,Prop,_,ArgsIn,ArgsOut):- length(ArgsIn,ArgUsed), /*show_failure*/ (
 (arity_no_bc(Prop,MaxArity),(number(ArgUsed),number(MaxArity),ArgUsed=<MaxArity))),
    discoverAndCorrectArgsIsa_from_right(Op,Prop,MaxArity,ArgsIn,ArgsOut),!.
discoverAndCorrectArgsIsa(Op,Prop,N,ArgsIn,ArgsOut):-discoverAndCorrectArgsIsa_from_left(Op,Prop,N,ArgsIn,ArgsOut),!.


%= 	 	 

%% discoverAndCorrectArgsIsa_from_right( ?Op, ?Prop, ?N1, ?In, ?Out) is semidet.
%
% discover and correct Arguments  (isa/2) Converted From right.
%
discoverAndCorrectArgsIsa_from_right(_O,_Prop,_N1,[],[]):-!.
discoverAndCorrectArgsIsa_from_right(Op,Prop,N1,In,Out):- append(Args,[A],In),
   ((argIsa_op_call(Op,Prop,N1,Type),must(correctAnyType(Op,A,Type,AA)))-> true ; A=AA),!,
   N2 is N1-1,
   discoverAndCorrectArgsIsa_from_right(Op,Prop,N2,Args,AArgs),
   ignore(AA=A),
   append(AArgs,[AA],Out).



%% discoverAndCorrectArgsIsa_from_left( ?O, ?Prop, ?N1, :TermARG4, ?VALUE5) is semidet.
%
% discover and correct Arguments  (isa/2) Converted From left.
%
discoverAndCorrectArgsIsa_from_left(_O,_Prop,_N1,[],[]):-!.
discoverAndCorrectArgsIsa_from_left(Op,Prop,N1,[A|Args],Out):-
   ((argIsa_op_call(Op,Prop,N1,Type),must(correctAnyType(Op,A,Type,AA)))->true;A=AA),!,
   N2 is N1+1,
   ignore(AA=A),
   discoverAndCorrectArgsIsa_from_left(Op,Prop,N2,Args,AArgs),
    Out = [AA|AArgs].




%= 	 	 

%% is_ephemeral( :TermVar) is semidet.
%
% If Is A Ephemeral.
%
is_ephemeral(Var):-var(Var),!,fail.
is_ephemeral(isMissing).
is_ephemeral(isOptional(_,_)).
is_ephemeral(isRandom(_)).
is_ephemeral(isOneOf(_)).

:- was_export(correctAnyType/4).


%= 	 	 

%% is_valuespec( ?G) is semidet.
%
% If Is A Valuespec.
%
is_valuespec(G):-var(G),!,fail.
is_valuespec(G):-is_ephemeral(G).
is_valuespec(G):-a(tCol,G).
is_valuespec(FT):-a(ttExpressionType,FT).
is_valuespec(G):-evaluatableArg(G,_).


%= 	 	 

%% evaluatableArg( ?AA, ?VALUE2) is semidet.
%
% Evaluatable Argument.
%
evaluatableArg(AA,_Value):-fail,sanity(nonvar(AA)),compound(AA),get_functor(AA,F),!,evaluatableFunctor(F).

%= 	 	 

%% evaluatableFunctor( ?VALUE1) is semidet.
%
% Evaluatable Functor.
%
evaluatableFunctor(isRandom).
evaluatableFunctor(isOptional).


%= 	 	 

%% correctAnyType( ?VALUE1, ?A, ?VALUE3, ?A) is semidet.
%
% Correct Any Type.
%

% ?- correctAnyType(query(must,t),ab_c,ftString,O).
% ?- correctType(query(must,t),ab_c,ftString,O).
% correctAnyType(_,A,_,A):- bad_idea.

correctAnyType(_, A,_Type,AA):- is_ftVar(A),sanity(var(AA)),A=AA,must_det(A==AA),!.
correctAnyType(Op,A,Type,AA):-  var(Type),!,trace_or_throw(var_correctAnyType(Op,A,Type,AA)).
% correctAnyType(Op,A,Type,AA):-  var(A),!,must(correctType(Op,A,Type,AA)),sanity(var(AA)),sanity(A==AA).
% correctAnyType(_, A,Type,AA):-  evaluatableArg(Type,A)->dmsg(evaluatableArg(A,Type))->must_det(A=AA),!.
correctAnyType(Op,A,Type,AA):- var(Type),trace_or_throw(correctAnyType(Op,A,Type,AA)).
% TODO snags on new tpyes correctAnyType(Op,A,Type,AA):- correctType(Op,A,Type,AA),nonvar(AA),!.
correctAnyType(Op,A,Type,AA):- correctType(Op,A,Type,AA),!,sanity(nonvar(AA)).
correctAnyType(Op,A,Type,AA):- must(A=AA),atom(Type),atom(A),!,nop(dmsg(dtrace(warn(not(correctAnyType(op(Op),arg(A),type(Type))))))).
correctAnyType(Op,A,Type,AA):- must(A=AA),dmsg(dtrace(nop(warn(not(correctAnyType(op(Op),arg(A),type(Type))))))).



%  @set mudMoveDist 4

:- was_export(correctFormatType/4).

%= 	 	 

%% correctFormatType( ?Op, ?A, ?Type, ?AA) is semidet.
%
% Correct Format Type.
%
correctFormatType(Op,A,Type,AA):- var(A),correctType(Op,A,Type,AA),sanity(var(AA)),must_det(A==AA),!,ignore(A=AA).
correctFormatType(Op,A,Type,AA):- var(Type),trace_or_throw(correctFormatType(Op,A,Type,AA)).
correctFormatType(Op,A,Type,AA):- correctType(Op,A,Type,AA),sanity(nonvar(AA)),!.
correctFormatType(Op,A,Type,AA):- tracing, correctType(Op,A,Type,AA).
correctFormatType(Op,A,Type,A):- dmsg(todo(not(correctFormatType(Op,A,Type)))),fail.

:- was_export(checkAnyType/4).


%= 	 	 

%% checkAnyType( ?Op, ?A, ?Type, ?AA) is semidet.
%
% Check Any Type.
%
checkAnyType(Op,A,Type,A):- var(A),correctType(Op,A,Type,AA),!,sanity(var(AA)),(A==AA).
checkAnyType(Op,A,Type,A):- correctType(Op,A,Type,AA),nonvar(AA),!,(AA=@=A).


%= 	 	 

%% correctAnyTypeOrFail( ?Op, ?A, ?Type, ?AA) is semidet.
%
% Correct Any Type Or Fail.
%
correctAnyTypeOrFail(Op,A,Type,AA):- locally(tlbugger:skipMust,correctType(Op,A,Type,AA)),A\=@=AA.



:- thread_local t_l:can_coerce/1.

%= 	 	 

%% correctType_gripe( ?Op, ?A, ?Fmt, ?AA) is semidet.
%
% Correct Type Gripe.
%
correctType_gripe(Op,A,Fmt,AA):- a(ttExpressionType,Fmt),!,trace_or_throw(correctType(is_ft_correctFormatType(Op,A,Fmt,AA))).
correctType_gripe(Op,A,Type,AA):- fail,atom(Type),must_equals(A,AA),
      dmsg(todo(isa_assert_type(Type))),
      % decl_type(Type),
      t_l:can_coerce(Op),
      dmsg(warning(ain(isa(A,Type)))),
      dtrace(ain(isa(A,Type))),!.

correctType_gripe(Op,A,C,A):-sanity(ground(A)),dmsg(todo(define(correctType(Op,A,C,'ConvertedArg')))),throw(retry(_)).
correctType_gripe(Op,A,Type,NewArg):-trace_or_throw(failure(correctType(Op,A,Type,NewArg))).

:- style_check(+singleton).


%= 	 	 

%% is_renamed_to( ?A, ?AA) is semidet.
%
% If Is A Renamed Converted To.
%
is_renamed_to(A,AA):- fail,atomic(A),not(A=[];A='';A=""),not(atom_concat(_,'Table',A)),not(atom_concat(_,'table',A)),
    atom_concat(Base,'able',A),atom_length(Base,AL),AL>2,!,atom_concat(Base,'Able',AA).


%= 	 	 

%% correctType( ?Op, ?A, ?Type, ?AA) is semidet.
%
% Correct Type.
%
correctType(Op,A,Type,AA):- sanity(nonvar(Type)), loop_check(correctType0(Op,A,Type,AA)).



is_call_like(ftCallable).
is_call_like(ftAskable).
is_call_like(ftAssertable).

is_uncheckable(X):-is_call_like(X).
is_uncheckable(tCol).
is_uncheckable(tSet).
is_uncheckable(ftProlog).
is_uncheckable(ftTerm).

ensure_never_ft_binding(C):- ignore((var(C),\+ attvar(C),freeze(C, \+ (atom(C),atom_concat('ft',_,C))))).


%% correctType0( ?Op, :TermA, :TermType, :TermAA) is semidet.
%
% Correct Type Primary Helper.
%
correctType0(change(_,_),A,T,AA):- A==T,!,must_equals(A,AA).
correctType0(_ ,A,T,AA):- A==T,!,must_equals(A,AA).
correctType0(Op,A,Type,AA):- var(Type),trace_or_throw(correctType(Op,A,Type,AA)).
correctType0(_,A,_,AA):- var(A),!, must_equals(A,AA).

correctType0(_ ,A,FTType,AA):- quotedIsa(A,FTType),!, must_equals(A,AA).
correctType0(_ ,A,ftString,AA):- !, must(any_to_string(A,AA)),!.
correctType0(_ ,A,ftText,AA):- atomic(A),convert_to_cycString(A,AA),!.
correctType0(_ ,A,ftText,AA):- any_to_string(A,M),convert_to_cycString(M,AA),!.
correctType0(Op,A,ftTerm(_),AA):- loop_check(must_equals_correct(Op,A,AA),
                        ((A=AA,dmsg(looped_on(correctType0(Op,A,ftTerm(_),AA)))))).
correctType0(_ ,A,FTType,A):- isa(A,FTType),!.
correctType0(_ ,String,ftNumber,Number):- string(String),!, any_to_number(String,Number).


correctType0(_ ,A,Type,AA):- is_uncheckable(Type), must_equals(A,AA).

correctType0(Op,A,Type,AA):-  is_call_like(Type),!,must_equals_correct(query(Type,Op),A,AA).
correctType0(Op,A,ftID,AA):- must_equals_correct(query(ftID,Op),A,AA),!.

correctType0(_ ,A,tCol,AA):- atom(A),!,must_equals(A,AA).

correctType0(_ ,A,Type,AA):-A==Type,!,A=AA.

correctType0(query(ftID,Op),A,ftAction,AA):- must_equals_correct(Op,A,AA),!.
correctType0(Op,A,Type,AAA):-is_renamed_to(A,AA),!,must(correctType(Op,AA,Type,AAA)).
correctType0(Op,+A,Type,+AA):-nonvar(A),!,correctType(Op,A,Type,AA).
correctType0(Op,-A,Type,-AA):-nonvar(A),!,correctType(Op,A,Type,AA).
correctType0(_ ,A,ftInt,AA):- any_to_number(A,AA).
correctType0(_ ,A,ftNumber,AA):- any_to_number(A,AA).
correctType0(_ ,A,ftVoprop,AA):- !, must(A=AA).
correctType0(Op,A,ftVoprop,AA):- is_list(A),!,maplist(correctTypeArg(Op,ftAskable),A,AA).
correctType0(Op,A,ftVoprop,AA):- !,locally(t_l:inVoprop,correctType(Op,A,ftAskable,AA)).

correctType0(_ ,A,tPred,AA):- any_to_relation(A,AA).
correctType0(_ ,A,tFunction,AA):- any_to_relation(A,AA).
correctType0(_ ,A,tRelation,AA):- any_to_relation(A,AA).
correctType0(_ ,A,ftAtom,AA):- any_to_atom(A,AA).
correctType0(change(_,_),A,tCol,AA):- atom(A),deduced_is_tCol(A),must_equals(A,AA).
correctType0(change(_,_),A,tCol,AA):- compound(A),deduced_is_tCol(A),must_equals(A,AA).
correctType0(_ ,A,vtVerb,AA):- must_equals(A,AA).
correctType0(_ ,A,Type,AA):- compound(A),( \+ is_list(A)),atom(Type),functor_safe(A,Type,_), must_equals(A,AA).
correctType0(_ ,A,Type,AA):- \+ (a(ttExpressionType,Type)),a(tCol,Type),isa_asserted(A,Type),!,must_equals(A,AA).
correctType0(Op,A,Fmt,AA):- trans_subft(Fmt,Code),Fmt\=Code,correctType(Op,A,Code,AA).
correctType0(Op,A,Super,AA):- a(ttExpressionType,Super),call_u(genls(Sub,Super)),Sub\=Super,correctType(Op,A,Sub,AA).

correctType0(_ ,What,Type,NewThing):- call_u(coerce(What,Type,NewThing)),!.

correctType0(Op,[A|NIL],ftListFn(T),[L]):-NIL==[],!,correctAnyType(Op,A,T,L).
correctType0(Op,[A|AA],ftListFn(T),[L|LIST]):-!,correctAnyType(Op,A,T,L),correctType0(Op,AA,ftListFn(T),LIST).

correctType0(Op,A,ftListFn(T),[OT]):-!,correctAnyType(Op,A,T,OT).
correctType0(_ ,[],[],[]):-!.
correctType0(Op,[H|T],[H2|T2],[H3|T3]):-!, correctAnyType(Op,H,H2,H3),correctType(Op,T,T2,T3).

correctType0(_,A,_,_):- \+ (compound(A)),!,fail.



correctType0(_ ,A,ftListFn(_),AA):- A == [],!,A=AA.
correctType0(Op,[A|AA],ftListFn(T),[L|LIST]):-!, correctType(Op,A,T,L), correctType(Op,AA,ftListFn(T),LIST).
correctType0(Op,A,ftListFn(T),[OT]):-!,correctAnyType(Op,A,T,OT).
correctType0(_ ,A,same(T),AA):-must_equals(T,AA),must_equals(A,AA).
correctType0(Op,A,isOneOf(List),AA):-!,member(Type,List),correctType(Op,A,Type,AA).


correctType0(Op,A,'&'(Type1,Type2),AA):-var(Type2),!,correctType(Op,A,Type1,AA).
correctType0(Op,A,'&'(Type1,Type2),AAA):-!,correctType(Op,A,Type1,AA),correctType(Op,AA,Type2,AAA).

correctType0(_ ,Obj,argIsaFn(_Prop,N),AA):-must_equals(Obj,AA),
   ignore((t_l:deduceArgTypes(_),
     sanity(N\=0),
      findall(OT,call_u(isa_asserted(Obj,OT)),_OType),
         ! /* must(nop(deduce_argN(Prop,N,Obj,OType,argIsaFn(Prop,N)))) */ )),!.


correctType0(_ ,A,Type,AA):- contains_var(Type,isThis),
   subst(Type,isThis,A,Call1),subst(Call1,value,AA,Call2),!,
      show_call(why,(Call2)),ignore(AA=A).

correctType0(_ ,A,Type,AA):- functor(Type,F,A),
   (A2 is A+2,current_predicate(F/A2)->show_call(why,clause_u(t(Type,A,AA)));
   (A1 is A+1,current_predicate(F/A1)->show_call(why,clause_u(t(Type,A))));
   fail),ignore(AA=A).

%TODO Confirm vtDirection is coerce/3'd correctType(_ ,A,vtDirection,AA):- call((current_predicate(any_to_dir/2),!,call(any_to_dir,A,AA))),!.
%TODO Confirm vtDirection is coerce/3'd correctType(_ ,A,vtDirection,AA):- must_equals(A,AA).

correctType0(query(HLDS,Must),A,xyzFn(Region, ftInt, ftInt, ftInt),xyzFn(AA, _, _, _)):-atom(A),correctAnyType(query(HLDS,Must),A,Region,AA).

correctType0(_ ,[],ftFormFn([]),[]):-!.
correctType0(Op,[H|T],ftFormFn([H2|T2]),[H3|T3]):-
   correctType(Op,H,H2,H3),
   correctType(Op,T,ftFormFn(T2),T3).

correctType0(Op,Args,ftFormFn(Types),NewArgs):- compound(Args),compound(Types),
   functor(Args,F,N),functor(Types,F,N),functor(NewArgs,F,N),
   Args=..[F|ArgsL],
   Types=..[F|TypesL],
   NewArgs=..[F|NewArgsL],!,   
   correctType(Op,ArgsL,TypesL,NewArgsL).

correctType0(Op,Args,Types,NewArgs):-compound(Args), 
   functor(Args,F,N),functor(Types,F,N),functor(NewArgs,F,N),
   Args=..[F|ArgsL],
   Types=..[F|TypesL],
   NewArgs=..[F|NewArgsL],
   correctAnyType(Op,ArgsL,TypesL,NewArgsL).

correctType0(Op,Arg,Props,NewArg):- 
   Props=..[F|TypesL],
   functor(Props,F,A),
   A2 is A+1,
   arity_no_bc(F,A2),
   C=..[F,Arg|TypesL],
   correctArgsIsa(Op,C,CC),
   CC=..[F,NewArg|_].

correctType0(Op,A,T,AAA):- once(correctArgsIsa(Op,A,AA)),A\=AA,!,correctType(Op,AA,T,AAA).
correctType0(_ ,A,T,AA):- get_functor(A,F),clause_b(resultIsa(F,T)),must_det(A=AA),!.
correctType0(_ ,A,T,AA):- get_functor(A,F),clause_b(formatted_resultIsa(F,T)),must_det(A=AA),!.



%= 	 	 

%% correctTypeArg( ?Op, ?Type, ?A, ?AA) is semidet.
%
% Correct Type Argument.
%
correctTypeArg(Op,Type,A,AA):-correctType(Op,A,Type,AA).


%= 	 	 

%% must_equals_correct( ?Op, ?A, ?AA) is semidet.
%
% Must Be Successfull Equals Correct.
%
must_equals_correct(Op,A,AA):-must(correctArgsIsa(Op,A,AA)).

% :- style_check(+singleton).


%= 	 	 

%% must_equals( ?A, ?AA) is semidet.
%
% Must Be Successfull Equals.
%
must_equals(A,AA):-must_det(A=AA).


%= 	 	 

%% deduced_is_tCol( ?VALUE1) is semidet.
%
% Deduced If Is A True Structure Col.
%
deduced_is_tCol(A):- (t_l:infSkipArgIsa->true; (a(tCol,A)->true;(fail,ain(isa(A,tCol))))),!.
:- style_check(+singleton).


%= 	 	 

:- export(correctArgsIsa/3).


:- was_export(atom_to_value/2).

%= 	 	 



%= 	 	 

%% any_to_relation( ?A, ?F) is det.
%
% Any Converted To Relation.
%
any_to_relation(A,F):-atomic(A),!,any_to_atom(A,F).
any_to_relation(A,F):-functor_h(A,F),!.


%= 	 	 

%% roll_dice( ?Rolls, ?VALUE2, ?Bonus, ?Result) is semidet.
%
% Roll Dice.
%
roll_dice(Rolls,_,Bonus,Result):- Rolls < 0, !, Result is Bonus.
roll_dice(Rolls,Sided,Bonus,Result):- LessRolls is Rolls-1, roll_dice(LessRolls,Sided, Bonus + random(Sided) +1, Result).

% call_argIsa_ForAssert(F,N,Type):-argIsa(F,N,Type),atom(Type),!,not(nonusefull_deduction_type(Type)),tCol(Type).

:- fixup_exports.

mpred_type_args_file.


