% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/mpred_type_wff.pl

:- module(mpred_type_wff, []).     

%:- include('mpred_header.pi').

/** <module> mpred_type_wff
% Provides a common set of operators in translation between the several logical languages
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/

:- meta_predicate 
        call_last_is_var(0).


:- set_how_virtualize_file(bodies).

:- kb_shared(functionCorrespondingPredicate/3).



is_pfc_clause(B):- mpred_term_expansion(B,Out),!, Out= ((:- cl_assert(pfc(_),_))).

is_prolog_clause(B):- compound(B),functor(B,F,_),arg(_,v((:-)),F).


%% subst_except( :TermSUB, ?Var, ?VarS, :TermSUB) is semidet.
%
% Subst Except.
%
subst_except( SUB, Var, VarS,SUB ) :- Var==VarS,!.
subst_except(  Var, VarS,SUB,SUB ) :- Var==VarS,!.
subst_except(  Var, _,_,Var ) :- \+ compound(Var),!.
subst_except(  Var, _,_,Var ) :- leave_as_is(Var),!.
subst_except([H|T],B,A,[HH|TT]):- !,subst_except(H,B,A,HH),subst_except(T,B,A,TT).
subst_except(HT,B,A,HHTT):- HT=..FARGS,subst_except(FARGS,B,A,[FM|MARGS]),
   (atom(FM)->HHTT=..[FM|MARGS];append_termlist(FM,MARGS,HHTT)).



:- thread_local t_l:override_hilog/1.




%% current_hilog( ?Dbase_t) is semidet.
%
% Current Hilog.
%
current_hilog(Dbase_t):- t_l:override_hilog(Dbase_t),!.
current_hilog(t).


% ===================================================================
% EXPORTS
% ===================================================================



%% isNonVar( ?Denotation) is semidet.
%
% If Is A Not Variable.
%
isNonVar(Denotation):-not(isSlot(Denotation)).

% ===============================================================================================
% ===============================================================================================





%% isSlot( ?Denotation, ?Denotation) is semidet.
%
% If Is A Slot.
%
isSlot(Denotation,Denotation):- isVarProlog(Denotation),!.
isSlot(Denotation,PrologVar):- isVarObject(Denotation,PrologVar),!.

% ===============================================================================================
% ===============================================================================================




%% isHiddenSlot( ?Term) is semidet.
%
% If Is A Hidden Slot.
%
isHiddenSlot(_Term):-fail.

% ===============================================================================================
% ===============================================================================================




%% isVarProlog( ?A) is semidet.
%
% If Is A Variable Prolog.
%
isVarProlog(A):-((var(A);A='$VAR'(_))).

% ===============================================================================================
% ===============================================================================================




%% isVarObject( ?Denotation) is semidet.
%
% If Is A Variable Object.
%
isVarObject(Denotation):-((
		  isObject(Denotation,_BaseType),
		  arg(1,Denotation,Value),!,isSlot(Value))).




%% isVarObject( ?Denotation, ?Value) is semidet.
%
% If Is A Variable Object.
%
isVarObject(Denotation,Value):-((
		  isObject(Denotation,_BaseType),
		  arg(1,Denotation,Value),!,isSlot(Value))).

% ===============================================================================================
% ===============================================================================================
	



%% isObject( ?Denotation, ?BaseType) is semidet.
%
% If Is A Object.
%
isObject(Denotation,BaseType):-
	(((atom(BaseType) ->
		  (atom_concat('$',BaseType,F),functor(Denotation,F,2));
		  (functor(Denotation,F,2),atom_concat('$',BaseType,F))
		 ),!)).

% ===============================================================================================
% ===============================================================================================




%% isQualifiableAsClass( :TermAtom) is semidet.
%
% If Is A Qualifiable Converted To Class.
%
isQualifiableAsClass(Atom):-atom(Atom),!.
isQualifiableAsClass('$Class'(Atom,_)):-atom(Atom),!.




%% isQualifiableAs( ?Denotation, ?BaseType, ?Value) is semidet.
%
% If Is A Qualifiable Converted To.
%
isQualifiableAs(Denotation,BaseType,Value):-
		  isObject(Denotation,BaseType),
		  arg(1,Denotation,Value).

% ===============================================================================================
% ===============================================================================================




%% isQualifiedAs( ?Denotation, ?VALUE2, ?VALUE3) is semidet.
%
% If Is A Qualified Converted To.
%
isQualifiedAs(Denotation,_,_):-not(compound(Denotation)),!,fail.
isQualifiedAs(Denotation,BaseType,Value):-
		  isQualifiedAs(Denotation,BaseType,Value,_SubType).



%% isQualifiedAs( ?Denotation, ?BaseType, ?Value, ?SubType) is semidet.
%
% If Is A Qualified Converted To.
%
isQualifiedAs(Denotation,BaseType,Value,SubType):-
		  isObject(Denotation,BaseType),
		  arg(1,Denotation,Value),
		  arg(2,Denotation,List),
		  lastImproperMember(BaseType,SubType,List).

% ===============================================================================================
% ===============================================================================================
:- style_check(-singleton).




%% lastImproperMember( ?Default, ?Default, :TermList) is semidet.
%
% Last Improper Member.
%
lastImproperMember(Default,Default,List):-isVarProlog(List),!.
lastImproperMember(Default,Default,[]):-!.
lastImproperMember(Default,SubType,List):-proper_list(List),last(SubType,List).
lastImproperMember(Default,SubType,[SubType|End]):-isVarProlog(End),!.
lastImproperMember(Default,SubType,[_|Rest]):-
	lastImproperMember(Default,SubType,Rest),!.
	
% ===============================================================================================
% ===============================================================================================




%% isQualifiedAndKnownAs( ?Denotation, ?BaseType, ?Value) is semidet.
%
% If Is A Qualified And Known Converted To.
%
isQualifiedAndKnownAs(Denotation,BaseType,Value):-
		  isQualifiedAs(Denotation,BaseType,Value),!,
		  not(isVarProlog(Value)).

% ===============================================================================================
% ===============================================================================================




%% isQualifiedAndVarAs( ?Denotation, ?BaseType, ?Value) is semidet.
%
% If Is A Qualified And Variable Converted To.
%
isQualifiedAndVarAs(Denotation,BaseType,Value):-
		  isQualifiedAs(Denotation,BaseType,Value),!,
		  isVarProlog(Value).

% ===============================================================================================
% ===============================================================================================




%% isQualifiedAndVarAndUnifiable( ?Denotation, ?BaseType, ?NValue) is semidet.
%
% If Is A Qualified And Variable And Unifiable.
%
isQualifiedAndVarAndUnifiable(Denotation,BaseType,NValue):-
		  isQualifiedAs(Denotation,BaseType,Value),!,
		  (isVarProlog(Value);
		  (\+ \+ NValue=Value)),!.

% ===============================================================================================
% ===============================================================================================
/*
:- dynamic(isBodyConnective/1).



%% isBodyConnective( ?Funct) is semidet.
%
% If Is A Body Connective.
%
isBodyConnective(Funct):-atom_concat(_,'_',Funct),!.
isBodyConnective(Funct):-atom_concat('t~',_,Funct),!.
isBodyConnective(Funct):-atom_concat('f~',_,Funct),!.
isBodyConnective(Funct):-member(Funct,[and,or,until,',',';',':-',unless,xor,holdsDuring]). % Other Propositional Wrhtml_appers
*/



%% isEntityref( ?Var, ?Var) is semidet.
%
% If Is A Entityref.
%
isEntityref(Var,Var):-isSlot(Var),!.
isEntityref(Term,A):-Term=..[F,A,B],!,atom_concat('$',_,F),!.


% ===============================================================================================
% ===============================================================================================




%% isLiteralTerm( :TermA) is semidet.
%
% If Is A Literal Term.
%
isLiteralTerm(A):-isLiteralTerm_util(A),!.
isLiteralTerm(not(A)):-isLiteralTerm_util(A),!.




%% isLiteralTerm_util( ?A) is semidet.
%
% If Is A Literal Term Util.
%
isLiteralTerm_util(A):-var(A),!.
isLiteralTerm_util('$VAR'(_)):-!.
isLiteralTerm_util(string(_)):-!.
isLiteralTerm_util(A):-not(compound(A)),!.
isLiteralTerm_util(A):-string(A).

% ===============================================================================================
% ===============================================================================================




%% isEntitySlot( ?Term) is semidet.
%
% If Is A Entity Slot.
%
isEntitySlot(Term):-isSlot(Term),!.
isEntitySlot(Term):-not(compound(Term)),!.
isEntitySlot(Term):-isEntityFunction(Term,FnT,ArgsT),!.

% ===============================================================================================
% ===============================================================================================




%% isEntityFunction( ?Term, ?FnT, ?ArgsT) is semidet.
%
% If Is A Entity Function.
%
isEntityFunction(Term,FnT,ArgsT):-isSlot(Term),!,fail.
isEntityFunction(Term,FnT,ArgsT):-atomic(Term),!,fail.
isEntityFunction(Term,FnT,ArgsT):-Term=..[FnT|ArgsT],is_function(FnT).

% ===============================================================================================
% ===============================================================================================




%% isNonCompound( :TermVar) is semidet.
%
% If Is A Not Compound.
%
isNonCompound(Var):-isSlot(Var),!.
isNonCompound(Var):-not(compound(Var)),!.
isNonCompound(svar(_,_)):-!.
isNonCompound(Var):-is_ftText(Var),!.
isNonCompound(string(Var)):-!.

% ===============================================================================================
% ===============================================================================================




%% logical_functor_ft( ?F) is semidet.
%
% Logical Functor Format Type.
%
logical_functor_ft(F):-is_sentence_functor(F).
logical_functor_ft((':-')).
logical_functor_ft((',')).

% ===============================================================================================
% ===============================================================================================




%% non_assertable( :TermWW, ?WW) is semidet.
%
% Not Assertable.
%
non_assertable(WW,isVar(WW)):- var(WW),!.
non_assertable(_:WW,Why):- !,non_assertable(WW,Why).
non_assertable(W,notAssertable(F)):- compound(W),get_functor(W,F),cheaply_u(notAssertable(F)).
% non_assertable(WW,Why):- db_prop_add

% ===============================================================================================
% ===============================================================================================

:- was_export(logical_functor_pttp/1).




%% logical_functor_pttp( ?X) is semidet.
%
% Logical Functor Pttp.
%
logical_functor_pttp(X):-not(atom(X)),!,fail.
logical_functor_pttp(props):-!,fail.
logical_functor_pttp(X):-pttp_nnf_pre_clean_functor(A,B,_),(X==A;X==B),!.
logical_functor_pttp(&).
logical_functor_pttp(~).
logical_functor_pttp(<=>).
logical_functor_pttp(=>).
logical_functor_pttp(v).


%% is_quantifier( ?F) is semidet.
%
% If Is A Quantifier.
%
is_quantifier(F):- pttp_nnf_pre_clean_functor(F,(all),[]);pttp_nnf_pre_clean_functor(F,(ex),[]).



%% pttp_nnf_pre_clean_functor( ?A, ?A, ?VALUE3) is semidet.
%
% Pttp Negated Normal Form Pre Clean Functor.
%
pttp_nnf_pre_clean_functor('&',(,),[]).
pttp_nnf_pre_clean_functor('v',(;),[]).
pttp_nnf_pre_clean_functor(and,(,),[]).
pttp_nnf_pre_clean_functor(('/\\'), (,),[]).
pttp_nnf_pre_clean_functor(or,(;),[]).
pttp_nnf_pre_clean_functor(('\\/'),(;),[]).
% pttp_nnf_pre_clean_functor('::',(:),[]).
pttp_nnf_pre_clean_functor(~,(-),[]).
pttp_nnf_pre_clean_functor(not,(-),[]).
pttp_nnf_pre_clean_functor(~,(-),[]).
pttp_nnf_pre_clean_functor(implies,(=>),[]).
pttp_nnf_pre_clean_functor(imp,(=>),[]).
pttp_nnf_pre_clean_functor(equiv,(<=>),[]).
%pttp_nnf_pre_clean_functor(->,(=>),[]).
pttp_nnf_pre_clean_functor(entailed_from,(:-),[]).
pttp_nnf_pre_clean_functor(implied_by,(:-),[]).
pttp_nnf_pre_clean_functor(forAll,(all),[]).
pttp_nnf_pre_clean_functor(thereExists,(ex),[]).
pttp_nnf_pre_clean_functor(forall,(all),[]).
pttp_nnf_pre_clean_functor(exists,(ex),[]).
pttp_nnf_pre_clean_functor(A,A,[]):-atom(A).
pttp_nnf_pre_clean_functor(A,A,[]).




%% pttp_nnf_post_clean_functor( ?VALUE1, ?VALUE2) is semidet.
%
% Pttp Negated Normal Form Post Clean Functor.
%
pttp_nnf_post_clean_functor('&',',').
pttp_nnf_post_clean_functor('v',';').


% ===============================================================================================
% ===============================================================================================




%% is_neg( :TermARG1) is semidet.
%
% If Is A Negated.
%
is_neg(not(_)).



%% is_pos( ?One) is semidet.
%
% If Is A Pos.
%
is_pos(One):- get_functor(One,F),!,not(is_log_op(F)).

%= %= :- was_export(is_log_sent/1).



%% is_log_sent( ?S) is semidet.
%
% If Is A Log Sentence.
%
is_log_sent(S):- get_functor(S,F,_),is_log_op(F).




%% not_log_op( ?OP) is semidet.
%
% Not Log Oper..
%
not_log_op(OP):- not(is_log_op(OP)).
%= %= :- was_export(is_log_op/1).



%% is_log_op( ?OP) is semidet.
%
% If Is A Log Oper..
%
 % is_log_op(OP):- current_predicate(OP,M:P),current_predicate(G,meta_predicate(_)),!.
is_log_op(OP):- atomic(OP),if_defined(to_dlog_ops(OPS)),!,(member(OP=_,OPS);member(_=OP,OPS)).



%% quant_singles( ?Wff, ?VALUE2, :TermARG3, ?Wff) is semidet.
%
% Quanitify Single Vars
%
quant_singles(Wff,_,[],Wff).
quant_singles(Wff,Exists,[S|Singles],NewWff):-   
   (already_quantified(Wff,S) 
     -> WffM = Wff ; 
     add_1quantifier(Exists,S,Wff,WffM)),!,
   quant_singles(WffM,Exists,Singles,NewWff),!.

add_1quantifier(Exists,S,Wff,WffM) :- \+ compound(Wff),!,WffM =..[Exists,S,Wff].
add_1quantifier(Exists,S,Wff,WffM) :- Wff=..[Q,X,Fml],is_quant_f(Q),add_1quantifier(Exists,S,Fml,Wff1),WffM=..[Q,X,Wff1].
add_1quantifier(Exists,S,Wff,WffM) :- Wff=..[Q,N,X,Fml],is_quant_f(Q),add_1quantifier(Exists,S,Fml,Wff1),WffM=..[Q,N,X,Wff1].
add_1quantifier(Exists,S,Wff,WffM) :- WffM =..[Exists,S,Wff].


already_quantified(Wff,S) :- each_subterm(Wff,SubTerm),compound(SubTerm), already_quanted_01(SubTerm,S).

already_quanted_01(SubTerm,S):- SubTerm=..[OtherExists,SO,_],
   member(OtherExists,[all,exists]),!,
  (is_list(SO)->member(V,SO);V=SO),
   same_vars(V,S).

already_quanted_01(SubTerm,S):- 
    SubTerm=..[OtherExists,_,SO,_],
    member(OtherExists,[atleast,quant,atmost,exactly]),
    same_vars(SO,S).
    
is_quant_f(Q):- arg(_,v(atleast,atmost,exactly,all,exists),Q).
is_quant_f(Q):- arg(_,v(no,some,one,two,quant),Q).


%= %= :- was_export(defunctionalize/2).






/*


  defunctionalize( +FmlIn, -FmlOut, [options...]).


   converts terms like...

         loves(joe,mary)

   Into...

         poss(loves(joe,mary)) => nesc(loves(joe,mary)).

   settings are...

*/



%% defunctionalize( ?Wff, ?WffO) is det.
%
% Defunctionalize.
%

defunctionalize((H:-B),WffO):- nonvar(H),!,defunctionalize(':-',(H:-B),WffO),!.
defunctionalize(Wff,WffO):- defunctionalize('=>',Wff,WffO),!.
% defunctionalize(Wff,WffO):- locally_tl(dont_use_mudEquals,defunctionalize(',',Wff,WffO)).

defunctionalize_each(Wff,WffO):-must_maplist(defunctionalize,Wff,WffO).

%% defunctionalize( ?OP, ?Wff, ?WffO) is det.
%
% Defunctionalize.
%
defunctionalize(OP ,Wff,WffO):- defunctionalize_op(OP,Wff,WffM),!,WffO=WffM.


defunctionalize_op(_OP,WffO,WffO):- not_ftCompound(WffO),!.
defunctionalize_op(OP,(H:- B),WffO):- 
   quietly(defunctionalize_did(OP,B,PREB,POSTB,Function,NewVar)),
   subst(H,Function,NewVar,HH),
   occurrences_of_var(Function,B,Count),
   (HH\==H -> defunctionalize(OP,(HH:- (ignore(PREB),POSTB)),WffO);
   (Count>1 -> defunctionalize(OP,(H:- (POSTB , ignore(PREB))),WffO);
     defunctionalize(OP,(H:- (POSTB)),WffO))).
 

defunctionalize_op(OP,(H:- B),WffO):- 
   quietly(defunctionalize_did(OP,H,PREH,POSTH,Function,NewVar)),!,
   subst(B,Function,NewVar,BB),
   (BB==B -> 
     =((H:- (B)),WffO) ; 
     defunctionalize(OP,(POSTH:- (BB,PREH)),WffO)).

defunctionalize_op(OP,Wff,WffO):- 
  (defunctionalize_did(OP,Wff,PREWff,POSTWff,_Function,_NewVar)),!,
  WffM=..[OP,PREWff,POSTWff],
  defunctionalize_op(OP,WffM,WffO).
defunctionalize_op(_,Wff,Wff).


defunctionalize_did(OP,Wff,PredifiedFunction,NextWff,Function,NewVar):- 
  sub_term(SubTerm,Wff),
  compound(SubTerm),
  \+ is_precond_like(SubTerm),
  arg(N,SubTerm,Function), N<4,
  compound(Function),
  quietly(is_function_expr(OP,Function)),
  quietly(\+ has_function(OP,Function)),
  must(quietly((function_to_predicate(Function,NewVar,PredifiedFunction),
  subst(Wff,Function,NewVar,NextWff)))),!.



is_precond_like(Var):- \+ compound(Var),!.
is_precond_like(SubTerm):- is_ftEquality(SubTerm),!.
is_precond_like('$spft'(_MZ,_,_,_)):- !.
is_precond_like({_}).

has_function(OP,Term):- 
 compound(Term),
 \+ is_precond_like(Term),
 arg(_,Term,Function), 
 (is_function_expr(OP,Function);has_function(OP,Function)),!.


%% correct_negations( ?Op, :TermX, ?O) is semidet.
%
% Correct Negations.
%
correct_negations(Op,(~({X})),O):-nonvar(X),wrap_in_neg_functor(Op,X,O).
correct_negations(Op,(-({X})),O):-nonvar(X),wrap_in_neg_functor(Op,X,O).
correct_negations(Op,(not({X})),O):-nonvar(X),wrap_in_neg_functor(Op,X,O).
correct_negations(Op,(notz({X})),O):-nonvar(X),wrap_in_neg_functor(Op,X,O).
correct_negations(Op,(assertable_not({X})),O):-nonvar(X),wrap_in_neg_functor(Op,X,O).
correct_negations(Op,(\+({X})),O):-nonvar(X),wrap_in_neg_functor(Op,X,O).




%% wrap_in_neg_functor( ?VALUE1, ?X, ?X) is semidet.
%
% Wrap In Negated Functor.
%
wrap_in_neg_functor(clause,X,assertable_neg(X)).
wrap_in_neg_functor(mpred,X,not(X)).
wrap_in_neg_functor(callable,X, (\+(X))).






%% contains_no_negs( ?X) is semidet.
%
% Contains No Negateds.
%
contains_no_negs(X):- \+ contains_negs(X).




%% contains_negs( ?X) is semidet.
%
% Contains Negateds.
%
contains_negs(X):-sub_term(Sub, X),compound(Sub),Sub=not(_).





%% is_modal( ?MODAL, ?VALUE2) is semidet.
%
% If Is A Modal.
%
is_modal(MODAL,_):- \+ compound(MODAL),!,fail.
is_modal(MODAL,BDT):- (MODAL = nesc(BDT,_) ; MODAL = poss(BDT,_)),!,nop(nonvar(BDT)).
is_modal(MODAL,BDT):- (MODAL = nesc(BDT) ; MODAL = poss(BDT)),!.
is_modal(MODAL,BDT):- arg(_,MODAL,ARG),is_modal(ARG,BDT).


contains_modal(G):- is_modal(G,_),!.
contains_modal(G):- contains_modalization_pred(G),!.


%% contains_modalization_pred( ?MODAL, ?VALUE2) is semidet.
%
% Contains a modalizion predicate like possible_t or known_t
%
contains_modalization_pred(MODAL):- compound(MODAL),functor(MODAL,F,_),
  (atom_concat(_,'_t',F);atom_concat(_,'_f',F);(arg(_,MODAL,ARG),contains_modalization_pred(ARG))).




%% contains_var_lits( ?Fml, ?Var, ?Lits) is semidet.
%
% Contains Variable Literals.
%
contains_var_lits(Fml,Var,Lits):- findall(Lit,contains_t_var(Fml,Var,Lit),Lits).


%% same_var( ?Var, ?Fml) is semidet.
%
% Same Variable.
%
same_var(Var,Fml):- Var=@=Fml,!.



%% contains_type_lits( ?Fml, ?Var, ?Lits) is semidet.
%
% Contains Type Literals.
%
contains_type_lits(Fml,Var,Lits):- findall(T,(contains_t_var(Fml,Var,Lit),get_isa(Lit,O,T),same_var(O,Var)),Lits).



%% contains_t_var( ?Fml, ?Var, ?Term) is semidet.
%
% Contains True Structure Variable.
%
contains_t_var(Fml,Var,Term):- each_subterm(Fml,Term),compound(Term),arg(_,Term,O),same_var(O,Var).





%% get_isa( ?Lit, ?I, ?TT) is semidet.
%
% get  (isa/2).
%
get_isa(Lit,I,TT):- compound(Lit),get_isa0(Lit,I,TT).



%% get_isa0( ?IT, ?I, ?TT) is semidet.
%
% get  (isa/2) Primary Helper.
%
get_isa0(isa(I,T),I,TT):- guess_type_name(T,TT),!.
get_isa0(IT,I,TT):- IT=..[T,I],is_colection_name(IT,T,TT),!.




%% is_colection_name( ?IT, ?T, ?TT) is semidet.
%
% If Is A Colection Name.
%
is_colection_name(_,-,_):- !,fail.
is_colection_name(IT,T,TT):- atom(T),atom_length(T,TL),TL>2,not(atom_contains(T,'_')),not(predicate_property(IT,_)),guess_type_name(T,TT).




%% leave_as_is( :TermV) is semidet.
%
% Leave Converted To If Is A.
%
leave_as_is(V):- is_ftVar(V),!.
leave_as_is(V):- \+ compound(V),!.
leave_as_is(poss(_,_)):-!,fail.
leave_as_is((_,_)):-!,fail.
leave_as_is((_ :-_ )):-!,fail.
leave_as_is((_;_)):-!,fail.
leave_as_is((_/_)):-!,fail.
leave_as_is([_|_]):-!,fail.
leave_as_is(not(_)):-!,fail.
leave_as_is(~_):-!,fail.
leave_as_is(V):-loop_check(leave_as_is_db(V)),!.






%% leave_as_is_db( :TermP) is semidet.
%
% Leave Converted To If Is A Database.
%
leave_as_is_db('$VAR'(_)).
leave_as_is_db('aNARTFn'(_)).
leave_as_is_db('comment'(_,_)).

leave_as_is_db(infer_by(_)).
leave_as_is_db(b_d(_,_,_)).
leave_as_is_db(ct(_,_)).
% leave_as_is_db('tColOfCollectionSubsetFn'(_,_)).
leave_as_is_db(ignore(_)).
leave_as_is_db(isa(_,_)).
leave_as_is_db(P):-prequent(P).
leave_as_is_db(C):-get_functor(C,F),loop_check(leave_as_is_functor(F)).





%% leave_as_is_functor( ?Atom) is semidet.
%
% Leave Converted To If Is A Functor.
%
leave_as_is_functor(Atom):- \+ atom(Atom),!,fail.
leave_as_is_functor('TINYKB-ASSERTION').
leave_as_is_functor('skolem').
leave_as_is_functor('$VAR').
leave_as_is_functor('kbMark').
leave_as_is_functor('z_unused').
leave_as_is_functor('wid').
leave_as_is_functor('genlMt').
% leave_as_is_functor('{}').
leave_as_is_functor(F):-cheaply_u(rtArgsVerbatum(F)).
% leave_as_is_functor(F):-loop_check(cheaply_u(rtReformulatorDirectivePredicate(F))).




%% prequent( :TermG) is semidet.
%
% Prequent.
%
prequent(original(_)).
prequent(mudEquals(_,_)).
prequent(skolem(_,_,_)).
prequent(different(_,_)).
prequent(argInst(_,_,_)).
prequent(G):-functor(G,call_builtin,_).
prequent(G):-functor(G,call_u,_).
prequent(G):-functor(G,not_call_builtin,_).




%% kb_nlit( ?KB, ?Neg) is semidet.
%
% Knowledge Base Nlit.
%
kb_nlit(_KB,Neg):-member(Neg,[(not),(~),(-),(~)]).




%% set_is_lit( ?A) is semidet.
%
% Set If Is A Literal.
%
set_is_lit(A):-when(nonvar(A),\+ is_ftVar(A)),!.








%= %= :- was_export(term_slots/2).



%% term_slots( ?Term, ?Slots) is semidet.
%
% Term Slots.
%
% term_slots(Term,Slots):-term_singleslots(Term, [],NS, [],S),append(NS,S,Slots).


:- export(head_singletons/2).
%head_singletons(Pre,Post):-  !, quietly((\+ ignore(must( \+ head_singles0(Pre,Post))))).



%% head_singletons( ?Pre, ?Post) is semidet.
%
% Head Singletons.
%
head_singletons(Pre,Post):-   quietly((\+ ignore(show_failure(why, \+ head_singles0(Pre,Post))))),!,dumpST.
:- export(head_singles0/2).
:- export(head_singles01/2).
% TODO how to adderess head_singles0(true, if_missing(foob(_G754993), foob(a)))?
% also should fix  (head_singles0(true, nt( foob(_G764659),  (call_u(foob(_G764666)), foob(_G764666)\==foob(a)), rhs([foob(a)]))))).



%% head_singles0( ?Pre, :TermPost) is semidet.
%
% Head Singles Primary Helper.
%
head_singles0(Pre,Post):-is_ftVar(Post),!,head_singles01(Pre,Post).
head_singles0(_,Post):- \+ compound(Post),!,fail.
head_singles0(Pre,M:Post):-atom(M),!,head_singles0(Pre,Post).
head_singles0(Pre,[Post|More]):-nonvar(Post),!,head_singles0(Pre,Post),head_singles0((Pre,Post),More).
head_singles0(Pre,'if_missing'(_,Post)):-nonvar(Post),!,head_singles0((Pre,Pre2),Post).
head_singles0(Pre,'->'(Pre2,Post)):-nonvar(Post),!,head_singles0((Pre,Pre2),Post).
head_singles0(Pre,'/'(Post,Pre2)):-nonvar(Post),!,head_singles0((Pre,Pre2),Post).
head_singles0(Pre,rhs(Post)):- nonvar(Post),mpred_rule_hb(Post,Post2,Pre2), !,head_singles0((Pre,Pre2),Post2).
head_singles0(Pre,mdefault(Post)):- nonvar(Post),mpred_rule_hb(Post,Post2,Pre2), !,head_singles0((Pre,Pre2),Post2).
head_singles0(Pre,nt(_,Pre2,Pre3,Post)):-nonvar(Post),!,head_singles0((Pre,Pre2,Pre3),Post).
head_singles0(Pre,pt(_,Pre2,Post)):-nonvar(Post),!,head_singles0((Pre,Pre2),Post).
head_singles0(Pre,Post):- nonvar(Post),mpred_rule_hb(Post,Post2,Pre2),Post2\=@=Post,!,head_singles0((Pre,Pre2),Post2).
head_singles0(Pre,Post):-head_singles01(Pre,Post).




%% head_singles01( ?Pre, ?Post) is semidet.
%
% Head Singles Primary Helper Secondary Helper.
%
head_singles01(Pre,Post):-
  quietly((
    term_singleslots(Post,_,CSingles),
    term_slots(Pre,PreVars),!,
    subtract_eq(CSingles,PreVars,Bad),!,Bad\==[])).




%% get_kv( ?KV, ?X, ?Y) is semidet.
%
% Get Kv.
%
/*get_kv(X=Y,X,Y):- !.
get_kv(X-Y,X,Y):- !.
get_kv(KV,X,Y):- functor(KV,_,1),KV=..[X,Y],!.
get_kv(KV,X,Y):- arg(1,KV,X),arg(2,KV,Y),!.
*/




%% is_function( ?F) is semidet.
%
% If Is A Function.
%
is_function(F):- is_ftVar(F),!,fail.
is_function(F):- clause_b(tFunction(F)).
is_function(F):- \+ atom(F),!,fail.
is_function(uU).
is_function(F):- atom_concat('sk',_Was,F),!,fail.
is_function(F):- atom_concat(_Was,'Fn',F),!.

is_function_expr(OP,Function):- compound(Function),!,compound_name_arity(Function,F,A),is_function_pfa(OP,Function,F,A).

is_function_expr(Function):- is_function_expr(assert,Function).


has_ftVar(Body):-is_ftVar(Body),!.
has_ftVar(Body):-compound(Body),arg(_,Body,Arg),has_ftVar(Arg),!.

%% is_function_pfa(OP, ?VALUE1, ?F, ?VALUE3) is semidet.
%
% If Is A Function.
%
is_function_pfa(_,_,F,_):- \+ atom(F),!,fail.
is_function_pfa(_,_,'$spft',_):-!,fail.
is_function_pfa(_,_,'uSubLQuoteFn',_):- !,fail.
is_function_pfa(_,_,'xQuoteFn',_):- !,fail.
is_function_pfa(_,_,'uNARTFn',_):- !,fail.
is_function_pfa(_,Body,F,_):- has_ftVar(Body),is_function(F).
is_function_pfa(_,_,'tCol_CollectionSubsetFn',_).
is_function_pfa((:-),_,F,_):- atom_concat(_Was,'Fn',F),!.

% is_function_pfa(OP,P,_,_):- loop_check(leave_as_is(P)),!,fail.
% is_function_pfa(OP,_,F,_):- loop_check(is_log_op(F)),!,fail.
% is_function_pfa(OP,_,F,A):- A2 is A+1, current_predicate(F/A2), \+ current_predicate(F/A).

%:- ain(isa(I,C)<=(ttRelationType(C),baseKB:isa(I,C))).




%% is_ftEquality( :TermTerm) is semidet.
%
% If Is A Format Type Equality.
%
is_ftEquality(Term):- is_ftVar(Term),!,fail.
%is_ftEquality(Term):- get_pred(Term,Pred),is),!,(Pred==mudEquals;genlPreds(Pred,equals);clause_asserted(prologEquality(Pred))),!.
is_ftEquality(mudEquals(_,_)).
is_ftEquality(skolem(_,_,_)).
is_ftEquality(equals(_,_)).
is_ftEquality(termOfUnit(_,_)).


:- thread_local(t_l:dont_use_mudEquals/0).




:- export(mpred_functor/3).
mpred_functor(Pred,Pred,A):-var(Pred),!,between(1,9,A).
mpred_functor(F/A,F,A):-!,probably_arity(F,A).
mpred_functor(_:Pred,F,A):-!,mpred_functor(Pred,F,A).
mpred_functor(F,F,A):-atom(F),!,probably_arity(F,A).
mpred_functor(Pred,F,A):-functor_safe(Pred,F,A).

probably_arity(F,A):-(integer(A)->true;(arity(F,A)*->true;between(1,9,A))).





%% ensure_quantifiers( ?Wff, ?WffO) is semidet.
%
% Ensure Quantifiers.
%
ensure_quantifiers(Wff:- B,WffO):- B== true,!, ensure_quantifiers(Wff,WffO).
ensure_quantifiers(Wff:- B,Wff:- B):- !.
% ensure_quantifiers(Wff,Wff):-!.
ensure_quantifiers(WffI,WffO):-
 subst(WffI,forall,all,Wff),
 must_det_l((show_failure(why,term_singleslots(Wff,NonSingles,Singles)),
  quant_singles(Wff,'exists',Singles,WffM),quant_singles(WffM,'all',NonSingles,WffO))).





%% get_pred( ?Pred, ?F) is semidet.
%
% Get Predicate.
%
get_pred(Pred,F):- get_functor(Pred,F).





:- kb_shared(function_corisponding_predicate/2).


%% function_to_predicate( ?Function, ?NewVar, ?PredifiedFunction) is semidet.
%
% Function Converted To Predicate.
%

function_to_predicate(Function,NewVar,PredifiedFunction):- 
 Function = 'tColOfCollectionSubsetFn'(Col,'tSetOfTheSetOfFn'(NewVar,Formulas)), 
 must(is_ftVar(NewVar)), % \+ is_ftVar(Col),!,
 PredifiedFunction = '&'(isa(NewVar,Col) , Formulas).


function_to_predicate(Function,NewVar,PredifiedFunction):- 
  Function=.. [F|ARGS],
  cheaply_u(functionCorrespondingPredicate(F,P,N)),
  fresh_varname(Function,NewVar),
  list_insert_at([P|ARGS],NewVar,N,[PR|Edified]),
  (atom(PR) -> PredifiedFunction=..[PR|Edified] ; PredifiedFunction=..[t,PR|Edified]),!.


function_to_predicate(Function,NewVar,mudEquals(NewVar,Function)):- 
 % \+ t_l:dont_use_mudEquals, 
  fresh_varname(Function,NewVar),!.



list_insert_at([A],NewVar,_,[A,NewVar]):-!.  % will append it to the last if too deep
list_insert_at(List,NewVar,0,[NewVar|List]):- !.
list_insert_at([A|List],NewVar,1,[A,NewVar|List]):-!.
list_insert_at([A|List],NewVar,N,[A|NewList]):- N2 is N -1,
   list_insert_at(List,NewVar,N2,NewList).


%= 	 	 

%% fresh_varname( :TermF, ?NewVar) is semidet.
%
% Fresh Varname.
%
fresh_varname(F,NewVar):-is_ftVar(F),NewVar=F.
fresh_varname(F,NewVar):-var(F),fresh_varname('mudEquals',NewVar).
fresh_varname([F0|_],NewVar):-!,fresh_varname(F0,NewVar).
fresh_varname(F,NewVar):- compound(F),arg(_,F,F1),atom(F1),!,functor(F,F0,_),atom_concat(F0,F1,FN),upcase_atom(FN,FUP),gensym(FUP,VARNAME),NewVar = '$VAR'(VARNAME),!.
fresh_varname(F,NewVar):- functor(F,FN,_),!, upcase_atom(FN,FUP),gensym(FUP,VARNAME),NewVar = '$VAR'(VARNAME),!.

:- fixup_exports.

