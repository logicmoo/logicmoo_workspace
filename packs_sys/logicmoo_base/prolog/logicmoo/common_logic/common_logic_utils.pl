

% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/common_logic/common_logic_boxlog.pl
%:- if(( ( \+ ((current_prolog_flag(logicmoo_include,Call),Call))) )).
:- module(common_logic_utils,
          [ ]).

:- include(library('logicmoo/common_logic/common_header.pi')).
%:- endif.
%% delistify_last_arg( ?Arg, :PredMiddleArgs, ?Last) is det.
%
% Delistify Last Argument.
%

delistify_last_arg(Arg,Pred,Last):- no_repeats(Last,must(delistify_last_arg0(Arg,Pred,Last))).

delistify_last_arg0(Arg,Pred,Last):- is_list(Arg),!,member(E,Arg),must(delistify_last_arg0(E,Pred,Last)).
delistify_last_arg0(Arg,M:Pred,Last):- Pred=..[F|ARGS],append([Arg|ARGS],[NEW],NARGS),NEWCALL=..[F|NARGS],quietly(M:NEWCALL),!,member_ele(NEW,Last).
delistify_last_arg0(Arg,Pred,Last):- Pred=..[F|ARGS],append([Arg|ARGS],[NEW],NARGS),NEWCALL=..[F|NARGS],quietly(NEWCALL),!,member_ele(NEW,Last).



%% is_kif_clause( ?Var) is det.
%
% If Is A Knowledge Interchange Format Rule.
%
is_kif_clause(Var):- is_ftVar(Var),!,fail.
is_kif_clause(R):- kif_hook(R),!.
is_kif_clause(R):- is_clif(R),!.




%% kif_hook(+TermC) is det.
%
% Knowledge Interchange Format Hook.
%
kif_hook(C):- not_ftCompound(C),!,fail.
kif_hook(_H :- _):-  !,fail.
kif_hook(_H <- _):-  !,fail.
kif_hook(_H --> _):- !,fail.
kif_hook(_ ==> _):-  !,fail.
kif_hook(_ <==> _):- !,fail.
% uncommenting these next 3 lines may break sanity_birdt test

 kif_hook(  ~(H)):- !,nonvar(H),kif_hook(H).
 kif_hook(  \+ H):- !,nonvar(H),kif_hook(H).
 kif_hook(not(H)):- !,nonvar(H),kif_hook(H).

kif_hook( naf(H)):- !,nonvar(H),kif_hook(H).
kif_hook(In):- kif_hook_skel(In).
kif_hook(C):- callable(C),functor(C,F,A),kif_hook(C,F,A).

kif_hook(_,F,_):- atom_concat('sk',_,F),atom_concat(_,'Fn',F),!.
kif_hook(C,_,_):- leave_as_is(C),!,fail.
kif_hook(C,F,_):- is_sentence_functor(F),!,arg(_,C,E),kif_hook(E).

:- fixup_exports.

%% kif_hook_skel(+TermC) is det.
%
% Knowledge Interchange Format Hook Skelecton.
%

kif_hook_skel(forAll(_,_)).
kif_hook_skel(=>(_,_)).
kif_hook_skel(<=(_,_)).
kif_hook_skel(<=>(_,_)).
kif_hook_skel(&(_,_)).
kif_hook_skel((_ /\ _)).
kif_hook_skel((_ \/ _)).
kif_hook_skel(v(_ , _)).
kif_hook_skel(nesc(_)).
kif_hook_skel(poss(_)).
kif_hook_skel(cir(_)).
kif_hook_skel(all(_,_)).
kif_hook_skel(exactly(_,_,_)).
kif_hook_skel(atmost(_,_,_)).
kif_hook_skel(atleast(_,_,_)).
kif_hook_skel(quant(_,_,_)).
kif_hook_skel(exists(_,_)).
kif_hook_skel(if(_,_)).
kif_hook_skel(iff(_,_)).
kif_hook_skel(equiv(_,_)).
kif_hook_skel(implies(_,_)).
kif_hook_skel(CLIF):- is_clif(CLIF).
kif_hook_skel( ~(H)):- loop_check(kif_hook(H)).
kif_hook_skel( not(H)):- loop_check(kif_hook(H)).
kif_hook_skel( Compound):- arg(_,v(poss,nesc,until,always,release,cir),F),between(2,3,A),functor( Compound,F,A).
kif_hook_skel( Compound):- compound( Compound),!,functor(Compound,F,_),arg(_,v(and,or,xor),F).
kif_hook_skel( Compound):- var(Compound),!,arg(_,v(and,or,xor),F),between(1,12,A),functor( Compound,F,A).



:- fixup_exports.

