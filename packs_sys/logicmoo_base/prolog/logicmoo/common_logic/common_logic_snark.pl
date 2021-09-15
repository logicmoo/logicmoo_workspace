% File: /opt/PrologMUD/pack/logicmoo_base/prolog/common_logic/snark/common_logic_snark.pl
% :- if(( ( \+ ((current_prolog_flag(logicmoo_include,Call),Call))) )).
:- module(common_logic_snark,
          [ 

   % op(300,fx,'-'),
   /*op(1150,xfx,'=>'),
   op(1150,xfx,'<=>'),
   op(350,xfx,'xor'),
   op(400,yfx,'&'),
   op(500,yfx,'v')*/
   % if/2,iif/2,   
            is_not_entailed/1]).

/** <module> common_logic_snark
% Provides a specific compilation API for KIF axioms
%

% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
:- 
 op(1199,fx,('==>')),
 op(1190,xfx,('::::')),
 op(1180,xfx,('==>')),
 op(1170,xfx,('<==>')),
 op(1160,xfx,('<-')),
 op(1150,xfx,('=>')),
 op(1140,xfx,('<=')),
 op(1130,xfx,'<=>'),
 op(1120,xfx,'<->'),
% op(1100,fx,('nesc')),
 op(600,yfx,('&')),
 op(600,yfx,('v')),
 op(350,xfx,('xor')),
 %op(300,fx,('-')),
 op(300,fx,('~')).

:- meta_predicate call_l2r(2,?,?).


:- ensure_loaded(common_logic_utils).


:- use_module(library('logicmoo/common_logic/common_logic_boxlog.pl')).
:- use_module(library('logicmoo_pttp')).




:- include(library('logicmoo/common_logic/common_header.pi')).
%:- endif.

:-
            op(1150,fx,(was_dynamic)),
            op(1150,fx,(was_multifile)),
            op(1150,fy,(was_module_transparent)).

:- use_module(library(logicmoo_typesystem)).

:- set_how_virtualize_file(bodies).

:-  ((
 op(1199,fx,('==>')), 
 op(1190,xfx,('::::')),
 op(1180,xfx,('==>')),
 op(1170,xfx,'<==>'),  
 op(1160,xfx,('<-')),
 op(1150,xfx,'=>'),
 op(1140,xfx,'<='),
 op(1130,xfx,'<=>'), 
 op(600,yfx,'&'), 
 op(600,yfx,'v'),
 op(350,xfx,'xor'),
 op(300,fx,'~'),
 op(300,fx,'-'))).


:- module_transparent(( are_clauses_entailed/1,
            is_prolog_entailed/1)).

:- meta_predicate
   % common_logic_snark
   convertAndCall(*,0),
   kif_add_boxes3(2,?,*),
   % common_logic_snark
   ain_h(2,?,*),
   map_each_clause(1,+),
   map_each_clause(2,+,-),

   % common_logic_snark
   to_nonvars(2,?,?).

%:- meta_predicate '__aux_maplist/2_map_each_clause+1'(*,1).
%:- meta_predicate '__aux_maplist/3_map_each_clause+1'(*,*,2).

:- fixup_exports.

/*
:- was_dynamic
        baseKB:as_prolog_hook/2,
        elInverse/2,
        
        not_mudEquals/2.
        
*/

% :- dynamic(baseKB:(if/2,iif/2)).

:- export(kw_to_vars/2).
kw_to_vars(KW,VARS):-subsT_each(KW,[':ARG1'=_ARG1,':ARG2'=_ARG2,':ARG3'=_ARG3,':ARG4'=_ARG4,':ARG5'=_ARG5,':ARG6'=_ARG6],VARS).




%% is_gaf( ?Gaf) is det.
%
% If Is A Gaf.
%
is_gaf(Gaf):-when(nonvar(Gaf), \+ (is_kif_clause(Gaf))).

%= %= :- was_export(is_kif_clause/1).


%% are_clauses_entailed( :TermE) is det.
%
% Are Clauses Entailed.
%
% 
are_clauses_entailed(E):-var(E),!,fail.
are_clauses_entailed(B):- unnumbervars(B,A),map_each_clause(is_prolog_entailed,A).


is_pfc_entailed(B):- unnumbervars(B,A),map_each_clause(mpred_supported(local),A).


%% is_prolog_entailed( ?Prolog) is det.
%
% True if the "Prolog" clause is asserted
%

is_prolog_entailed(UCL):-clause_asserted_u(UCL),!.
is_prolog_entailed(UCL):-show_success(clause_asserted(UCL)),!.
is_prolog_entailed(UCL):-show_success(clause_asserted_i(UCL)),!.
is_prolog_entailed(UCL):-boxlog_to_pfc(UCL,PFC),show_failure(is_pfc_entailed(PFC)),!.
is_prolog_entailed(PFC):-show_failure(is_pfc_entailed(PFC)),!.
is_prolog_entailed(UCL):-clause(UCL,B),split_attrs(B,A,BB),must(A),BB.
is_prolog_entailed(UCL):-clause(UCL,B,Ref),(B\==true->must(B);(dtrace,clause(HH,BB,Ref),dmsg(BB:-(UCL,HH)))),!.
is_prolog_entailed(UCL):-dmsg(warn(not_is_prolog_entailed(UCL))),!,fail.



%% member_ele( ?E, ?E) is det.
%
% Member Ele.
%
member_ele(E,E):- is_ftVar(E),!.
member_ele([],_):-!,fail.
member_ele(E,E):- ( \+ (compound(E))),!.
member_ele([L|List],E):- must(is_list([L|List])),!,member(EE,[L|List]),member_ele(EE,E).
member_ele((H,T),E):- nonvar(H),nonvar(T),!, (member_ele(H,E);member_ele(T,E)).
member_ele(E,E).



% sanity that mpreds (manage prolog prodicate) are abily to transform

% cwc "code-wise chaining" is always true in Prolog but will throw programming error if evalled in LogicMOO Prover.
% Use this to mark code and not axiomatic prolog


map_each_clause(P,CLIF,Prolog):- is_list(CLIF),!,must_maplist_det(map_each_clause(P),CLIF,Prolog).
map_each_clause(P,(H,CLIF),(T,Prolog)):-  sanity(nonvar(H)),!,map_each_clause(P,H,T),map_each_clause(P,CLIF,Prolog).
map_each_clause(P,A,B):- call(P,A,B).

map_each_clause(P,CLIF):-  is_list(CLIF),!,must_maplist_det(map_each_clause(P),CLIF).
map_each_clause(P,(H,CLIF)):-  sanity(nonvar(H)),!,map_each_clause(P,H),map_each_clause(P,CLIF).
map_each_clause(P,A):- call(P,A).

%% any_to_pfc( :TermCLIF, ?Prolog) is det.
%
% Converted To Prolog.
%
any_to_pfc(B,A):-  sumo_to_pdkb(B,B0),must(map_each_clause(any_to_pfc0,B0,A)).

any_to_pfc0(B,A):-  is_kif_clause(B),!,maybe_notrace(kif_to_pfc0(B,A)).
any_to_pfc0(B,A):-  is_pfc_clause(B),!,maybe_notrace(fully_expand(clause(any_to_pfc,any_to_pfc),B,A)).
any_to_pfc0(B,A):-  is_prolog_clause(B),!,maybe_notrace(boxlog_to_pfc(B,A)).
any_to_pfc0(B,A):-  maybe_notrace(boxlog_to_pfc(B,A)),!.
% any_to_pfc0(B,A):-  \+ \+ lookup_u(B),!,A=B.
any_to_pfc0(B,A):-  !, trace_or_throw(should_never_be_here(any_to_pfc0(B,A))).
any_to_pfc0((H:-B),PrologO):- must((show_failure(why,boxlog_to_pfc((H:-B),Prolog)),!,=(Prolog,PrologO))),!.


%% kif_to_pfc( :TermCLIF, ?Prolog) is det.
%
% Ieee Standard Common Logic Interchange Format Version Converted To Prolog.
%
kif_to_pfc(B,A):-  must(map_each_clause(kif_to_pfc0,B,A)).
kif_to_pfc(K):-
  kif_to_pfc(K,A),
  show_boxlog(A).
 

kif_to_pfc0(CLIF,Prolog):- 
   sanity(is_kif_clause(CLIF)),
  % somehow integrate why_to_id(tell,Wff,Why),
     must_det_l((
      kif_to_boxlog(CLIF,BOXLOG),
      boxlog_to_pfc(BOXLOG,Prolog),
      (BOXLOG=@=Prolog -> true; (pfc_for_print_left(Prolog,PrintPFC),dmsg(-PrintPFC))))),!.
      


%% pfc_for_print_left( ?Prolog, ?PrintPFC) is det.
%
% Prolog Backward Chaining Print.
%
pfc_for_print_left(Prolog,PrintPFC):-is_list(Prolog),!,must_maplist_det(pfc_for_print_left,Prolog,PrintPFC).
%pfc_for_print_left(==>(P,if_missing(R,Q)),(Q :- (fwc, naf(R), P))):-!.
%pfc_for_print_left(if_missing(R,Q),(Q :- (fwc, naf(R)))):-!.
pfc_for_print_left(==>(P,Q),(Q:-fwc, P)):-!.
pfc_for_print_left(Prolog,PrintPFC):- =(Prolog,PrintPFC).

%% pfc_for_print_right( ?Prolog, ?PrintPFC) is det.
%
% Prolog Forward Chaining Print.
%
pfc_for_print_right(Prolog,PrintPFC):-is_list(Prolog),!,must_maplist_det(pfc_for_print_right,Prolog,PrintPFC).
pfc_for_print_right('<-'(Q,P),'->'(P, Q)):-!.
pfc_for_print_right(Prolog,PrintPFC):- =(Prolog,PrintPFC).



%% is_entailed_u( ?CLIF) is det.
%
% If Is A Entailed.
%   A good sanity Test for expected side-effect entailments
%   
%
is_entailed_u(CLIF):- 
 mpred_run,
 mpred_nochaining((
   must_det(any_to_pfc(CLIF,Prolog)),!, 
   \+ \+ are_clauses_entailed(Prolog))),!.


%% is_not_entailed( ?CLIF) is det.
%
% If Is A Not Entailed.
%  A good sanity Test for required absence of specific side-effect entailments
%
is_not_entailed(CLIF):-  mpred_nochaining((kif_to_boxlog(CLIF,Prolog), 
  \+ are_clauses_entailed(Prolog))).

:- op(1190,xfx,(:-)).
:- op(1200,fy,(is_entailed_u)).

% this defines a recogniser for clif syntax (well stuff that might be safe to send in thru kif_to_boxlog)



%% is_clif( :TermCLIF) is det.
%
% True if an expression is in ISO Common Logic Interchange Format.
%
is_clif(all(_,X)):-compound(X),!.
is_clif(forall(_,X)):-compound(X),!,is_clif(X).
is_clif(CLIF):-
  VVs = v(if,iff,clif_forall,all,exists,poss,nesc), % but not: implies,equiv,forall
   (var(CLIF)-> (arg(_,VVs,F),functor(CLIF,F,2));
     compound(CLIF),functor(CLIF,F,2),arg(_,VVs,F)).


:- style_check(+singleton).

%:- endif.
%% delistify_last_arg( ?Arg, :PredMiddleArgs, ?Last) is det.
%
% Delistify Last Argument.
%
:- module_transparent(delistify_last_arg/3).

delistify_last_arg(Arg,Pred,Last):- no_repeats(Last,must(delistify_last_arg0(Arg,Pred,Last))).

delistify_last_arg0(Arg,Pred,Last):- is_list(Arg),!,member(E,Arg),must(delistify_last_arg0(E,Pred,Last)).
delistify_last_arg0(Arg,Pred,Last):- call(Pred,Arg,NEW),each_ele(NEW,Last).
/*delistify_last_arg0(Arg,M:Pred,Last):- Pred=..[F|ARGS],append([Arg|ARGS],[NEW],NARGS),NEWCALL=..[F|NARGS],
  quietly(M:NEWCALL),member_ele(NEW,Last).*/

each_ele(NEW,Last):- member_ele(NEW,Last).

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

:- include(library(pfc_syntax)).

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



%% correct_arities( ?VALUE1, ?FmlO, ?FmlO) is det.
%
% Correct Arities.
%
correct_arities([],Fml,Fml):-!.
correct_arities(_,Fml,Fml):- \+ compound(Fml),!.
correct_arities(_,FmlO,FmlO):-leave_as_is_logically(FmlO),!.
correct_arities([H|B],Fml,FmlO):-!,correct_arities(H,Fml,FmlM),correct_arities(B,FmlM,FmlO).
correct_arities(H,Fml,FmlO):- Fml=..[H,A|ARGS], ARGS\=[_],
  (ARGS==[] -> 
    correct_arities(H,A,FmlO);
       (correct_arities(H,A,CA), FmlM=..[H|ARGS],correct_arities(H,FmlM,FmlMC),FmlO=..[H,CA,FmlMC])),!.
correct_arities(H,Fml,FmlM):-  Fml=..[F|ARGS],must_maplist_det(correct_arities(H),ARGS,ARGSM),FmlM =.. [F|ARGSM].


:- public(subsT_each/3).



%% subsT_each( ?In, :TermARG2, ?In) is det.
%
% Subs True Stucture Each.
%
subsT_each(In,[],In):- !.
% subsT_each(In,[KV|TODO],Out):- !,get_kv(KV,X,Y),subst_except(In,X,Y,Mid),!,subsT_each(Mid,TODO,Out),!.
subsT_each(In,[KV|TODO],Out):- quietly(subst_except_l(In,[KV|TODO],Out)),!.




%% subst_except_l( :TermVar, ?VALUE2, :TermVar) is det.
%
% Subst Except (list Version).
%
subst_except_l(  Var, List, NVar ) :- nonvar(NVar),!,subst_except_l(  Var, List, MVar ),!,must(MVar=NVar).
subst_except_l(  Var, _,Var ) :- is_ftVar(Var),!.
% subst_except_l(  Var, _,Var ) :- leave_as_is_logically(Var),!.
subst_except_l([H|T],List,[HH|TT]):- !, subst_except_l(H,List,HH), subst_except_l(T,List,TT).
subst_except_l(   [], _,[] ) :-!.
subst_except_l(HT,List,HHTT):- compound(HT),
   compound_name_arguments(HT,F,ARGS0),
   subst_except_l([F|ARGS0],List,[FM|MARGS]),!,
   (atom(FM)->HHTT=..[FM|MARGS];append_termlist(FM,MARGS,HHTT)),!.

subst_except_l(  N, List,V ) :- memberchk(N=V,List),!.
subst_except_l(  N, List,V ) :- memberchk(N-V,List),!.
subst_except_l(HT, [N=V|List],TH):- subst(HT,N,V,HH),!,subst_except_l(HH,List,TH).
subst_except_l(HT,_List,HT).





%% skolem_in_code( ?X, ?Y) is det.
%
% Skolem In Code.
%
skolem_in_code(X,Y):- ignore(X=Y).



%% skolem_in_code( ?X, ?VALUE2, ?Fml) is det.
%
% Skolem In Code.
%
skolem_in_code(X,_,Fml):- when('?='(X,_),skolem_in_code(X,Fml)).

:- public(not_mudEquals/2).
:- was_dynamic(not_mudEquals/2).



%% not_mudEquals( ?X, ?Y) is det.
%
% Not Application Equals.
%
not_mudEquals(X,Y):- dif:dif(X,Y).

:- public(type_of_var/3).



%% type_of_var( ?Fml, ?Var, ?Type) is det.
%
% Type Of Variable.
%
type_of_var(Fml,Var,Type):- contains_type_lits(Fml,Var,Lits),!,(member(Type,Lits)*->true;Type='Unk').
:- style_check(+singleton).





%% to_dlog_ops( ?VALUE1) is det.
%
% Converted To Datalog Oper.s.
%

to_dlog_ops_old([
   '~'='~',
       'theExists'='exists',
       'thereExists'='exists',
       'ex'='exists',
       'forAll'='all',
       'forall'='all',
       ';'='v',
       ','='&',
       'neg'='~',
     % '-'='~',
     
    'not'='~',
    '\\+'='~',
     % '\\+'='naf',
     'and'='&',
     '^'='&',
     '/\\'='&',
      'or'='v',
      '\\/'='v',
      'V'='v',
      ':-'=':-',
 'implies'='=>',
   'if'='=>',
   'iff'='<=>',
'entails'='==>',
 'implies_fc'='==>',
 'implies_bc'='->',
   'equiv'='<=>',
      '=>'='=>',
     '<=>'='<=>']).

to_dlog_ops(XY):-
  to_sigma_ops(Y),maplist(rev_equals,Y,X),
  to_dlog_ops_old(Z),
  append([Y,X,Z],XY).

rev_equals(X=Y,Y=X).

to_sigma_ops([
             nesc = necessary,
             poss = possible,
             next = next,
            until = until,
                t = holds,
          'exists'='exists',
       'theExists'='exists',
     'thereExists'='exists',          
              'ex'='exists',
            'some'='exists',
             'all'='forall',
          'forAll'='forall',
          'forall'='forall',
               '~'='not',           
             'neg'='not',
             'not'='not',
             % '-'='not',
             '\\+'='not',
           % '\\+'='naf',
               '&'='and',
               ','='and',
             'and'='and',
               '^'='and',
             '/\\'='and',
               'v'='or',
               ';'='or',
              'or'='or',
             '\\/'='or',
               'V'='or',
              ':-'=':-',
              '<-'=':-',
     'entailed_by'=':-',
              '=>'='=>',
         'implies'='=>',
              'if'='=>',
             'iff'='<=>',
           'equiv'='<=>',
             '<=>'='<=>',
         'entails'='==>',
      'implies_fc'='==>',
      'implies_bc'='->']).




%% to_symlog_ops( ?VALUE1) is det.
%
% Converted To Symlog Oper.s.
%
to_symlog_ops([
   ';'='v',
   ','='&'
   %'=>'='=>',
   %'<=>'='<=>',
   %'not'='~',
   %':-'=':-'
   ]).




%% to_prolog_ops( ?VALUE1) is det.
%
% Converted To Prolog Oper.s.
%
to_prolog_ops([
   'v'=';',
   '&'=(',')
   %'=>'='=>',
   %'<=>'='<=>',
   %'~'='~',
   %'naf'='not',
   %'naf'='not',
   %':-'=':-'
   ]).



%% to_nonvars( :PRED2Type, ?IN, ?IN) is det.
%
% Converted To Nonvars.
%
to_nonvars(_Type,IN,IN):- is_ftVar(IN),!.
to_nonvars(_,Fml,Fml):- leave_as_is_logically(Fml),!.
to_nonvars(Type,IN,OUT):- is_list(IN),!,must_maplist_det(to_nonvars(Type),IN,OUT),!.
to_nonvars(Type,IN,OUT):- call(Type,IN,OUT),!.



%% convertAndCall( ?Type, :Goal) is det.
%
% Convert And Call.
%
convertAndCall(Type,Call):- fail,Call=..[F|IN],must_maplist_det(to_nonvars(Type),IN,OUT), IN \=@= OUT, !, must(apply(F,OUT)).
convertAndCall(_Type,Call):-call_last_is_var(Call).




%% as_dlog( ?Fml, ?Fml) is det.
%
% Converted To Datalog.
%
as_dlog(Fml,Fml):- leave_as_is_logically(Fml),!.
as_dlog(Fml,FmlO):- to_dlog_ops(OPS),subst_each(Fml,OPS,FmlM),!,correct_arities(['v','&'],FmlM,FmlO).


as_sigma(Fml,Fml):- leave_as_is_logically(Fml),!.
as_sigma(Fml,FmlO):- to_sigma_ops(OPS),subsT_each(Fml,OPS,FmlM),!,correct_arities(['or','and'],FmlM,FmlO).







%% as_symlog( ?Fml, ?Fml) is det.
%
% Converted To Symlog.
%
as_symlog(Fml,Fml):- leave_as_is_logically(Fml),!.
as_symlog(Fml,FmlO):- quietly((as_dlog(Fml,FmlM),!,to_symlog_ops(OPS),!,
  subsT_each(FmlM,OPS,FmlM),!,correct_arities(['v','&'],FmlM,FmlO))),!.




%% baseKB:as_prolog_hook( ?Fml, ?Fml) is det.
%
% Converted To Prolog.
%
baseKB:as_prolog_hook(Fml,Fml):- is_ftVar(Fml),!.
baseKB:as_prolog_hook(Fml,FmlO):- as_symlog(Fml,FmlM),
  to_prolog_ops(OPS),subsT_each(FmlM,OPS,FmlO).







%% adjust_kif( ?KB, ?Kif, ?KifO) is det.
%
% Adjust Knowledge Interchange Format.
%
adjust_kif(KB,Kif,KifO):- as_dlog(Kif,KifM),maybe_notrace(adjust_kif0(KB,KifM,KifO)),!.

% Converts to syntax that NNF/DNF/CNF/removeQ like

prepend_isas(_Quant,Expr,_Var,[],Expr).
prepend_isas(Quant,Expr, Var,[Type|Types],IsaExpr):- !,
 prepend_isas(Quant,Expr, Var,Type,ExprM),
 prepend_isas(Quant,ExprM,Var,Types,IsaExpr).

prepend_isas(all(),Expr,Var,Type,(isa(Var,Type) => Expr)).
prepend_isas(atmost(_),Expr,Var,Type,(isa(Var,Type) => Expr)).
prepend_isas(_Quant,Expr,Var,Type,(Expr & isa(Var,Type))):- leave_as_is_logically(Expr),!.
prepend_isas(_Quant, ~Expr,Var,Type,(isa(Var,Type) & ~Expr)).
prepend_isas(Quant, Body => Expr,Var,Type, Body => ExprO):- !, prepend_isas(Quant,Expr,Var,Type,ExprO).
prepend_isas(Quant, Body & Expr, Var,Type, Body & ExprO):- !, prepend_isas(Quant,Expr,Var,Type,ExprO).
prepend_isas(Quant,quant(Q,V,Expr),Var,Type,quant(Q,V,ExprO)):- !, prepend_isas(Quant,Expr,Var,Type,ExprO).
prepend_isas(_Quant,Expr,Var,Type,(Expr & isa(Var,Type))).




%% adjust_kif0( ?VALUE1, ?V, ?V) is det.
%
% Adjust Knowledge Interchange Format Primary Helper.
%

adjust_kif0(KB,I,O):-nonvar(O),!,adjust_kif0(KB,I,M),M=O,!.

adjust_kif0(_,V,V):- is_ftVar(V),!.
adjust_kif0(_,A,A):- \+ compound(A),!.
adjust_kif0(_,A,A):- leave_as_is_logically(A),!.

adjust_kif0(KB,not(Kif),(KifO)):- !,adjust_kif0(KB, ~(Kif),KifO).
% adjust_kif0(KB,\+(Kif),(KifO)):- !,adjust_kif0(KB, naf(Kif),KifO).
adjust_kif0(KB,nesc(Kif),nesc(KifO)):- !,adjust_kif0(KB,Kif,KifO).
adjust_kif0(KB,poss(Kif),poss(KifO)):- !,adjust_kif0(KB,Kif,KifO).
adjust_kif0(KB, ~(Kif),    ~(KifO)):- !,adjust_kif0(KB,Kif,KifO).
adjust_kif0(KB,t(Kif),t(KifO)):- !,adjust_kif0(KB,Kif,KifO).

/*
adjust_kif0(KB,all(L,Expr),               ExprO):-L==[],!,adjust_kif0(KB,Expr,ExprO).
adjust_kif0(KB,all(V,Expr),               ExprO):-atom(V),svar_fixvarname(V,L),subst(Expr,V,'$VAR'(L),ExprM),!,adjust_kif0(KB,all('$VAR'(L),ExprM),ExprO).
adjust_kif0(KB,all([L|List],Expr),all(L,ExprO)):-is_list(List),!,adjust_kif0(KB,all(List,Expr),ExprO).
% adjust_kif0(KB,all(L,Expr),               ExprO):- \+ contains_var(L,Expr),!,adjust_kif0(KB,Expr,ExprO).
adjust_kif0(KB,all(L,Expr),all(L,ExprO)):-!,adjust_kif0(KB,Expr,ExprO).
*/

adjust_kif0(KB,(H & B),(HH & ConjO)):- !, adjust_kif(KB,H,HH),adjust_kif(KB,B,ConjO).
adjust_kif0(KB,(H v B),(HH v ConjO)):- !, adjust_kif(KB,H,HH),adjust_kif(KB,B,ConjO).
adjust_kif0(KB,[L|Ist],ConjO):- is_list([L|Ist]),!,must_maplist_det(adjust_kif0(KB),[L|Ist],ConjO),!.
adjust_kif0(KB,(H:-[L|Ist]),(HH:-ConjO)):- nonvar(Ist), adjust_kif(KB,H,HH),is_list([L|Ist]),adjust_kif0(KB,'&'([L|Ist]),ConjO).
adjust_kif0(KB,(H:-B),(HH:-ConjO)):- !, adjust_kif(KB,H,HH),adjust_kif(KB,B,ConjO).
adjust_kif0(KB,'&'([L|Ist]),ConjO):- is_list([L|Ist]),list_to_conjuncts_det('&',[L|Ist],Conj),adjust_kif0(KB,Conj,ConjO).
adjust_kif0(KB,'v'([L|Ist]),ConjO):- is_list([L|Ist]),list_to_conjuncts_det('v',[L|Ist],Conj),adjust_kif0(KB,Conj,ConjO).
%adjust_kif0(KB,CONJ,ConjO):- compund_name_arguments(CONJ,'&',[L|Ist]),list_to_conjuncts_det('&',[L|Ist],Conj),adjust_kif0(KB,Conj,ConjO).
%adjust_kif0(KB,CONJ,ConjO):- compund_name_arguments(CONJ,'v',[L|Ist]),list_to_conjuncts_det('v',[L|Ist],Conj),adjust_kif0(KB,Conj,ConjO).
adjust_kif0(KB,PAB,KifO):- PAB=..[F|AB],must_maplist_det(adjust_kif0(KB),AB,ABO),maybe_notrace(adjust_kif4(KB,F,ABO,KifO)).

%% adjust_kif0( ?KB, ?Not_P, :TermARGS, ?O) is det.
%
% Adjust Knowledge Interchange Format Primary Helper.
%
adjust_kif4(KB,call_builtin,ARGS,O):-!,PARGS=..ARGS,adjust_kif0(KB,PARGS,O),!.

adjust_kif4(KB,'v',[E|LIST],O3):- !, adjust_kif0(KB,'v'([E|LIST]),O3).
adjust_kif4(KB,'&',[E|LIST],O3):- !, adjust_kif0(KB,'&'([E|LIST]),O3).
adjust_kif4(KB,true_t,[F|LIST],O3):-atom(F),!,PARGS=..[F|LIST],adjust_kif0(KB,(PARGS),O3),!.
adjust_kif4(KB,not_true_t,[F|LIST],O3):-atom(F),!,PARGS=..[F|LIST],adjust_kif0(KB, ~(PARGS),O3),!.
adjust_kif4(KB,~,[F|LIST],O3):-atom(F),!,PARGS=..[F|LIST],adjust_kif0(KB, ~(PARGS),O3),!.

/*
adjust_kif4(KB,possible_t,[A],O):-!,adjust_kif0(KB,poss(A),O),!.
adjust_kif4(KB,possible_t,ARGS,O):-!,PARGS=..ARGS,adjust_kif0(KB,poss(PARGS),O).

% adjust_kif0(KB,asserted_t,[A],O):-!,adjust_kif0(KB,t(A),O),!.
% adjust_kif0(KB,asserted_t,[A|RGS],O):- atom(A),PARGS=..[A|RGS],!,adjust_kif0(KB,t(PARGS),O).

adjust_kif4(KB,true_t,[A|RGS],O):- atom(A),PARGS=..[A|RGS],adjust_kif0(KB,PARGS,O),!.
adjust_kif4(KB,Not_P,ARGS,O):-atom_concat('not_',P,Not_P),!,PARGS=..[P|ARGS],adjust_kif0(KB, ~(PARGS),O).
adjust_kif4(KB,Int_P,ARGS,O):-atom_concat('int_',P,Int_P),!,append(LARGS,[_, _, _, _, _, _, _ ],ARGS),
   PLARGS=..[P|LARGS],adjust_kif0(KB,PLARGS,O).

adjust_kif4(KB,P,ARGS,O):- fail, atom_concat(_,'_t',P),!,append(LARGS,[_, _, _, _, _, _],ARGS),
   PARGS=..[P|LARGS],adjust_kif0(KB,PARGS,O).
*/

adjust_kif4(KB,W,[P,A,R|GS],O):- call(clause_b(is_wrapper_pred(W))),PARGS=..[P,A,R|GS],adjust_kif0(KB,t(PARGS),O).

adjust_kif4(KB,F,ARGS,O):-KIF=..[F|ARGS],length(ARGS,L),L>2,adjust_kif0(KB,KIF,F,ARGS,Conj),KIF\=@=Conj,!,adjust_kif0(KB,Conj,O).
% adjust_kif0(KB,W,[A],O):-is_wrapper_pred(W),adjust_kif(KB,A,O),!.

adjust_kif4(_, F,ARGS,P):- P=..[F|ARGS],!.



%% adjust_kif0( ?KB, ?KIF, ?OP, ?ARGS, ?Conj) is det.
%
% Adjust Knowledge Interchange Format Primary Helper.
%
adjust_kif0(KB,KIF,OP,ARGS,Conj):-must_maplist_det(adjust_kif(KB),ARGS,ABO),adjust_kif5(KB,KIF,OP,ABO,Conj).




%% adjust_kif5( ?KB, ?KIF, ?VALUE3, ?ARGS, ?Conj) is det.
%
% Adjust Kif5.
%
adjust_kif5(_KB,_KIF,',',ARGS,Conj):- list_to_conjuncts_det('&',ARGS,Conj).
adjust_kif5(      _,_,';',ARGS,Conj):-list_to_conjuncts_det('v',ARGS,Conj).
adjust_kif5(      _,_,'v',ARGS,Conj):-list_to_conjuncts_det('v',ARGS,Conj).
adjust_kif5(      _,_,'&',ARGS,Conj):-list_to_conjuncts_det('&',ARGS,Conj).





%% local_pterm_to_sterm( ?P, ?S) is det.
%
% Local Pterm Converted To Sterm.
%
local_pterm_to_sterm(P,['$VAR'(S)]):- if_defined(mpred_sexp_reader:svar(P,S),fail),!.
local_pterm_to_sterm(P,['$VAR'(S)]):- if_defined(mpred_sexp_reader:lvar(P,S),fail),!.
local_pterm_to_sterm(P,[P]):- leave_as_is_logically(P),!.
local_pterm_to_sterm((H:-P),(H:-S)):-!,local_pterm_to_sterm(P,S),!.
local_pterm_to_sterm((P=>Q),[implies,PP,=>,QQ]):-local_pterm_to_sterm(P,PP),local_pterm_to_sterm(Q,QQ).
local_pterm_to_sterm((P<=>Q),[equiv,PP,QQ]):-local_pterm_to_sterm(P,PP),local_pterm_to_sterm(Q,QQ).
local_pterm_to_sterm(all(P,Q),[all(PP),QQ]):-local_pterm_to_sterm(P,PP),local_pterm_to_sterm(Q,QQ).
local_pterm_to_sterm(exists(P,Q),[ex(PP),QQ]):-local_pterm_to_sterm(P,PP),local_pterm_to_sterm(Q,QQ).
local_pterm_to_sterm( ~(Q),[not,QQ]):-local_pterm_to_sterm(Q,QQ).
local_pterm_to_sterm(poss(Q),[poss(QQ)]):-local_pterm_to_sterm(Q,QQ).
local_pterm_to_sterm('&'(P,Q),PPQQ):-local_pterm_to_sterm(P,PP),local_pterm_to_sterm(Q,QQ),flatten([PP,QQ],PPQQ0),list_to_set(PPQQ0,PPQQ).
local_pterm_to_sterm(','(P,Q),PPQQ):-local_pterm_to_sterm(P,PP),local_pterm_to_sterm(Q,QQ),flatten([PP,QQ],PPQQ0),list_to_set(PPQQ0,PPQQ).
local_pterm_to_sterm('v'(P,Q),[or,[PP],[QQ]]):-local_pterm_to_sterm(P,PP),local_pterm_to_sterm(Q,QQ),!.
local_pterm_to_sterm('beliefs'(P,Q),[beliefs(PP),QQ]):-local_pterm_to_sterm2(P,PP),local_pterm_to_sterm(Q,QQ),!.
local_pterm_to_sterm(P,S):-subst_except(P,'&',',',Q),P\=@=Q,!,local_pterm_to_sterm(Q,S),!.
local_pterm_to_sterm(P,S):-subst_except(P,'v',';',Q),P\=@=Q,!,local_pterm_to_sterm(Q,S),!.
local_pterm_to_sterm(P,[Q]):-P=..[F|ARGS],must_maplist_det(local_pterm_to_sterm2,ARGS,QARGS),Q=..[F|QARGS].
local_pterm_to_sterm(P,[P]).




%% local_pterm_to_sterm2( ?P, ?Q) is det.
%
% Local Pterm Converted To Sterm Extended Helper.
%
local_pterm_to_sterm2(P,Q):-local_pterm_to_sterm(P,PP),([Q]=PP;Q=PP),!.






%======  make a sequence out of a disjunction =====



%% flatten_or_list( ?A, ?B, ?C) is det.
%
% Flatten Or List.
%
flatten_or_list(A,B,C):- convertAndCall(as_symlog,flatten_or_list(A,B,C)).
flatten_or_list(KB,v(X , Y), F):- !,
   flatten_or_list(KB,X,A),
   flatten_or_list(KB,Y,B),
   flatten([A,B],F).
flatten_or_list(_KB,X,[X]).






%% fmtl( ?X) is det.
%
% Fmtl.
%
fmtl(X):- baseKB:as_prolog_hook(X,XX), fmt(XX).




%% write_list( ?F) is det.
%
% Write List.
%
write_list([F|R]):- write(F), write('.'), nl, write_list(R).
write_list([]).




%% unnumbervars_with_names( ?Term, ?CTerm) is det.
%
% Numbervars Using Names.
%

%unnumbervars_with_names(X,X):-!.
% unnumbervars_with_names(Term,CTerm):- ground(Term),!,dupe_term(Term,CTerm).

/*
unnumbervars_with_names(Term,CTerm):- break,
 must_det_l((
   source_variables_l(NamedVars),
   copy_term(Term:NamedVars,CTerm:CNamedVars),
   b_implode_varnames0(CNamedVars))),!.
*/
unnumbervars_with_names(Term,CTerm):-   show_failure(quietly(unnumbervars(Term,CTerm))),!.

unnumbervars_with_names1(Term,CTerm):-
  must_det_l((
   source_variables_l(NamedVars),
   copy_term(Term:NamedVars,CTerm:CNamedVars),
   term_variables(CTerm,Vars),
   call((dtrace,get_var_names(Vars,CNamedVars,Names))),
   b_implode_varnames0(Names),
  % numbervars(CTerm,91,_,[attvar(skip),singletons(false)]),
   append(CNamedVars,NamedVars,NewCNamedVars),
   list_to_set(NewCNamedVars,NewCNamedVarsS),
   remove_grounds(NewCNamedVarsS,NewCNamedVarsSG),
   put_variable_names(NewCNamedVarsSG))),!.


unnumbervars_with_names_best2(Term,CTerm):-
 must_det_l((
   source_variables_l(NamedVars),!,
   copy_term(Term:NamedVars,CTerm:CNamedVars),
   term_variables(CTerm,Vars),
   get_var_names(Vars,CNamedVars,Names),
   b_implode_varnames0(Names),
  %  numbervars(CTerm,91,_,[attvar(skip),singletons(false)]),
   append(CNamedVars,NamedVars,NewCNamedVars),
   list_to_set(NewCNamedVars,NewCNamedVarsS),
   remove_grounds(NewCNamedVarsS,NewCNamedVarsSG),
   put_variable_names(NewCNamedVarsSG))),!.




%% get_var_names( :TermV, ?NamedVars, :TermS) is det.
%
% Get Variable Names.
%
get_var_names([],_,[]).
get_var_names([V|Vars],NamedVars,[S|SNames]):-
    get_1_var_name(V,NamedVars,S),
    get_var_names(Vars,NamedVars,SNames).




%% get_1_var_name( ?Var, :TermNamedVars, ?Name) is det.
%
% get  Secondary Helper Variable name.
%
get_1_var_name(_V,[],_S).

get_1_var_name(Var,NamedVars,Name):- compound(Var),arg(1,Var,NV),!,get_1_var_name(NV,NamedVars,Name).
get_1_var_name(Var,NamedVars,Var=NV):-atom(Var),NamedVars=[_|T],nb_setarg(2,NamedVars,[Var=NV|T]),!.
get_1_var_name(Var,[N=V|_NamedVars],Name=V):-
     (Var == V -> Name = N ; (Var==Name -> Name=Var ; fail )),!.
get_1_var_name(Var,[_|NamedVars],Name):- get_1_var_name(Var,NamedVars,Name).



call_l2r(P,X,Y):- var(X) -> freeze(X,call_l2r(P,X,Y)) ; (call(P,X,Z),Y=Z).


must_det_here(G):- w_o_c(must_det(G)).





:- meta_predicate(unless_ignore(*,*)).
unless_ignore(A,B):- call(A)->B;true.


local_pretty_numbervars_ground(P,X):-
  pretty_numbervars_unground(P,X),
  guess_pretty(X).

no_kb_why_flags_assert([X],Y):-!,no_kb_why_flags_assert(X,Y).
no_kb_why_flags_assert(kb_why_flags_assert(_,_,_,X),X).
no_kb_why_flags_assert(X,X).


:- meta_predicate(in_box(:)).
write_eng(X):- in_cmt(in_box((wots(S,write_eng(0,X)),write(S)))).

in_box(G):-
  format("~N~n%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%~n"),ttyflush,
  ignore(G),ttyflush,
  format("~N~n%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%~n"),ttyflush,
  !.

is_lit_fact(P):- contains_modal(P),!,fail.
is_lit_fact(P):- is_lit_atom(P).

% Our Modal Ops are arity 1
no_junctions(P):- \+ compound(P).
no_junctions(P):- P=..[_,A],!,no_junctions(A).
no_junctions(P):- is_lit_atom(P),!.

write_eng(_,X):- var(X),!,writeq(var(X)), write(' ').
write_eng(_,'$VAR'(P)):- !,write('?'), write(P), write(' '),!.
write_eng(L,X):- is_list(X), !, maplist(write_eng(L),X).

write_eng(L,quote(P)):- !, write('" '),write_eng(L,P),write('" ').
write_eng(L,paren(P)):- is_lit_fact(P),!, write_eng(L,quote(P)).
write_eng(L,paren(P)):- no_junctions(P),!, write_eng(L,P).
write_eng(L,paren(P)):- !, write('('),write_eng(L,P),write(')').
write_eng(_,q(X)):- writeq(X),write(' ').

% rules are 0
write_eng(0,~poss(P)):-  write_eng(1,['it is impossible that ',paren(P)]).
write_eng(0,~nesc(P)):-  write_eng(1,['it is NOT necessarily true that ',paren(P)]).
write_eng(0,~P):- write_eng(1,[' it is false that ',P]).
write_eng(0,poss(~P)):-  write_eng(1,[' it is possiblly FALSE that ',P]).
write_eng(0,nesc(~P)):-  write_eng(1,[' it is necessarily FALSE that ',P]).
write_eng(0,poss(P)):-  write_eng(1,[' it is possible that ',P]).
write_eng(0,nesc(P)):-  write_eng(1,[' it is necessarily true that ',P]).
write_eng(0,P):- write_eng(1,[' ',P]).


write_eng(L,all(V,P)):-   write_eng(L,['~n For all ',V,'~n   ',paren(P)]).
write_eng(L,exists(V,P)):-   write_eng(L,['~n There exists ',V,'~n   ',paren(P)]).
write_eng(L,'$existential'(V,_,P)):- write_eng(L,[' by default ',V,q(P)]).

write_eng(L,poss(~P)):-   write_eng(L,[paren(P),'is possibly false ']).
write_eng(L,nesc(~P)):-   write_eng(L,[paren(P),'is necessarily false ']).
write_eng(L,poss( P)):-   write_eng(L,[paren(P),'is possible ']).
write_eng(L,nesc( P)):-   write_eng(L,[paren(P),'is necessarily true ']).
write_eng(L,~poss( P)):-   write_eng(L,[paren(P),'is impossible ']).
write_eng(L,~nesc( P)):-   write_eng(L,[paren(P),'is NOT necessarily true ']).

write_eng(L,A => B):- !, write_eng(L,['If: ~n   ',A,'then it is~n Implied that:~n   ',B]).
write_eng(L,A ==>B):- !, write_eng(L,['Whenever: ~n   ',A,'~n It\'s Proof that:~n   ',B]).
write_eng(L,A & B):- write_eng(L,[paren(A),' and ~n   ',paren(B)]).
write_eng(L,A v B):- write_eng(L,[paren(A),' or ',paren(B)]).


write_eng(_,X):- atom(X), atom_contains(X,' '), format(X),!.

% fact/1 is 9
write_eng(_,fact(P)):- !, write_eng(9,P).

% facts are 9
write_eng(9,X):- X=..[F,A,B],!,write_eng(9,[A,F,B]).
write_eng(9,X):- X=..[F,A],!,write_eng(9,[A,'isa',F]).
write_eng(9,X):- writeq(X),write(' ').

write_eng(_,~P):- write_eng(9,[paren(P),' is false']).
write_eng(_, P):- write_eng(9, P).


nnf_default(Fml,_):- \+ compound(Fml), !, fail.
nnf_default( default(Fml), Fml).
nnf_default(default1(Fml), possible(Fml) => Fml).
nnf_default(default2(Fml), Fml => necessary(Fml)).
nnf_default(default3(Fml), possible(Fml) => necessary(Fml)).
nnf_default(default4(Fml), Fml => possible(Fml)).

nnf_default(and(Fml1 , Fml2), AND):-
  \+ (compound(Fml1), functor(Fml1,F,2), member(F,[and,or,(=>),not,possible,necessary])),
  AND = and('=>'(possible(Fml2),Fml1), '=>'(possible(Fml1),Fml2)).

%final_nnf(O,OO):-nnf('?KB',O,M),O\=@=M,!,final_nnf(M,OO).

simplify_all(KB,Fml,OutZ):- simplify_cheap(post,Fml,Fml2), (Fml \=@= Fml2 -> simplify_all(KB,Fml2,OutZ); Fml2=OutZ).

% final_nnf(O,O):-!.
final_nnf(A,A):- (is_ftVar(A);leave_as_is_logically(A)),!.
%final_nnf(A==>B,AA==>BB):-!,final_nnf(A,AA),final_nnf(B,BB).
final_nnf(O,OO):- once((nnf(KB,O,O1),adjust_kif(KB,O1,O2),simplify_all(KB,O2,OO))), !. % O\=@=M,!,final_nnf(M,OO).
%final_nnf(A==>B,AA==>BB):-!,final_nnf(nesc(A),AA),final_nnf(nesc(B),BB).
%final_nnf(O,OO):-as_sigma(O,E), sigma_ace:getNegationForm(toplevel,0,'?KB','?Ctx',_KRVars,_Flags,E,_,OM,_),as_dlog(OM,OO).
final_nnf(O,O).

entails_to_boxlog(KB,Why,Flags,List,RealOUT):-
  entails_to_boxlog(0,KB,Why,Flags,List,RealOUT).
entails_to_boxlog(N,KB,Why,Flags,List,RealOUT):- is_list(List),!,
  entails_to_boxlog(N,KB,Why,Flags,List,ListL), append(ListL,RealOUT).
entails_to_boxlog(_,KB,Why,Flags,A,[RealOUT]):- leave_as_is_logically(A),
  RealOUT = A, nop(kb_why_flags_assert(KB,Why,Flags,A)).
entails_to_boxlog(N,KB,Why,Flags,(A & B),RealOUT):-
  entails_to_boxlog(N,KB,Why,Flags,A,AL),
  entails_to_boxlog(N,KB,Why,Flags,B,BL),
  append(AL,BL,RealOUT),!.
entails_to_boxlog(N,KB,Why,Flags,A,RealOUT):- N<5,
  once(final_nnf(A,OUT)), A\=@=OUT,!, N2 is N +1,
  entails_to_boxlog(N2,KB,Why,Flags,OUT,RealOUT).
entails_to_boxlog(_,KB,Why,Flags,A,[RealOUT]):-  
  RealOUT = A, nop(kb_why_flags_assert(KB,Why,Flags,A)).

show_boxlog(O):- is_ftVar(O), !, show_boxlog(ftVar(O)).
show_boxlog([X|Y]):- show_boxlog(X), ignore((Y \==[], format('~N%  AND~n'),show_boxlog(Y))).
show_boxlog(O):- is_list(O),!,maplist(show_boxlog,O).
show_boxlog(and(X,Y)):- show_boxlog(X),format('~N%  AND~n'),show_boxlog(Y).
show_boxlog('&'(X,Y)):- show_boxlog(X),format('~N%  AND~n'),show_boxlog(Y).
show_boxlog(kb_why_flags_assert(KB,Why,_Flags,O)):-
   nop(wdmsg(kb_y_________________________________________________________f(KB,Why))),!,
   %show_boxlog(O).
   setup_call_cleanup(system:push_operators(baseKB:[op(1000,yfx,'&')],Undo),show_boxlog(O),system:pop_operators(Undo)).
  
show_boxlog(O):- write_eng(O), subst_each(O,['& '='/\\','v '='\\/'],OO), format('~N~n'), print_tree(OO),format('.~N~n').

show_boxlog:- forall(call_u(boxlog(O)),show_boxlog(O)).

%% kif_to_boxlog( ?Wff is det.
%
% Knowledge Interchange Format Converted To Datalog.
%
kif_to_boxlog(P):-
  update_changed_files,
  local_pretty_numbervars_ground(P,X),
  format('~N~n~n~n~n=======================================================~n'),  
  display(X),
  format("~N============================================~n"),
  format('~n~n?- kif_to_boxlog( ~q ).\n\n',[(X)]),ttyflush,
  
   
   format('~n~n% In English: ',[]),
   write_eng(X),

   %with_no_output
   kif_to_boxlog(X,E),flatten([E],EF),
   maplist(no_kb_why_flags_assert,EF,S),sort(S,O),

   length(O,L),
   in_box((format('% Results in the following ~w entailment(s): ~n',[L]),
           call(maplist,writeqln1,O))),

   show_boxlog(O),
   format("============================================"),!.

writeqln1(O):- writeq(O),writeln('.').

%% kif_to_boxlog( ?Wff, ?Out) is det.
%
% Knowledge Interchange Format Converted To Datalog.
%
%====== kif_to_boxlog(+Wff,-NormalClauses):-
:- public(kif_to_boxlog/2).

%% kif_to_boxlog( +Fml, -Datalog) is det.
%
% Knowledge Interchange Format Converted To Datalog.
%
kif_to_boxlog(Wff,Out):- Wff\=(_:-_), why_to_id(rule,Wff,Why),!,must(kif_to_boxlog(Wff,Out,Why)),!.
% kif_to_boxlog(Wff,Out):- loop_check(kif_to_boxlog(Wff,Out),Out=looped_kb(Wff)). % kif_to_boxlog('=>'(WffIn,enables(Rule)),'$VAR'('MT2'),complete,Out1), % kif_to_boxlog('=>'(enabled(Rule),WffIn),'$VAR'('KB'),complete,Out).

%% kif_to_boxlog( +Fml, +Why, -Datalog) is det.
%
% Knowledge Interchange Format Converted To Datalog.
%
:- export(kif_to_boxlog/3).
kif_to_boxlog(Wff,Out,Why):- must(Wff\=(_:-_)), w_o_c(kif_to_boxlog(Wff,'$VAR'('KB'),Why,Out)),!.

%% kif_to_boxlog( +Fml, ?KB, +Why, -Datalog) is det.
%
% Knowledge Interchange Format Converted To Datalog.
%
:- export(kif_to_boxlog/4).
kif_to_boxlog(I,KB,Why,Datalog):- % trace,
  convert_if_kif_string( I, PTerm),
  kif_to_boxlog(PTerm,KB,Why,Datalog), !.

kif_to_boxlog(HB,KB,Why,FlattenedO):- 
  sumo_to_pdkb(HB,HB00)->
  sumo_to_pdkb(KB,KB00)->
  sumo_to_pdkb(Why,Why00)->
  pretty_numbervars_ground((HB00,KB00,Why00),(HB0,KB0,Why0)),
  with_no_kif_var_coroutines(must(kif_to_boxlog_attvars(HB,HB0,KB0,Why0,FlattenedO))),!.


kif_to_boxlog_attvars(Original,kif(HB),KB,Why,FlattenedO):-!,kif_to_boxlog_attvars(Original,HB,KB,Why,FlattenedO).
kif_to_boxlog_attvars(Original,clif(HB),KB,Why,FlattenedO):-!,kif_to_boxlog_attvars(Original,HB,KB,Why,FlattenedO).
kif_to_boxlog_attvars(Original,modal_clif(HB),KB,Why,FlattenedO):-!,kif_to_boxlog_attvars(Original,HB,KB,Why,FlattenedO).
  
kif_to_boxlog_attvars(_Original,HB,KB,Why,FlattenedO):- compound(HB),HB=(HEAD:- BODY),!,
  must_det_l((
   check_is_kb(KB),
   conjuncts_to_list_det(HEAD,HEADL),
   conjuncts_to_list_det(BODY,BODYL),
   finish_clausify([cl(HEADL,BODYL)],KB,Why,FlattenedO))),!.

kif_to_boxlog_attvars(Original,WffIn0,KB0,Why0,RealOUT):-    
   pretty_numbervars_ground(WffIn0:KB0:Why0,WffIn:KB:Why),
   check_is_kb(KB),
   clif_to_modal_clif(WffIn,ModalKBKIF),   
   % save_wid(Why,kif,DLOGKIF),
   % save_wid(Why,pkif,FullQuant),   
   kif_to_boxlog_attvars2(Original,ModalKBKIF,KB,Why,RealOUT).   

kif_to_boxlog_attvars(Original,Clause,KB,Why,RealOUT):-
 kif_to_boxlog_attvars2(Original,Clause,KB,Why,RealOUT).

kif_to_boxlog_attvars2(_Original,Clause,KB,Why,RealOUT):-
  flag(skolem_count,_,0),
  kif_optionally_e(true,as_sigma,Clause,CTerm),
  %unnumbervars_with_names_best2(Clause0,CTerm),
  %Clause0=CTerm,
 % get_varname_list(KRVars),
  % copy_term(KRVars+CTerm,Copy,_),
  in_cmt((write('kif_to_boxlog_attvars2 = '),display(CTerm))),
	sigma_ace:getAssertionClauses(KB,Why,CTerm,Out0,[],FlagsL),!,
  kif_optionally_e(true,as_dlog,Out0,Out00),
	subst(FlagsL,'.','&',Flags),
  entails_to_boxlog(KB,Why,Flags,Out00,RealOUT),!.

kif_to_boxlog_attvars2(Original,ModalKBKIF,KB,Why,RealOUT):-
   kif_optionally_e(true,nnf(KB),ModalKBKIF,NNF),
   flag(skolem_count,_,0),
   term_attvars(NNF,NNFVs), maplist(del_attr_type(skv),NNFVs),
   sanity(NNF \== poss(~t)),
   % sdmsg(nnf=(NNF)),
  %%kif_optionally_e(true,nnf(KB),ModalKBKIF,NNF),
   %term_attvars(NNF,NNFVs), maplist(del_attr_type(skv),NNFVs),
   %sanity(NNF \== poss(~t)),
   kif_optionally_e(false,removeQ_3(KB),NNF,UnQ),   
   must(kif_to_boxlog_theorist(Original,Why,KB,UnQ,RealOUT)),!.

removeQ_3(KB,NNF,UnQ):-removeQ(KB,NNF,[],UnQ).

kif_to_boxlog_theorist2(Original,Why,KB,THIN,RealOUT):-
   demodal_clauses(KB,THIN,THIN2),
   kif_optionally_e(always,as_prolog_hook,THIN2,THIN3),    
   must(kif_optionally_e(always,cf(Original,Why,KB),THIN3,RealOUT)),!.


kif_to_boxlog_theorist(_FullQuant,Why,KB,UnQ,RealOUT):-
  %must_maplist_det(call,[
   current_outer_modal_t(HOLDS_T),
   % true cause poss loss
   kif_optionally_e(false,to_tlog(HOLDS_T,KB),UnQ,UnQ666),
   kif_optionally_e(always,as_prolog_hook,UnQ666,THIN0),
   kif_optionally_e(always,to_tnot_2,THIN0,THIN),!,
   must(must_be_unqualified(THIN)),
   % unless_ignore(THIN\==UnQ, sdmsg(tlog_nnf_in=THIN)),
   kif_optionally_e(always,tlog_nnf(even),THIN,RULIFY),
   %unless_ignore(THIN\== ~ RULIFY,kif_optionally_e(always,as_dlog,RULIFY,_)),
   % trace,
   once((rulify(constraint,RULIFY,SideEffectsList),SideEffectsList\==[])),!,
   kif_optionally_e(always,list_to_set,SideEffectsList,SetList),
   kif_optionally_e(always,finish_clausify(KB,Why),SetList,RealOUT),!.

tlog_nnf(Even,THIN,RULIFY):- th_nnf(THIN,Even,RULIFY).


/*
finish_clausify(KB,Why,Datalog,FlattenedO):-
  set_prolog_flag(gc,true),
  (current_prolog_flag(runtime_breaks,3)-> 
       zotrace(kif_optionally_e(true,interface_to_correct_boxlog(KB,Why),Datalog,DatalogT));
   kif_optionally_e(true,interface_to_correct_boxlog(KB,Why),Datalog,DatalogT)),
   demodal_clauses(KB,DatalogT,FlattenedO),!.
*/

finish_clausify(KB,Why,Datalog,RealOUT):-
  locally(set_prolog_flag(dmsg_level,never),
  ((must_maplist_det(call,[
  kif_optionally_e(true,from_tlog,Datalog,Datalog11),
  kif_optionally_e(false,interface_to_correct_boxlog(KB,Why),Datalog11,Datalog1),  
  kif_optionally_e(true,demodal_clauses(KB),Datalog1,Datalog12),
  kif_optionally_e(true,remove_unused_clauses,Datalog12,Datalog2),
  kif_optionally(false,defunctionalize_each,Datalog2,Datalog3),
  predsort(sort_by_pred_class,Datalog3,Datalog4), 
  kif_optionally(false,vbody_sort,Datalog4,Datalog5),
  kif_optionally(false,demodal_clauses(KB),Datalog5,Datalog6),
  kif_optionally_e(true,combine_clauses_with_disjuncts,Datalog6,Datalog7),  
  kif_optionally_e(true,demodal_clauses(KB),Datalog7,Datalog8),
  kif_optionally(false,kb_ify(KB),Datalog8,RealOUT)])))),!.




is_4th_order(F):- atom_concat('never_',_,F).
is_4th_order(F):- atom_concat('prove',_,F).
is_4th_order(F):- atom_concat('call_',_,F).
is_4th_order(F):- atom_concat('with_',_,F).
is_4th_order(F):- atom_concat('fals',_,F).
is_4th_order(F):- atom_concat(_,'neg',F).
is_4th_order(F):- atom_concat(_,'nesc',F).
is_4th_order(F):- atom_concat(_,'dom',F).
is_4th_order(F):- atom_concat(_,'xdoms',F).
is_4th_order(F):- atom_concat(_,'serted',F).
is_4th_order(F):- atom_concat(_,'_mu',F).
is_4th_order(F):- atom_concat(_,'_i',F).
is_4th_order(F):- atom_concat(_,'_u',F).
is_4th_order(make_existential).
is_4th_order(ist).
is_4th_order(F):-is_2nd_order_holds(F).


:- fixup_exports.
  
kb_ify(_,FlattenedOUT,FlattenedOUT):- \+ compound(FlattenedOUT),!.
kb_ify(_,ist(KB,H),ist(KB,H)):-!.
kb_ify(_KB,skolem(X,SK),skolem(X,SK)):-!.
kb_ify(_KB,make_existential(X,SK),make_existential(X,SK)):-!.

kb_ify(KB,H,HH):- H=..[F,ARG1,ARG2|ARGS],is_4th_order(F),HH=..[F,ARG1,ARG2,KB|ARGS],!.
kb_ify(KB,H,HH):- H=..[F,ARG1],is_4th_order(F),HH=..[F,KB,ARG1],!.

kb_ify(KB,H,HH ):- H=..[F|ARGS],tlog_is_sentence_functor(F),!,must_maplist_det(kb_ify(KB),ARGS,ARGSO),!,HH=..[F|ARGSO].
kb_ify(KB,FlattenedOUT,FlattenedOUT):- contains_var(KB,FlattenedOUT),!.

kb_ify(KB,H,ist(KB,HH)):- H=..[F|ARGS],!,must_maplist_det(kb_ify(KB),ARGS,ARGSO),!,HH=..[F|ARGSO].
kb_ify(_KB, (G), (G)):- !.


:- meta_predicate(to_tlog(+,+,*,-)).
:- meta_predicate(to_tlog_lit(+,+,+,*,-)).

to_tlog(args,_KB,Var, Var):-!.  % for now
to_tlog(_,_KB,Var, Var):- quietly(leave_as_is_logically(Var)),!.
to_tlog(MD,KB,M:H,M:HH):- !, to_tlog(MD,KB,H,HH).
to_tlog(MD,KB,[H|T],[HH|TT]):- !, to_tlog(MD,KB,H,HH),to_tlog(MD,KB,T,TT).


to_tlog(MD,KB, quant(Q,X,F1), quant(Q,X,F2)):- !, to_tlog(MD,KB,F1,F2).
to_tlog(_,KB, poss(F),  poss( HH)):-!, to_tlog(poss,KB,F, HH).
to_tlog(_,KB, poss(F),  poss( HH)):-!, to_tlog(poss,KB,F, HH).

to_tlog(MD,KB,(X ; Y),(Xp v Yp)) :- !, to_tlog(MD,KB,X,Xp), to_tlog(MD,KB,Y,Yp),!.
to_tlog(MD,KB,(X v Y),(Xp v Yp)) :- !, to_tlog(MD,KB,X,Xp), to_tlog(MD,KB,Y,Yp),!.
to_tlog(MD,KB,H,HH ):- H=..[F|ARGS],tlog_is_sentence_functor(F),!,must_maplist_det(to_tlog(MD,KB),ARGS,ARGSO),!,HH=..[F|ARGSO].
to_tlog(MD,KB, ~(XF),  n(HH)):- !,to_tlog(MD,KB,XF, HH).
to_tlog(MD,KB, n(XF),  n(HH)):- !,to_tlog(MD,KB,XF, HH).
to_tlog(MD,KB, neg(XF),  n(HH)):- !,to_tlog(MD,KB,XF, HH).
to_tlog(MD,KB, nesc(XF),(HH)):- !,to_tlog(MD,KB,XF, HH).

to_tlog(MD,KB,H,HH ):- H=..[F|ARGS],is_quantifier(F),!,
    append(Left,[XF],ARGS), to_tlog(MD,KB, XF, XFF),
    append(Left,[XFF],ARGSO), HH=..[XFF|ARGSO].
  
to_tlog(_,KB,H,HH ):- H=..[F,ARG],is_holds_functor(F),!,(is_ftVar(ARG)->HH=H;to_tlog(F,KB,ARG,HH)).
to_tlog(_,KB, poss(XF),  HH):- !, to_tlog(poss_t,KB, XF,  HH).
to_tlog(_,KB,H,HH):- H=..[F,ARG],to_tlog(args,KB,ARG,ARGO),HH=..[F,ARGO],!.
to_tlog(MD,KB,H,HH):- H=..[F|ARGS],must_maplist_det(to_tlog(args,KB),ARGS,ARGSO),!,to_tlog_lit(MD,KB,F,ARGSO,HH).

tlog_is_sentence_functor(F):- \+ atom(F),!,fail.
tlog_is_sentence_functor(F):- upcase_atom(F,F).
tlog_is_sentence_functor(F):- is_sentence_functor(F), \+ is_holds_functor(F).

to_tlog_lit(MD,KB,F,[ARG],HH):- (clause_b(ttExpressionType(F));atom_concat(ft,_,F)),!,to_tlog(MD,KB,quotedIsa(ARG,F ),HH).
to_tlog_lit(MD,KB,F,[ARG],HH):- (clause_b(tSet(F));(atom_concat(t,_,F), \+ atom_concat(_,'_t',F))),!,to_tlog(MD,KB,isa(ARG,F ),HH).
to_tlog_lit(MD,_ ,F,ARGSO,HHH):- is_holds_functor(F),!,HH=..[F|ARGSO],maybe_wrap_modal(MD,HH,HHH).
to_tlog_lit(MD,_ ,F,ARGSO,HHH):- get_holds_wrapper(F,W),(W==h;W==c),!,HH=..[F|ARGSO],maybe_wrap_modal(MD,HH,HHH).
to_tlog_lit(MD,_ ,F,ARGSO,HHH):- get_holds_wrapper(F,W),!,XF=..[F|ARGSO],into_ff(W,XF,HH),maybe_wrap_modal(MD,HH,HHH).
to_tlog_lit(MD,_ ,F,ARGSO,HHH):- XF=..[F|ARGSO],into_ff(MD,XF,HHH).

into_ff(MD,XF,HHH):-into_functor_form(MD,XF,HHH).

current_outer_modal_t(t).

maybe_wrap_modal(MD,HH,HH):- current_outer_modal_t(H_T),MD==H_T,!.
maybe_wrap_modal(MD,HH,HHH):- var_or_atomic(HH),HHH=..[MD,HH],!.
maybe_wrap_modal(MD,M:HH,M:HHH):-!,maybe_wrap_modal(MD,HH,HHH).
maybe_wrap_modal(MD,HH,HH):- functor(HH,MD,_).
maybe_wrap_modal(MD,HH,HHH):- HH=..[F|_],get_holds_wrapper(F,W),W==h,HHH=..[MD,HH].
maybe_wrap_modal(MD,HH,HHH):- HH=..[F|ARGS],get_holds_wrapper(F,W),W==c,atomic_list_concat([F,'_',MD],MF),HHH=..[MF|ARGS].
maybe_wrap_modal(MD,HH,HHH):- HH=..[F|ARGS],is_holds_functor(F),atom_concat(MD,F,MF),HHH=..[MF|ARGS].
maybe_wrap_modal(MD,HH,HHH):-HHH=..[MD,HH].

                                                                        
% get_holds_wrapper(isa,c).
get_holds_wrapper(isa,c).
get_holds_wrapper(arity,c).
get_holds_wrapper(reify,c).
get_holds_wrapper(skolem,c).
get_holds_wrapper(quotedIsa,c).
get_holds_wrapper(resultIsa,c).
get_holds_wrapper(quotedArgIsa,c).
get_holds_wrapper(ISA,c):- if_defined(tinyKB(isa(ISA,_)),fail).
get_holds_wrapper(IST,c):- atom_contains(IST,ist). % relationAllExists ist etc
get_holds_wrapper(IST,c):- atom_contains(IST,ist).
get_holds_wrapper(genls,c).
get_holds_wrapper(admittedArgument,c).
get_holds_wrapper(argIsa,c).
get_holds_wrapper(disjointWith,c).
%get_holds_wrapper(_,_):-!,fail.
%get_holds_wrapper(_P,t).

to_tnot_2(ThIn,ThOut):- to_tnot(ThIn,ThOut),!.

to_tnot(ThIn,ThOut):- \+ compound(ThIn),!,ThOut=ThIn.
to_tnot(~ ThIn,ThOut):- !, to_tnot( ThIn, Mid),!,to_neg(Mid,ThOut).
to_tnot(poss_t(C,I),POSCI):- atom(C),CI=..[C,I],!,to_poss(CI,POSCI).
to_tnot(poss(ThIn),ThOut):- to_poss( ThIn, ThOut),!.
to_tnot(nesc(ThIn),ThOut):- !, to_tnot( ThIn, ThOut),!.
to_tnot((X ; Y),(Xp ; Yp)) :- to_tnot(X,Xp), to_tnot(Y,Yp).
to_tnot((X :- Y),(Xp :- Yp)) :- to_tnot(X,Xp), to_tnot(Y,Yp).
to_tnot((X , Y),(Xp , Yp)) :- to_tnot(X,Xp), to_tnot(Y,Yp).
to_tnot(ThIn,nesc(ThOut)):- into_mpred_form(ThIn,ThOut).

to_neg(THIN,THIN):- \+ compound(THIN),!.
to_neg(neg(THIN),THIN).
to_neg(nesc(THIN),neg(THIN)).
to_neg(THIN,neg(THIN)).

to_poss(THIN,poss(THIN)):- \+ compound(THIN),!.
to_poss(poss(THIN),poss(THIN)).
to_poss(nesc(THIN),poss(THIN)).
to_poss(neg(THIN),poss(neg(THIN))).
to_poss(THIN,poss(THIN)).

%% no_rewrites is det.
%
% Hook To [baseKB:no_rewrites/0] For Module Common_logic_snark.
% No Rewrites.
%

baseKB:no_rewrites :- fail.




from_tlog(Var, NVar):- \+ compound(Var),!,Var=NVar.
from_tlog(Var, Var):- quietly(leave_as_is_logically(Var)),!.
from_tlog(M:H,M:HH):- !, from_tlog(H,HH).
from_tlog([H|T],[HH|TT]):- !, from_tlog(H,HH),from_tlog(T,TT).
from_tlog( t(XF),  (HH)):- !,from_tlog(XF, HH).
from_tlog( proven_not_holds_t(XF),  ~(HH)):- !,from_tlog(XF, HH).
from_tlog( proven_not_holds_t(F,A,B),  ~(t(F,A,B))):- var(F),!.
from_tlog( proven_not_t(XF),  ~(HH)):- !,from_tlog(XF, HH).
from_tlog( proven_not_t(F,A,B),  ~(t(F,A,B))):- var(F),!.
from_tlog( proven_t(XF),  (HH)):- !,from_tlog(XF, HH).
from_tlog( proven_t(F,A,B),  (t(F,A,B))):- var(F),!.
from_tlog(H,HH):- H=..[F,ARG],is_holds_functor(F),is_ftVar(ARG)->HH=H,!.
% from_tlog(H,HH):- H=..[F|ARGS],tlog_is_sentence_functor(F),!,must_maplist_det(from_tlog,ARGS,ARGSO),!,HH=..[F|ARGSO].
% from_tlog(H,HH):- H=..[F|ARGS],must_maplist_det(from_tlog,ARGS,ARGSO),must(from_tlog_lit(F,ARGSO,HH)).

from_tlog(H,H):-!.
from_tlog(H,HH):- H=..[F|ARGS],from_tlog_lit(F,ARGS,HH),!.

add_modal(t,HH,HH).
add_modal(MD,HH,HHH):- HHH=..[MD,HH].

from_tlog_lit(F,ARGSO,HHH):- F==u,HHH=..[F|ARGSO],!.
from_tlog_lit(F,ARGSO,HHH):- \+ is_holds_functor(F),!,HHH=..[F|ARGSO],!.
%from_tlog_lit(F,ARGSO,HHH):- get_holds_unwrapper(F,MD,W),!,XF=..[W|ARGSO],into_mpred_form(XF,HH),from_tlog(HH,HHHH),add_modal(MD,HHHH,HHH).
from_tlog_lit(F,ARGSO,HHH):- XF=..[F|ARGSO],into_mpred_form(XF,HHH).

%get_holds_unwrapper(F,t,t):- current_outer_modal_t(F).
%get_holds_unwrapper(FIn,MD,F):- modal_prefix(MDF,MD),atom_concat(MDF,F,FIn).


/*modal_prefix(proven_,t).
modal_prefix(holds_,t).
modal_prefix(not_,~).
modal_prefix(possible_,poss).
modal_prefix(poss_,poss).
modal_prefix(unknown_,unknown).
modal_prefix(false_,~).
*/
% PrologMUD is created to correct the mistakes we made in the projects i worked on that we forgot the funding was to create the platform/OS to run Roger Schank''s outlined software and not Doug Lenat''s software. 
% Additionally to allow it to be taken for granted by current scientists whom were unaware of the breakthroughs we made in those projects due to the fact we where affaid competetors would take our future grant money.


%% check_is_kb( ?KB) is det.
%
% Check If Is A Knowledge Base.
%
check_is_kb(KB):-attvar(KB),!.
check_is_kb(KB):-ignore('$VAR'('KB')=KB).




%% add_preconds( ?X, ?X) is det.
%
% Add Preconds.
%
add_preconds(X,X):- baseKB:no_rewrites,!.
add_preconds(X,Z):-
 locally(leave_as_is_db('CollectionS666666666666666ubsetFn'(_,_)),
   locally_tl(dont_use_mudEquals,must(defunctionalize('=>',X,Y)))),must(add_preconds2(Y,Z)).




%% add_preconds2( ?Wff6667, ?PreCondPOS) is det.
%
% Add Preconds Extended Helper.
%
add_preconds2(Wff6667,PreCondPOS):-
   must_det_l((get_lits(Wff6667,PreCond),list_to_set(PreCond,PreCondS),
     add_poss_to(PreCondS,Wff6667, PreCondPOS))).




%% get_lits( ?PQ, ?QQ) is det.
%
% Get Literals.
%
get_lits(PQ,[]):- var(PQ),!.
get_lits(PQ,QQ):- PQ=..[F,_Vs,Q],is_quantifier(F),get_lits(Q,QQ).
get_lits(OuterQuantKIF,[OuterQuantKIF]):-leave_as_is_logically(OuterQuantKIF),!.
get_lits( ~(IN),NOUT):-get_lits(IN,OUT),must_maplist_det(simple_negate_literal(not),OUT,NOUT).
get_lits(knows(WHO,IN),NOUT):-get_lits(IN,OUT),must_maplist_det(simple_negate_literal(knows(WHO)),OUT,NOUT).
get_lits(beliefs(WHO,IN),NOUT):-get_lits(IN,OUT),must_maplist_det(simple_negate_literal(beliefs(WHO)),OUT,NOUT).
get_lits(IN,OUTLF):-IN=..[F|INL],logical_functor_pttp(F),!,must_maplist_det(get_lits,INL,OUTL),flatten(OUTL,OUTLF).
get_lits(IN,[IN]).




%% simple_negate_literal( ?F, ?FX, ?X) is det.
%
% Simple Negate Literal.
%
simple_negate_literal(F,FX,X):-FX=..FXL,F=..FL,append(FL,[X],FXL),!.
simple_negate_literal(F,X,FX):-append_term(F,X,FX).



%% should_be_poss( ?VALUE1) is det.
%
% Should Be Possibility.
%
should_be_poss(argInst).

% :- was_dynamic(elInverse/2).




%% clauses_to_boxlog( ?KB, ?Why, ?In, ?Prolog) is det.
%
% Clauses Converted To Datalog.
%
clauses_to_boxlog(KB,Why,In,Prolog):- clauses_to_boxlog_0(KB,Why,In,Prolog),!.





%% clauses_to_boxlog_0( ?KB, ?Why, ?In, ?Prolog) is det.
%
% clauses Converted To Datalog  Primary Helper.
%
clauses_to_boxlog_0(KB,Why,In,Prolog):-
 loop_check(clauses_to_boxlog_1(KB,Why,In,Prolog),
    show_call(why,(clauses_to_boxlog_5(KB,Why,In,Prolog)))),!.
clauses_to_boxlog_0(KB,Why,In,Prolog):-correct_cls(KB,In,Mid),!,clauses_to_boxlog_1(KB,Why,Mid,PrologM),!,Prolog=PrologM.




%% clauses_to_boxlog_1( ?KB, ?Why, ?In, ?Prolog) is det.
%
% clauses Converted To Datalog  Secondary Helper.
%
clauses_to_boxlog_1(KB, Why,In,Prolog):- clauses_to_boxlog_2(KB,Why,In,PrologM),!,must(Prolog=PrologM).




%% clauses_to_boxlog_2( ?KB, ?Why, :TermIn, ?Prolog) is det.
%
% clauses Converted To Datalog  Extended Helper.
%
clauses_to_boxlog_2(KB, Why,In,Prolog):- is_list(In),!,must_maplist_det(clauses_to_boxlog_1(KB,Why),In,Prolog).
clauses_to_boxlog_2(KB, Why,cl([],BodyIn),  Prolog):- !, (is_lit_atom(BodyIn) -> clauses_to_boxlog_1(KB,Why,cl([inconsistentKB(KB)],BodyIn),Prolog);  (dtrace,kif_to_boxlog( ~(BodyIn),KB,Why,Prolog))).
clauses_to_boxlog_2(KB, Why,cl([HeadIn],[]),Prolog):- !, (is_lit_atom(HeadIn) -> Prolog=HeadIn ; (kif_to_boxlog(HeadIn,KB,Why,Prolog))).
clauses_to_boxlog_2(KB,_Why,cl([HeadIn],BodyIn),(HeadIn:- BodyOut)):-!, must_maplist_det(logical_pos(KB),BodyIn,Body), list_to_conjuncts_det(Body,BodyOut),!.

clauses_to_boxlog_2(KB, Why,cl([H,Head|List],BodyIn),Prolog):- 
  findall(Answer,((member(E,[H,Head|List]),delete_eq([H,Head|List],E,RestHead),
     must_maplist_det(logical_neg(KB),RestHead,RestHeadS),append(RestHeadS,BodyIn,Body),
       clauses_to_boxlog_1(KB,Why,cl([E],Body),Answer))),Prolog),!.

clauses_to_boxlog_2(_KB,_Why,(H:-B),(H:-B)):- !.




%% clauses_to_boxlog_5( ?KB, ?Why, ?In, ?Prolog) is det.
%
% Clauses Converted To Datalog Helper Number 5..
%
clauses_to_boxlog_5(KB, Why,In,Prolog):- is_list(In),!,must_maplist_det(clauses_to_boxlog_5(KB,Why),In,Prolog).
clauses_to_boxlog_5(_KB,_Why,(H:-B),(H:-B)):-!.
clauses_to_boxlog_5(_KB,_Why,cl([HeadIn],[]),HeadIn):-!.
clauses_to_boxlog_5(_KB,_Why,In,Prolog):-dtrace,In=Prolog.






%% mpred_t_tell_kif( ?OP2, ?RULE) is det.
%
% Managed Predicate True Structure Canonicalize And Store Knowledge Interchange Format.
%
mpred_t_tell_kif(OP2,RULE):-
 locally_tl(current_pttp_db_oper(mud_call_store_op(OP2)),
   (show_call(why,call((must(kif_add(RULE))))))).


:- public(why_to_id/3).



:- kb_shared(baseKB:wid/3).

%% why_to_id( ?Term, ?Wff, ?IDWhy) is det.
%
% Generation Of Proof Converted To Id.
%
why_to_id(Term,Wff,IDWhy):-  \+ atom(Term),term_to_atom(Term,Atom),!,why_to_id(Atom,Wff,IDWhy),!.
why_to_id(Atom,Wff,IDWhy):- clause_asserted(wid(IDWhy,Atom,Wff)),!.
why_to_id(Atom,Wff,IDWhy):- must(atomic(Atom)),gensym(Atom,IDWhyI),kb_incr(IDWhyI,IDWhy),
  mpred_ain(wid(IDWhy,Atom,Wff)),!.



%% kif_ask_sent( ?VALUE1) is det.
%
% Knowledge Interchange Format Complete Inference Sentence.
%
:- public(kif_ask_sent/1).
kif_ask_sent(Wff):-
   why_to_id(ask,Wff,Why),
   term_variables(Wff,Vars),
   gensym(z_q,ZQ),
   Query=..[ZQ,666|Vars],
   why_to_id(rule,'=>'(Wff,Query),Why),
   kif_to_boxlog('=>'(Wff,Query),Why,QueryAsserts),!,
   kif_add_boxes1(Why,QueryAsserts),!,
   call_cleanup(
     kif_ask(Query),
     find_and_call(retractall_wid(Why))).


:- public(kif_ask/1).


%% kif_ask( :TermP) is det.
%
% Knowledge Interchange Format Complete Inference.
%
kif_ask(Goal0):-  call_unwrap(Goal0,Goal),!,kif_ask(Goal).
%kif_ask(P <=> Q):- kif_ask_sent(P <=> Q).
%kif_ask(P => Q):- kif_ask_sent(P => Q).
%kif_ask((P v Q)):- kif_ask_sent(((P v Q))).
%kif_ask((P & Q)):- kif_ask_sent((P & Q)).
kif_ask((PQ)):- kif_hook_skel(PQ),kif_ask_sent((PQ)).
kif_ask(Goal0):-  logical_pos(_KB,Goal0,Goal),
    no_repeats(baseKB:(
	if_defined(add_args(Goal0,Goal,_,_,[],_,_,[],[],DepthIn,DepthOut,[PrfEnd|PrfEnd],_ProofOut1,Goal1,_)),!,
        call(call,search(Goal1,60,0,1,3,DepthIn,DepthOut)))).

:- public(kif_ask/2).



%% kif_ask( ?VALUE1, ?VALUE2) is det.
%
% Knowledge Interchange Format Complete Inference.
%
kif_ask(Goal0,ProofOut):- logical_pos(_KB,Goal0,Goal),
    no_repeats(baseKB:(
	if_defined(add_args(Goal0,Goal,_,_,[],_,_,[],[],DepthIn,DepthOut,[PrfEnd|PrfEnd],ProofOut1,Goal1,_)),!,
        call(call,search(Goal1,60,0,1,3,DepthIn,DepthOut)),
        call(call,contract_output_proof(ProofOut1,ProofOut)))).





call_unwrap(WffIn,OUT):- call_unwrap0(WffIn,OUT),!,WffIn\==OUT.

call_unwrap0(WffIn,WffIn):- is_ftVar(WffIn),!.
call_unwrap0(==>WffIn,OUT):-!,call_unwrap0(WffIn,OUT).
call_unwrap0(clif(WffIn),OUT):-!,call_unwrap0(WffIn,OUT).
call_unwrap0(WffIn,WffIn).




%% local_sterm_to_pterm( ?Wff, ?WffO) is det.
%
% Local Sterm Converted To Pterm.
%
local_sterm_to_pterm(Wff,WffO):- sexpr_sterm_to_pterm(Wff,WffO),!.



:- op(1000,fy,(kif_add)).
%% kif_add( ?InS) is det.
%
% Knowledge Interchange Format Add.
%
kif_add(InS):-
 sanity( \+ is_ftVar(InS)),
 string(InS),!,
 must_det_l((
  input_to_forms(string(InS),Wff,Vs),
  nop(b_implode_varnames0(Vs)),
  local_sterm_to_pterm(Wff,Wff0))),
  InS \=@= Wff0,
  kif_add(Wff0),!.

kif_add(Goal0):-  call_unwrap(Goal0,Goal),Goal0\=@=Goal,!,kif_add(Goal).
kif_add([]).
kif_add([H|T]):- !,kif_add(H),kif_add(T).
kif_add((H <- B)):- !, ain((H <- B)).
kif_add((H :- B)):- !, ain((H :- B)).
kif_add((P ==> Q)):- !, ain((P ==> Q)). 
%kif_add(Call):- kif_to_callable(Call,Prolog),is_static_predicate(Prolog),!,kif_process(call_u,Prolog).
kif_add(WffIn):- kif_hook(WffIn),!,
  kif_add2(WffIn),
  test_boxlog(WffIn).
kif_add(WffIn):- kif_add2(WffIn).


kif_add2(WffIn):- 
 pretty_numbervars_ground(WffIn,WffIn0),
 add_history(kif_to_boxlog(WffIn0)),
 show_call(ain(clif(WffIn))),!,
 kif_to_boxlog(WffIn).

% unnumbervars_with_names(WffIn,Wff),
%kif_add(WffIn):- show_call(ain(clif(WffIn))),!.

kif_ain(InS):-kif_add(InS).
kif_assert(InS):-kif_add(InS).

/*
:- public((kif_add)/2).

kif_add(_,[]).
kif_add(Why,[H|T]):- !,must_det_l((kif_add(Why,H),kb_incr(Why,Why2),kif_add(Why2,T))).
kif_add(Why,P):- !, mpred_ain(P,Why). 
kif_add(Why,(H <- B)):- !, mpred_ain((H <- B),Why).
%kif_add(Why,(P ==> Q)):- !, ain((P ==> Q),Why). 


kif_add(Why,Wff):-
   must_det_l((kif_to_boxlog(Wff,Why,Asserts),
      kif_add_boxes(assert_wfs_def,Why,Wff,Asserts))),!.


:- thread_local(t_l:assert_wfs/2).
assert_wfs_def(HBINFO,HB):-if_defined(t_l:assert_wfs(HBINFO,HB)),!.
assert_wfs_def(Why,H):-assert_wfs_fallback(Why,H).

assert_wfs_fallback(Why, HB):- subst(HB,(~),(-),HB2),subst(HB2,(not_proven_t),(not_true_t),HB1),subst(HB1,(poss),(possible_t),HBO),assert_wfs_fallback0(Why, HBO).
assert_wfs_fallback0(Why,(H:-B)):- adjust_kif('$VAR'(KB),B,HBK),to_modal1('$VAR'(KB),HBK,HBKD),
   wdmsg((H:-w_infer_by(Why),HBKD)),pttp_assert_wid(Why,pttp_in,(H:-B)),!.
assert_wfs_fallback0(Why, HB):- adjust_kif('$VAR'(KB),HB,HBK),to_modal1('$VAR'(KB),HBK,HBKD),
   wdmsg((HBKD:-w_infer_by(Why))),pttp_assert_wid(Why,pttp_in,(HB)),!.

*/

:- public(kb_incr/2).



%% kb_incr( ?WffNum1, ?WffNum2) is det.
%
% Knowledge Base Incr.
%
kb_incr(WffNum1 ,WffNum2):-is_ftVar(WffNum1),trace_or_throw(kb_incr(WffNum1 ,WffNum2)).
kb_incr(WffNum1 ,WffNum2):-number(WffNum1),WffNum2 is WffNum1 + 1,!.
%kb_incr(WffNum1 ,WffNum2):-atom(WffNum1),WffNum2=..[WffNum1,0],!.
kb_incr(WffNum1 ,WffNum2):-atomic(WffNum1),WffNum2 = WffNum1:0,!.
kb_incr(WffNum1 ,WffNum2):-WffNum1=..[F,P,A|REST],kb_incr(A ,AA),!,WffNum2=..[F,P,AA|REST].
kb_incr(WffNum1 ,WffNum2):-trace_or_throw(kb_incr(WffNum1 ,WffNum2)).
/*
kif_add_boxes(How,Why,Wff0,Asserts0):-
 must_det_l((
  show_failure(why,kif_unnumbervars(Asserts0+Wff0,Asserts+Wff)),
  %fully_expand(Get1,Get),
  get_constraints(Wff,Isas),
  kif_add_adding_constraints(Why,Isas,Asserts))),
   findall(HB-WhyHB,retract(t_l:in_code_Buffer(HB,WhyHB,_)),List),
   list_to_set(List,Set),
   forall(member(HB-WhyHB,Set),
      call(How,WhyHB,HB)).
*/




%% kif_add_adding_constraints( ?Why, ?Isas, :TermGet1Get2) is det.
%
% Knowledge Interchange Format Add Adding Constraints.
%
kif_add_adding_constraints(Why,Isas,Get1Get2):- var(Get1Get2),!,trace_or_throw(var_kif_add_isa_boxes(Why,Isas,Get1Get2)).
kif_add_adding_constraints(Why,Isas,(Get1,Get2)):- !,kif_add_adding_constraints(Why,Isas,Get1),kb_incr(Why,Why2),kif_add_adding_constraints(Why2,Isas,Get2).
kif_add_adding_constraints(Why,Isas,[Get1|Get2]):- !,kif_add_adding_constraints(Why,Isas,Get1),kb_incr(Why,Why2),kif_add_adding_constraints(Why2,Isas,Get2).
kif_add_adding_constraints(_,_,[]).
kif_add_adding_constraints(_,_,z_unused(_)):-!.
kif_add_adding_constraints(Why,Isas,((H:- B))):- conjoin(Isas,B,BB), kif_add_boxes1(Why,(H:- BB)).
kif_add_adding_constraints(Why,Isas,((H))):- kif_add_boxes1(Why,(H:- Isas)).




%% kif_add_boxes1( ?Why, ?List) is det.
%
% Knowledge Interchange Format Add Boxes Secondary Helper.
%
kif_add_boxes1(_,[]).
kif_add_boxes1(Why,List):- is_list(List),!,list_to_set(List,[H|T]),must_det_l((kif_add_boxes1(Why,H),kb_incr(Why,Why2),kif_add_boxes1(Why2,T))).
kif_add_boxes1(_,z_unused(_)):-!.
kif_add_boxes1(Why,AssertI):- must_det_l((simplify_bodies(AssertI,AssertO),kif_add_boxes3(save_wfs,Why,AssertO))).

:- thread_local(t_l:in_code_Buffer/3).





%% kif_add_boxes3( :PRED2How, ?Why, ?Assert) is det.
%
% Knowledge Interchange Format Add Boxes3.
%
kif_add_boxes3(How,Why,Assert):-
  must_det_l((
  boxlog_to_pfc(Assert,Prolog1),
  defunctionalize(Prolog1,Prolog2),
  kif_unnumbervars(Prolog2,PTTP),
  call(How,Why,PTTP))).




%% kif_unnumbervars( ?X, ?YY) is det.
%
% Knowledge Interchange Format Unnumbervars.
%
kif_unnumbervars(X,YY):-unnumbervars(X,YY),!.
kif_unnumbervars(X,YY):-
 must_det_l((
   with_output_to(string(A),write_term(X,[character_escapes(true),ignore_ops(true),quoted(true)])),
   atom_to_term(A,Y,NamedVars),
   YY=Y,
   add_newvars(NamedVars))).





%% simplify_bodies( ?B, ?BC) is det.
%
% Simplify Bodies.
%
simplify_bodies((H:- B),(H:- BC)):- must_det_l((conjuncts_to_list_det(B,RB),simplify_list(_KB,RB,BB),list_to_conjuncts_det(BB,BC))).
simplify_bodies((B),(BC)):- must_det_l((conjuncts_to_list_det(B,RB),simplify_list(_KB,RB,BB),list_to_conjuncts_det(BB,BC))).





%% simplify_list( ?KB, ?RB, ?BBS) is det.
%
% Simplify List.
%
simplify_list(KB,RB,BBS):- list_to_set(RB,BB),must_maplist_det(removeQ(KB),BB,BBO),list_to_set(BBO,BBS).




%% save_wfs( ?Why, ?PrologI) is det.
%
% Save Well-founded Semantics Version.
%
save_wfs(Why,PrologI):- 
 must_det_l((baseKB:as_prolog_hook(PrologI,Prolog),
   \+ \+
    ( b_setval('$current_why',wp(Why,Prolog)),
      ain_h(save_in_code_buffer,Why,Prolog)))).




%% nots_to( ?H, ?To, ?HH) is det.
%
% Negations Converted To.
%
nots_to(H,To,HH):-subst_except(H,not,To,HH),subst_except(H,-,To,HH),subst_except(H,~,To,HH),subst_except(H, \+ ,To,HH),!.



%% neg_h_if_neg( ?H, ?HH) is det.
%
% Negated Head If Negated.
%
neg_h_if_neg(H,HH):-nots_to(H,'~',HH).



%% neg_b_if_neg( ?HBINFO, ?B, ?BBB) is det.
%
% Negated Backtackable If Negated.
%
neg_b_if_neg(HBINFO,B,BBB):-nots_to(B,'~',BB),sort_body(HBINFO,BB,BBB),!.


%% simp_code( ?A, ?A) is det.
%
% Simp Code.
%
simp_code(HB,(H:-BS)):-expand_to_hb(HB,H,B),conjuncts_to_list_det(B,BL),sort(BL,BSL),!,list_to_conjuncts_det(BSL,BS).
simp_code(A,A).





%% var_count_num( ?Term, ?SharedTest, ?SharedCount, ?UnsharedCount) is det.
%
% Variable Count Num.
%
var_count_num(Term,SharedTest,SharedCount,UnsharedCount):- term_slots(Term,Slots),term_slots(SharedTest,TestSlots),
  subtract(Slots,TestSlots,UnsharedSlots),
  subtract(Slots,UnsharedSlots,SharedSlots),
  length(SharedSlots,SharedCount),
  length(UnsharedSlots,UnsharedCount).




%% ain_h( :PRED2How, ?Why, ?H) is det.
%
% Assert If New Head.
%
ain_h(How,Why,(H:- B)):- neg_h_if_neg(H,HH), neg_b_if_neg((HH:- B),B,BB),!,call(How,Why,(HH:-BB)).
ain_h(How,Why,(H)):- neg_h_if_neg(H,HH), call(How,Why,(HH)).




%% save_in_code_buffer( ?VALUE1, ?HB) is det.
%
% Save In Code Buffer.
%
save_in_code_buffer(_ ,HB):- simp_code(HB,SIMP),t_l:in_code_Buffer(HB,_,SIMP),!.
save_in_code_buffer(Why,HB):- simp_code(HB,SIMP),assert(t_l:in_code_Buffer(HB,Why,SIMP)).




%% use_was_isa_h( ?I, :TermT, ?ISA) is det.
%
% use was  (isa/2) Head.
%
use_was_isa_h(_,ftTerm,true):- !.
use_was_isa_h(_,argi(mudEquals,_),true):- !.
use_was_isa_h(_,argi(skolem,_),true):- !.
use_was_isa_h(I,T,ISA):- to_isa_form0(I,T,ISA),!.

%% to_isa_form( ?I, ?C, ?OUT) is nondet.
%
% Converted To  (isa/2) out.
%
to_isa_form0(I,C,isa(I,C)).
to_isa_form0(I,C,t(C,I)).
to_isa_form0(I,C,OUT):- atom(C)->OUT=..[C,I].



%% generate_ante( :TermARG1, :TermARG2, ?InOut, ?InOut) is det.
%
% Generate Ante.
%
generate_ante([],[],InOut,InOut).
generate_ante([I|VarsA],[T|VarsB],In,Isas):- use_was_isa_h(I,T,ISA), conjoin(In,ISA,Mid),generate_ante(VarsA,VarsB,Mid,Isas).




%% get_constraints( ?ListA, ?Isas) is det.
%
% Get Constraints.
%
get_constraints(T,true):- T==true.
get_constraints(_,true):- !.
get_constraints(ListA,Isas):-
     must_det_l((copy_term(ListA,ListB),
      term_variables(ListA,VarsA),
      term_variables(ListB,VarsB),
      attempt_attribute_args(isAnd,ftAskable,ListB),
      attribs_to_atoms(VarsB,VarsB),
      generate_ante(VarsA,VarsB,true,Isas))).



:- source_location(S,_),forall((source_file(H,S),once((clause(H,B),B\=true))),(functor(H,F,A),module_transparent(F/A))).


:- fixup_exports.

