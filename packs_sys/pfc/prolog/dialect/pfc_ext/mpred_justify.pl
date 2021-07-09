/* Part of LogicMOO Base Logicmoo Debug Tools
% ===================================================================
%   File   : mpred_why.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Updated:
%   Purpose: predicates for interactively exploring Pfc justifications.

% ***** predicates for brousing justifications *****
% ===================================================================
*/

%:- if(( ( \+ ((current_prolog_flag(logicmoo_include,Call),Call))) )).

%:- throw(module(pfcumt,[umt/1])).

:- if(current_prolog_flag(xref,true)).  % XREF
:- module(mpred_justify, []).
:- set_module(class(library)).
:- endif.

%:- use_module(mpred_kb_ops).
%:- use_module(library(util_varnames)).
%:- use_module(library(no_repeats)).

:- include('mpred_header.pi').

%:- endif.

%   File   : pfcjust.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Author :  Dave Matuszek, dave@prc.unisys.com
%   Updated:
%   Purpose: predicates for accessing Pfc justifications.
%   Status: more or less working.
%   Bugs:

%  *** predicates for exploring supports of a fact *****

justification(F,J):- supporters_list_how(F,J).

justifications(F,Js):- bagof_nr(J,justification(F,J),Js).

:- set_prolog_flag(expect_pfc_file,never).

mpred_why(M:Conseq,Ante):- atom(M),!,M:mpred_why_2(Conseq,Ante).

mpred_why(Conseq,Ante):- mpred_why_2(Conseq,Ante).

mpred_why_2(Conseq,Ante):- var(Conseq),!,mpred_children(Ante,Conseq).
mpred_why_2(Conseq,Ante):- justifications(Conseq,Ante).


mpred_info(O):-call_u(mpred_info0(O)).

mpred_info0(O):-
 with_output_to(user_error,
 ((dmsg_pretty("======================================================================="),  
  %quietly(call_with_inference_limit(ignore(on_xf_cont(deterministically_must(mpred_why_1(O)))),4000,_)),
  ignore(on_xf_cont(deterministically_must(mpred_why_1(O)))),
  dmsg_pretty("======================================================================="),
  findall(mpred_supported(Mode,O),is_tms_mode(Mode),SupportedModes),
  must_maplist(mp_printAll(O),
  [   mpred_db_type(O,v),  
      mpred_child(O,v),
      % mpred_fact(O),
      mpred_axiom(O),
      well_founded(O),
      mpred_assumption(O),
      call(listing(O)),
      get_mpred_is_tracing(O),
      get_tms_mode(O,v)|
      SupportedModes]),
 dmsg_pretty("=======================================================================")))).

mp_printAll(S,call(O)):- !, subst(O,S,s,NAME),nl,flush_output,fmt("==================\n"),
  doall(((flush_output,deterministically_must(catch(call_u(O),E,(fmt9(error(E)),fail))),flush_output)*->true;wdmsg_pfc(fail=NAME))),nl.

mp_printAll(S,+(O)):- subst(O,v,V,CALL), CALL\==O,!,
  subst(O,S,s,NAME),safe_functor(O,F,_),!,
  nl,flush_output, fmt("==================\n"),%wdmsg_pfc(NAME),wdmsg_pfc("---"),flush_output,!,
  doall(((flush_output,call_u(CALL),flush_output)*->fmt9(F=V);(fmt9(fail=NAME)))),fresh_line,fmt("==================\n"),flush_output.

mp_printAll(S,(O)):- subst(O,v,V,CALL),CALL\==O,!, subst(O,S,s,NAME),safe_functor(O,F,_),
  format('~N',[]),flush_output, fmt("==================\n"),
  doall(((flush_output,call_u(CALL),flush_output)*->fmt9(F=V);fmt9(fail=NAME))),nl.

mp_printAll(S,(O)):-  !,  safe_functor(O,F,A),mp_nnvv(S,O,F,A),flush_output.

mp_nnvv(_,(O),F,1):- !, doall(((flush_output,deterministically_must(call_u(O)),flush_output)*->wdmsg_pfc(+F);wdmsg_pfc(-F))).
mp_nnvv(S,(O),_,_):- !, subst(O,S,s,NAME), !,
  doall(((flush_output,deterministically_must(call_u(O)),flush_output)*->wdmsg_pfc(-NAME);wdmsg_pfc(+NAME))).






%%  mpred_basis_list(+P,-L)
%
%  is true iff L is a list of "base" facts which, taken
%  together, allows us to deduce P.  A mpred "based on" list fact is an axiom (a fact
%  added by the user or a raw Prolog fact (i.e. one w/o any support))
%  or an assumption.
%
mpred_basis_list(F,[F]):- (mpred_axiom(F) ; mpred_assumption(F)),!.

mpred_basis_list(F,L):-
  % i.e. (reduce 'append (map 'mpred_basis_list (justification f)))
  justification(F,Js),
  bases_union(Js,L).


%%  bases_union(+L1,+L2).
%
%  is true if list L2 represents the union of all of the
%  facts on which some conclusion in list L1 is based.
%
bases_union([],[]).
bases_union([X|Rest],L):-
  mpred_basis_list(X,Bx),
  bases_union(Rest,Br),
  mpred_union(Bx,Br,L).

%mpred_axiom(F):- !, % Like OLD TODO
%  mpred_get_support(F,(_,ax)).
mpred_axiom(F):-
  mpred_get_support(F,UU),
  is_user_reason(UU),!.

%% mpred_assumption(P)
%
%  an mpred_assumption is a failed goal, i.e. were assuming that our failure to
%  prove P is a proof of not(P)
%
mpred_assumption(P):- !, % Like OLD TODO
  nonvar(P), mpred_unnegate(P,_).
mpred_assumption(P):- nonvar(P), 
  mpred_unnegate(P,N), 
 % fail,
  % added prohibited_check
  (current_prolog_flag(explicitly_prohibited_check,false) -> true ; \+ mpred_axiom(~ N)).

:- set_prolog_flag(explicitly_prohibited_check,false).

%% mpred_assumptions( +X, +AsSet) is semidet.
%
% true if AsSet is a set of assumptions which underly X.
%
mpred_assumptions(X,[X]):- mpred_assumption(X).
mpred_assumptions(X,[]):- mpred_axiom(X).
mpred_assumptions(X,L):-
  justification(X,Js),
  do_assumpts(Js,L).


%% do_assumpts(+Set1,?Set2) is semidet.
%
% Assumptions Secondary Helper.
%
do_assumpts([],[]).
do_assumpts([X|Rest],L):-
  mpred_assumptions(X,Bx),
  do_assumpts(Rest,Br),
  mpred_union(Bx,Br,L).


%  mpred_proofTree(P,T) the proof tree for P is T where a proof tree is
%  of the form
%
%      [P , J1, J2, ;;; Jn]         each Ji is an independent P justifier.
%           ^                         and has the form of
%           [J11, J12,... J1n]      a list of proof trees.


%% mpred_child(+P,?Q) is semidet.
%
% is true iff P is an immediate justifier for Q.
%
mpred_child(P,Q):- is_list(P),!,maplist(mpred_child,P,Q).
mpred_child(P,Q):-
  mpred_get_support(Q,(P,_)).
mpred_child(P,Q):-
  mpred_get_support(Q,(_,Trig)),
  mpred_db_type(Trig,trigger(_TT)),
  mpred_child(P,Trig).


%% mpred_children(+P, ?L) is semidet.
%
% PFC Children.
%
mpred_children(P,L):- bagof_nr(C,mpred_child(P,C),L).



%% mpred_descendant(+P, ?Q) is semidet.
%
% mpred_descendant(P,Q) is true iff P is a justifier for Q.
%
mpred_descendant(P,Q):-
   mpred_descendant1(P,Q,[]).


%% mpred_descendant1(+P, ?Q, ?Seen) is semidet.
%
% PFC Descendant Secondary Helper.
%
mpred_descendant1(P,Q,Seen):-
  mpred_child(X,Q),
  (\+ member(X,Seen)),
  (P=X ; mpred_descendant1(P,X,[X|Seen])).


%% mpred_descendants(+P, ?L) is semidet.
%
% PFC Descendants.
%
mpred_descendants(P,L):-
  bagof_nr(Q,mpred_descendant1(P,Q,[]),L).

:- meta_predicate(bagof_nr(?,^,*)).
bagof_nr(T,G,B):- no_repeats(B,(bagof(T,G,B))).
:- meta_predicate(bagof_or_nil(?,^,-)).
bagof_or_nil(T,G,B):- (bagof_nr(T,G,B) *-> true; B=[]).


:- meta_predicate(sanity_check(0,0)).
sanity_check(When,Must):- notrace(catch((When,Must),_,fail)),!.
sanity_check(When,Must):- show_call(must_or_rtrace(When)),!,must_or_rtrace(Must),!.

%
%  predicates for manipulating support relationships
%

notify_if_neg_trigger('$spft'(_MZ,P,Fact,Trigger)):- 
  (Trigger= '$nt'(F,Condition,Action) ->
    (mpred_trace_msg('~N~n\tAdding NEG mpred_do_fcnt via support~n\t\ttrigger: ~p~n\t\tcond: ~p~n\t\taction: ~p~n\t from: ~p~N',
      [F,Condition,Action,mpred_add_support_fast(P,(Fact,Trigger))]));true).

%  mpred_add_support(+Fact,+Support)
mpred_add_support(P,(Fact,Trigger)):-
  MSPFT = '$spft'(MZ,P,Fact,Trigger),
   fix_mp("mpred_add_support",MSPFT,M,SPFT),
   must(MZ=M),
   M:notify_if_neg_trigger(SPFT),
  M:(clause_asserted_u(SPFT)-> true; 
  sanity_check(assertz_mu(M:SPFT),call(M:clause_asserted(M:SPFT)))),!.

%  mpred_add_support_fast(+Fact,+Support)
mpred_add_support_fast(P,(Fact,Trigger)):-
  must_or_rtrace((    
   fix_mp("mpred_add_support_fast",MSPFT,M,SPFT),
   MSPFT = '$spft'(M,P,Fact,Trigger),
   M:notify_if_neg_trigger(SPFT),
   sanity_check(M:assertz_mu(M:SPFT),call(M:clause_asserted(M:SPFT))))),!.


                                                                
:- meta_predicate(mpred_get_support(*,-)).

mpred_get_support((H:-B),(Fact,Trigger)):- 
   lookup_u('$spft'(MZ,(H <- B),_,_),Ref),
   clause('$spft'(MZ,HH<-BB,Fact,Trigger),true,Ref),
   clause_ref_module(Ref),   
   H=@=HH,B=@=BB.
mpred_get_support(P,(Fact,Trigger)):-
  lookup_spft(P,Fact,Trigger).


mpred_get_support_why(P,FT):-
  (mpred_get_support_perfect(P,FT)*->true;
   (mpred_get_support_deeper(P,FT))).

mpred_get_support_perfect(P,(Fact,Trigger)):-
   lookup_spft_match_first(P,Fact,Trigger).

mpred_get_support_deeper((H:-B),(Fact,Trigger)):- (nonvar(H) -> ! ; true),
 lookup_u('$spft'(MZ,(H <- B),_,_),Ref),
  clause('$spft'(MZ,HH<-BB,Fact,Trigger),true,Ref),
  H=@=HH,B=@=BB.

mpred_get_support_deeper(P,(Fact,Trigger)):-
    lookup_spft_match_deeper(P,Fact,Trigger).

lookup_spft_match(A,B,C):- copy_term(A,AA),lookup_spft(A,B,C),A=@=AA.

lookup_spft_match_deeper(H,Fact,Trigger):- 
  copy_term(H,HH),
  lookup_spft((H:- _B),Fact,Trigger),
  H=@=HH.

lookup_spft_match_first(A,B,C):- nonvar(A),!, 
  no_repeats(((lookup_spft_match(A,B,C);lookup_spft(A,B,C)))).

lookup_spft_match_first(A,B,C):- lookup_spft(A,B,C).


lookup_spft(A,B,C):- !, get_mz(MZ), lookup_u('$spft'(MZ,A,B,C)).
% cutted above
/*
lookup_spft(A,B,C):- nonvar(A),!,lookup_spft_p(A,B,C).
lookup_spft(A,B,C):- var(B),!,lookup_spft_t(A,B,C).
lookup_spft(A,B,C):- lookup_spft_f(A,B,C).

lookup_spft_p(A,B,C):- with_some_vars_locked(A,lookup_u('$spft'(MZ,A,B,C))).
% TODO UNCOMMENT MAYBE IF NEEDED lookup_spft_p(A,B,C):- full_transform(lookup,A,AA),!,A\=@=AA,!,show_mpred_success(baseKB:'$spft'(MZ,AA,B,C)).

lookup_spft_f(A,B,C):- with_some_vars_locked(B,lookup_u('$spft'(MZ,A,B,C))).
% TODO UNCOMMENT MAYBE IF NEEDED lookup_spft_f(A,B,C):- full_transform(lookup,B,BB),!,B\=@=BB,!,show_mpred_success(baseKB:'$spft'(MZ,A,BB,C)).

lookup_spft_t(A,B,C):- lookup_u('$spft'(MZ,A,B,C)).
*/
/*
%  TODO MAYBE
mpred_get_support(F,J):-
  full_transform(mpred_get_support,F,FF),!,
  F\==FF,mpred_get_support(FF,J).
*/

mpred_rem_support_if_exists(P,(Fact,Trigger)):-
  lookup_spft(P,Fact,Trigger),
  get_mz(MZ),
  mpred_retract_i_or_warn('$spft'(MZ,P,Fact,Trigger)).


mpred_rem_support(P,(Fact,Trigger)):-
  get_mz(MZ),
  closest_u('$spft'(MZ,P,Fact,Trigger),'$spft'(MZ,P,FactO,TriggerO)),
  mpred_retract_i_or_warn_1('$spft'(MZ,P,FactO,TriggerO)).
mpred_rem_support(P,S):-
  get_mz(MZ),
  mpred_retract_i_or_warn('$spft'(MZ,P,Fact,Trigger)),
  ignore((Fact,Trigger)=S).



closest_u(Was,WasO):-clause_asserted_u(Was),!,Was=WasO.
closest_u(Was,WasO):-lookup_u(Was),!,Was=WasO,!.
closest_u(Was,WasO):-lookup_u(WasO),ignore(Was=WasO),!.
closest_u(H,HH):- ref(_) = Result,closest_uu(H,H,HH,Result),ref(Ref)= Result,
  (H==HH -> true ; nonvar(Ref)),!.

closest_uu(H,P,PP):- copy_term(H+P,HH+PP),
      ((lookup_u(HH)*-> (=@=(P,PP)->(!,HH=H);(fail));(!,fail));(true)).
closest_uu(H,P,PP,Result):-
      sanity(Result=@=ref(Ref)),
      (copy_term(H+P,HH+PP),
      ((lookup_u(HH,Ref)*-> (=@=(P,PP)->(!,HH=H);
          (nb_setarg(1,Result,Ref),fail));(!,fail));((clause(HH,B,Ref),must_ex(B))))).

/*
*/

mpred_collect_supports(Tripples):-
  bagof_or_nil(Tripple, mpred_support_relation(Tripple), Tripples).

mpred_support_relation((P,F,T)):- lookup_spft(P,F,T).

mpred_make_supports((P,S1,S2)):-
  mpred_add_support(P,(S1,S2)),
  (mpred_ain_object(P); true),
  !.


pp_why:-mpred_why.

mpred_why:-
  call(t_l:whybuffer(P,_)),
  mpred_why_1(P).

pp_why(A):-mpred_why_1(A).

clear_proofs:- retractall(t_l:whybuffer(_P,_Js)),color_line(cyan,1).


:- thread_local(t_l:shown_why/1).

% see pfc_why

:- export(with_no_english/1).
:- meta_predicate(with_no_english(*)).
with_no_english(Goal):- setup_call_cleanup(flag('english', Was, 0),Goal,flag('english', _, Was )).

mpred_why(P):- clear_proofs,!,with_no_english((must(mpred_why_1(P)))).

mpred_why_1(M:P):-  atom(M),!,call_from_module(M,mpred_why_1(P)).
mpred_why_1(NX):- number(NX),!, trace, pfcWhy0(NX),!.
mpred_why_1(P):- is_list(P), !, maplist(mpred_why_1, P).
mpred_why_1(P):- ((callable(P), ((must_ex((mpred_why_justs(P))))))) *-> true ; mpred_why_1_fallback(P).

mpred_why_1_fallback(NX):-  
  (number(NX)-> true ; clear_proofs),
  nop(trace),
  pfcWhy0(NX),!.
mpred_why_1_fallback(P):- mpred_why_sub(P).

% mpred_why_1(N):- number(N),!, call(t_l:whybuffer(P,Js)), mpred_handle_why_command(N,P,Js).

mpred_why_justs(P):- mpred_why_justs_1a(P)*->true;forall(mpred_why_justs_1b(P),true).

/*
mpred_why_justs_1a(P) :-    
  color_line(green,2),!,
  findall(Js,((no_repeats(P-Js,(justifications(P,Js))),
    must((color_line(yellow,1),
      ignore(pfcShowJustifications(P,Js)))))),Count),
  (Count==[]-> format("~N No justifications for ~p. ~n~n",[P]) ; true),
  color_line(green,2).
*/
mpred_why_justs_1a(P) :-    
  color_line(green,2),
  Found=fnd(0),!,
  forall(justifications(P,Js),
    (color_line(yellow,1),
     nb_setarg(1,Found,1),
     pfcShowJustifications(P,Js))),
  (Found==fnd(0)-> format("~N No justifications for ~p. ~n~n",[P]) ; true),
  color_line(green,2),!,
  Found\==fnd(0).

mpred_why_justs_1b(P) :- term_variables(P,VarsPC), 
  ( (call_u_no_bc(P),mpred_why_justs_1a(P))
   *-> (term_variables(P,VarsAC),(VarsPC==VarsAC->!;true))
  ; mpred_why_justs_1a(P)).

/*
mpred_why_justs_2(P) :-    
  color_line(green,2),!,
  findall(Js,((no_repeats(P-Js,deterministically_must(justifications(P,Js))),
    must((color_line(yellow,1),
      ignore(pfcShowJustifications(P,Js)))))),Count),
  (Count==[]-> format("~N No justifications for ~p. ~n~n",[P]) ; true),
  color_line(green,2).
*/
/*

mpred_why_1(P):- loop_check(quietly_ex((must_ex(mpred_why_try_each(P)),color_line(green,2))),true).

% user:mpred_why_1((user:prolog_exception_hook(A, B, C, D) :- exception_hook(A, B, C, D))).
% mpred_why_1((prolog_exception_hook(A, B, C, D) :- exception_hook(A, B, C, D))).

mpred_why_try_each(MN):- strip_module(MN,_,N),number(N),!,pfcWhy0(N),!.

mpred_why_try_each(ain(H)):-!,mpred_why_try_each(H).
mpred_why_try_each(call_u(H)):-!,mpred_why_try_each(H).
mpred_why_try_each(clause(H,B)):-!,mpred_why_try_each(H:-B).
mpred_why_try_each(clause(H,B,_)):-!,mpred_why_try_each(H:-B).
mpred_why_try_each(clause_u(P)):-!,mpred_why_try_each(P).
mpred_why_try_each(clause_u(H,B)):-!,mpred_why_try_each(H:-B).
mpred_why_try_each(clause_u(H,B,_)):-!,mpred_why_try_each(H:-B).

mpred_why_try_each(P):- once((retractall(t_l:whybuffer(P,_)),color_line(green,2),
    show_current_source_location,format("~NJustifications for ~p:",[P]))),
    fail.

mpred_why_try_each(P):- mpred_why_try_each_0(P),!.
mpred_why_try_each(P):- mpred_why_sub(P),!.
mpred_why_try_each(M:P :- B):- atom(M),call_from_module(M,mpred_why_try_each_0(P:-B)),!.
mpred_why_try_each(M:P):- atom(M),call_from_module(M,mpred_why_try_each_0(P)),!.
mpred_why_try_each(P :- B):- is_src_true(B),!,mpred_why_try_each(P ).
mpred_why_try_each(M:H):- strip_module(H,Ctx,P),P==H,Ctx==M,!,mpred_why_try_each(H).
mpred_why_try_each(_):- format("~N No justifications. ~n").

mpred_why_try_each_0(P):- findall(Js,mpred_why_try_each_1(P,Js),Count),Count\==[],!.
mpred_why_try_each_0(\+ P):- mpred_why_try_each_0(~P)*->true;(call_u(\+ P),wdmsgl(why:- \+ P)),!.

mpred_why_try_each_1(P,Js):-
  ((no_repeats(P-Js,deterministically_must(justifications(P,Js))),
    ((color_line(yellow,1), pfcShowJustifications(P,Js))))).
mpred_why_try_each_1(\+ P,[MFL]):- !, find_mfl(P,MFL),ansi_format([fg(cyan)],"~N    ~q",[MFL]),fail.
mpred_why_try_each_1( P,[MFL]):-  find_mfl(P,MFL), \+ clause_asserted(t_l:shown_why(MFL)), ansi_format([fg(cyan)],"~N    ~q",[MFL]).

*/
pfcWhy0(N) :-
  number(N),
  !,
  t_l:whybuffer(P,Js),
  pfcWhyCommand0(N,P,Js).

pfcWhy0(P) :-
  justifications(P,Js),  
  assert(t_l:whybuffer(P,Js)),                     
  pfcWhyBrouse(P,Js).

pfcWhy1(P) :-
  justifications(P,Js),
  pfcWhyBrouse(P,Js).

pfcWhyBrouse(P,Js) :-    % non-interactive
  pfcShowJustifications(P,Js),source_file(_,_),!.

pfcWhyBrouse(P,Js) :- 
  pfcShowJustifications(P,Js),
  ttyflush,
  read_pending_chars(current_input,_,[]),!,
  ttyflush,
  % pfcAsk(' >> ',Answer),
  % read_pending_chars(current_input,[Answer|_],[]),!,  
  format('~N',[]),write('proof [q/h/u/?.?]: '),get_char(Answer),
  pfcWhyCommand0(Answer,P,Js).

pfcWhyCommand0(q,_,_) :- !.
pfcWhyCommand0(h,_,_) :- 
  !,
  format("~n
Justification Brouser Commands:
 q   quit.
 N   focus on Nth justification.
 N.M brouse step M of the Nth justification
 u   up a level
",
 []).

pfcWhyCommand0(N,_P,Js) :-
  float(N),
  !,
  pfcSelectJustificationNode(Js,N,Node),
  pfcWhy1(Node).

pfcWhyCommand0(u,_,_) :-
  % u=up
  !.

pfcWhyCommand0(N,_,_) :-
  integer(N),
  !,
  format("~n~w is a yet unimplemented command.",[N]),
  fail.

pfcWhyCommand0(X,_,_) :-
 format("~n~w is an unrecognized command, enter h. for help.",[X]),
 fail.

reset_shown_justs:- retractall(t_l:shown_why(_)),color_line(red,1).
  
pfcShowJustifications(P,Js) :-
  show_current_source_location,
  reset_shown_justs,
  color_line(yellow,1),
  format("~N~nJustifications for ~p:~n",[P]),  

  pfcShowJustification1(Js,1),!.

pfcShowJustification1([],_):-!.
pfcShowJustification1([J|Js],N) :- !,
  % show one justification and recurse.    
  reset_shown_justs,
  pfcShowSingleJust(N,step(1),J),!,
  N2 is N+1,  
  pfcShowJustification1(Js,N2).

pfcShowJustification1(J,N) :- 
  reset_shown_justs, % nl,
  pfcShowSingleJust(N,step(1),J),!.

incrStep(StepNo,Step):-arg(1,StepNo,Step),X is Step+1,nb_setarg(1,StepNo,X).

pfcShowSingleJust(JustNo,StepNo,C):- is_ftVar(C),!,incrStep(StepNo,Step),
  color_format([fg(cyan)],"~N    ~w.~w ~w ",[JustNo,Step,C]),!.
pfcShowSingleJust(_JustNo,_StepNo,[]):-!.
pfcShowSingleJust(JustNo,StepNo,(P,T)):-!, 
  pfcShowSingleJust(JustNo,StepNo,P),
  pfcShowSingleJust(JustNo,StepNo,T).
pfcShowSingleJust(JustNo,StepNo,(P,F,T)):-!, 
  pfcShowSingleJust1(JustNo,StepNo,P),
  pfcShowSingleJust(JustNo,StepNo,F),
  pfcShowSingleJust1(JustNo,StepNo,T).
pfcShowSingleJust(JustNo,StepNo,(P*->T)):-!, 
  pfcShowSingleJust1(JustNo,StepNo,P),format('      *-> ',[]),
  pfcShowSingleJust1(JustNo,StepNo,T).

pfcShowSingleJust(JustNo,StepNo,(P:-T)):-!, 
  pfcShowSingleJust1(JustNo,StepNo,P),format(':- ~p.',[T]).
 
pfcShowSingleJust(JustNo,StepNo,(P : -T)):-!, 
  pfcShowSingleJust1(JustNo,StepNo,P),format('      :- ',[]),
  pfcShowSingleJust(JustNo,StepNo,T).

pfcShowSingleJust(JustNo,StepNo,(P :- T) ):- !, 
  pfcShowSingleJust1(JustNo,StepNo,call(T)),  
  pfcShowSingleJust1(JustNo,StepNo,P).


pfcShowSingleJust(JustNo,StepNo,[P|T]):-!, 
  pfcShowSingleJust(JustNo,StepNo,P),
  pfcShowSingleJust(JustNo,StepNo,T).

pfcShowSingleJust(JustNo,StepNo,'$pt'(MZ,P,Body)):- !, 
  pfcShowSingleJust1(JustNo,StepNo,'$pt'(MZ,P)),  
  pfcShowSingleJust(JustNo,StepNo,Body).

pfcShowSingleJust(JustNo,StepNo,C):- 
 pfcShowSingleJust1(JustNo,StepNo,C).

fmt_cl(P):- \+ \+ (numbervars(P,126,_,[attvar(skip),singletons(true)]),write_term(P,[portray(true)])).

unwrap_litr(C,CCC+VS):- copy_term(C,CC,VS),
  numbervars(CC+VS,0,_),
  unwrap_litr0(CC,CCC),!.
unwrap_litr0(call(C),CC):-unwrap_litr0(C,CC).
unwrap_litr0('$pt'(_,C),CC):-unwrap_litr0(C,CC).
unwrap_litr0(body(C),CC):-unwrap_litr0(C,CC).
unwrap_litr0(head(C),CC):-unwrap_litr0(C,CC).
unwrap_litr0(C,C).

pfcShowSingleJust1(JustNo,StepNo,C):- unwrap_litr(C,CC),!,pfcShowSingleJust4(JustNo,StepNo,C,CC).
pfcShowSingleJust4(_,_,_,CC):- t_l:shown_why(C),C=@=CC,!.
pfcShowSingleJust4(JustNo,StepNo,C,CC):- assert(t_l:shown_why(CC)),!,
   incrStep(StepNo,Step),
   color_format([fg(cyan)],"~N    ~w.~w ~@ ",[JustNo,Step,fmt_cl(C)]),   
   pfcShowSingleJust_C(C),!.

pfcShowSingleJust_C(C):-is_file_ref(C),!.
pfcShowSingleJust_C(C):-find_mfl(C,MFL),assert(t_l:shown_why(MFL)),!,pfcShowSingleJust_MFL(MFL).
pfcShowSingleJust_C(_):-color_format([hfg(black)]," % [no_mfl] ",[]),!.

pfc_short_filename(F,FN):- atomic_list_concat([_,FN],'/pack/',F),!.
pfc_short_filename(F,FN):- atomic_list_concat([_,FN],swipl,F),!.
pfc_short_filename(F,FN):- F=FN,!.

pfcShowSingleJust_MFL(MFL):- MFL=mfl4(VarNameZ,_M,F,L),atom(F),pfc_short_filename(F,FN),!,varnames_load_context(VarNameZ),
   color_format([hfg(black)]," % [~w:~w] ",[FN,L]).
pfcShowSingleJust_MFL(MFL):- color_format([hfg(black)]," % [~w] ",[MFL]),!.

pfcAsk(Msg,Ans) :-
  format("~n~w",[Msg]),
  read(Ans).

pfcSelectJustificationNode(Js,Index,Step) :-
  JustNo is integer(Index),
  nth1(JustNo,Js,Justification),
  StepNo is 1+ integer(Index*10 - JustNo*10),
  nth1(StepNo,Justification,Step).







mpred_why_maybe(_,(F:-P)):-!,wdmsgl(F:-P),!.
mpred_why_maybe(F,P):-wdmsgl(F:-P),!.
mpred_why_maybe(_,P):-ignore(mpred_why_1(P)).

mpred_why_sub(P):- nop(trace), loop_check(mpred_why_sub0(P),true).
mpred_why_sub0(P):- mpred_why_2(P,Why),!,wdmsg_pfc(:-mpred_why_1(P)),wdmsgl(mpred_why_maybe(P),Why).
mpred_why_sub0(P):-loop_check(mpred_why_sub_lc(P),trace_or_throw_ex(mpred_why_sub_lc(P)))-> \+ \+ call(t_l:whybuffer(_,_)),!.
mpred_why_sub_lc(P):- 
  justifications(P,Js),
  nb_setval('$last_printed',[]),
  clear_proofs,
  assertz(t_l:whybuffer(P,Js)),
  mpred_whyBrouse(P,Js).
  

mpred_why_sub_sub(P):-
  justifications(P,Js),
  clear_proofs,
  % retractall_u(t_l:whybuffer(_,_)),
  (nb_hasval('$last_printed',P)-> dmsg_pretty(hasVal(P)) ;
   ((
  assertz(t_l:whybuffer(P,Js)),
   nb_getval('$last_printed',LP),
   ((mpred_pp_db_justification1(LP,Js,1),fmt('~N~n',[])))))).

nb_pushval(Name,Value):-nb_current(Name,Before)->nb_setval(Name,[Value|Before]);nb_setval(Name,[Value]).
nb_peekval(Name,Value):-nb_current(Name,[Value|_Before]).
nb_hasval(Name,Value):-nb_current(Name,List),member(Value,List).
nb_popval(Name,Value):-nb_current(Name,[Value|Before])->nb_setval(Name,Before).

mpred_why1(P):-
  justifications(P,Js),
  mpred_whyBrouse(P,Js).

% non-interactive
mpred_whyBrouse(P,Js):-
   must_ex(quietly_ex(in_cmt((mpred_pp_db_justifications(P,Js))))), !.

% Interactive
mpred_whyBrouse(P,Js):-
  mpred_pp_db_justifications(P,Js),
  mpred_prompt_ask(' >> ',Answer),
  mpred_handle_why_command(Answer,P,Js).

mpred_handle_why_command(q,_,_):- !.
mpred_handle_why_command(h,_,_):-
  !,
  format("~N
Justification Brouser Commands:
 q   quit.
 N   focus on Nth justification.
 N.M brouse step M of the Nth justification
 user   up a level ~n",
  []).

mpred_handle_why_command(N,_ZP,Js):-
  float(N),
  !,
  mpred_select_justification_node(Js,N,Node),
  mpred_why1(Node).

mpred_handle_why_command(u,_,_):-
  % u=up
  !.

mpred_unhandled_command(N,_,_):-
  integer(N),
  !,
  format("~N~p is a yet unimplemented command.",[N]),
  fail.

mpred_unhandled_command(X,_,_):-
 format("~N~p is an unrecognized command, enter h. for help.",[X]),
 fail.

mpred_pp_db_justifications(P,Js):-
 show_current_source_location, 
 must_ex(quietly_ex(( format("~NJustifications for ~p:",[P]),
  mpred_pp_db_justification1('',Js,1)))).

mpred_pp_db_justification1(_Prefix,[],_).

mpred_pp_db_justification1(Prefix,[J|Js],N):-
  % show one justification and recurse.
  nl,  
  mpred_pp_db_justifications2(Prefix,J,N,1),
  reset_shown_justs,
  N2 is N+1,
  mpred_pp_db_justification1(Prefix,Js,N2).

mpred_pp_db_justifications2(_Prefix,[],_,_).

mpred_pp_db_justifications2(Prefix,[C|Rest],JustNo,StepNo):-
(nb_hasval('$last_printed',C)-> dmsg_pretty(chasVal(C)) ;
(
 (StepNo==1->fmt('~N~n',[]);true),
  format(string(LP),' ~w.~p.~p',[Prefix,JustNo,StepNo]),
  nb_pushval('$last_printed',LP),
  format("~N  ~w ~p",[LP,C]),
  ignore(loop_check(mpred_why_sub_sub(C))),
  StepNext is 1+StepNo,
  mpred_pp_db_justifications2(Prefix,Rest,JustNo,StepNext))).

mpred_prompt_ask(Info,Ans):-
  format("~N~p",[Info]),
  read(Ans).

mpred_select_justification_node(Js,Index,Step):-
  JustNo is integer(Index),
  nth1(JustNo,Js,Justification),
  StepNo is 1+ integer(Index*10 - JustNo*10),
  nth1(StepNo,Justification,Step).


%%  mpred_supported(+P) is semidet.
%
%  succeeds if P is "supported". What this means
%  depends on the TMS mode selected.
%
mpred_supported(P):-
  must_ex(get_tms_mode(P,Mode))->
  mpred_supported(Mode,P).

%%  mpred_supported(+TMS,+P) is semidet.
%
%  succeeds if P is "supported". What this means
%  depends on the TMS mode supplied.
%
mpred_supported(local,P):- !, mpred_get_support(P,_),!, not_rejected(P).
mpred_supported(full,P):-  !, well_founded(P),!, not_rejected(P).
% mpred_supported(deep,P) :- mpred_deep_support(_How,P).
mpred_supported(How,P):- ignore(How=unknown),not_rejected(P).

%% mpred_tms_supported( +Mode, ?P, ?How) is semidet.
%
% PFC Truth Maintainence/wff Supported.
%
mpred_tms_supported(P,How):-
  must_ex(get_tms_mode(P,Mode))->
  mpred_tms_supported0(Mode,P,How).

mpred_tms_supported0(local,P,How) :-  mpred_get_support(P,How). % ,sanity(mpred_deep_support(How,S)).
mpred_tms_supported0(full,P,How) :-  well_founded(P,How).
mpred_tms_supported0(deep,P,How) :- mpred_deep_support(How,P).


not_rejected(~P):- nonvar(P),  \+ mpred_get_support(P,_).
not_rejected(P):-  \+ mpred_get_support(~P,_).

%% mpred_tms_supported( +P, ?How) is semidet.
%
% PFC Truth Maintainence/wff Supported.
%

mpred_tms_supported(Mode,P,How) :- is_ftVar(Mode),get_tms_mode(P,tms(Mode)),!,mpred_tms_supported0(Mode,P,How).
mpred_tms_supported(Mode,P,How) :- mpred_tms_supported0(Mode,P,How).
mpred_tms_supported(How,_P,unknown(How)).

%% well_founded(+Fact) is semidet.
%
% a fact is well founded if it is supported by the user
%  or by a set of facts and a rules, all of which are well founded.
%
well_founded(Fact):- each_E(well_founded_0,Fact,[_]).

%% well_founded( +Fact, ?How) is semidet.
%
% a fact is well founded if it is supported by the user
% or by a set of facts and a rules, all of which are well founded.
%
well_founded(Fact,How) :- well_founded_0_how(Fact,[],How).

well_founded_0(F,_):-
  % supported by user (axiom) or an "absent" fact (assumption).
  (mpred_axiom(F) ; mpred_assumption(F)),
  !.
well_founded_0(F,Descendants):-
  % first make sure we aren't in a loop.
  (\+ memberchk(F,Descendants)),
  % find a justification.
  supporters_list_prefered(F,Supporters),!,
  % all of whose members are well founded.
  well_founded_list(Supporters,[F|Descendants]),
  !.

%% well_founded_0_how( ?F, ?VALUE2, :TermHow) is semidet.
%
% PFC Well-formed Formula.
%
well_founded_0_how(F,_,How) :-
  % supported by user (mpred_axiom) or an "absent" fact (assumption).
  ((mpred_axiom(F),How =mpred_axiom(F) ); (mpred_assumption(F),How=mpred_assumption(F))),
  !.

well_founded_0_how(F,Descendants,wff(Supporters)) :-
  % first make sure we aren''t in a loop.
  (\+ memberchk(F,Descendants)),
  % find a justification.
  supporters_list_how(F,Supporters),
  % all of whose members are well founded.
  well_founded_list_how(Supporters,[F|Descendants]),
  !.


%%  well_founded_list(+List,-Decendants) is det.
%
% simply maps well_founded over the list.
%
well_founded_list([],_).
well_founded_list([X|Rest],L):-
  well_founded_0(X,L),
  well_founded_list(Rest,L).
%% well_founded_list_how(+L1, ?L2) is semidet.
%
%  simply maps well_founded_0_how over the list.
%
well_founded_list_how([],_).
well_founded_list_how([X|Rest],L) :-
  well_founded_0_how(X,L,_How),
  well_founded_list_how(Rest,L).

%= mpred_tms_supported(+P,-How) succeeds if P is "supported". What "How" means
%= depends on the TMS mode selected.


:- module_transparent((well_founded_list_how)/2).
:- module_transparent((well_founded_0_how)/3).



% baseKB:hook_one_minute_timer_tick:- statistics.


%% mpred_scan_tms( +P) is semidet.
%
% PFC Scan Truth Maintainence/wff.
%
mpred_scan_tms(P):-mpred_get_support(P,(S,SS)),
  (S==SS-> true;
   once((mpred_deep_support(_How,P)->true;
     (mpred_trace_msg(warn(now_maybe_unsupported(mpred_get_support(P,(S,SS)),fail))))))).


%% user_atom( +U) is semidet.
%
% User Atom.
%
user_atom(mfl4(_VarNameZ,_,_,_)):-!.
user_atom(ax).
user_atom(s(_)).


%% mpred_deep_support( +How, ?M) is semidet.
%
% PFC Deep Support.
%
mpred_deep_support(_How,unbound):-!,fail.
mpred_deep_support(How,M):-mpred_deep_support_how(How,M).


%% mpred_deep_support_how( +U, ?U) is semidet.
%
% PFC Deep Support Primary Helper.
%
mpred_deep_support_how(user_atom(U),(U,ax)):-user_atom(U),!.
mpred_deep_support_how(How,(A==>_)):-!,mpred_deep_support(How,A).
mpred_deep_support_how('$pt'(MZ,HowA,HowB),'$pt'(MZ,A,B)):-!,mpred_deep_support(HowA,A),mpred_deep_support(HowB,B).
mpred_deep_support_how(HowA->HowB,(A->B)):-!,mpred_deep_support(HowA,A),mpred_deep_support(HowB,B).
mpred_deep_support_how(HowA/HowB,(A/B)):-!,mpred_deep_support(HowA,A),mpred_deep_support(HowB,B).
mpred_deep_support_how((HowA,HowB),(A,B)):-!,mpred_deep_support(HowA,A),mpred_deep_support(HowB,B).
mpred_deep_support_how(How,rhs(P)):-!,maplist(mpred_deep_support,How,P).
mpred_deep_support_how(mpred_call_only_facts(\+ P),\+ call_u(P)):-!,mpred_call_only_facts(\+ P).
mpred_deep_support_how(mpred_call_only_facts(P),call_u(P)):-!,mpred_call_only_facts(P).
mpred_deep_support_how(mpred_call_only_facts(P),{P}):-!,mpred_call_only_facts(P).
mpred_deep_support_how(S==>How,P):-mpred_get_support(P,S),mpred_deep_support(How,S),!.
mpred_deep_support_how(mpred_call_only_facts(\+(P)),\+(P)):-!, mpred_call_only_facts(\+(P)).
mpred_deep_support_how(user_atom(P),P):-user_atom(P),!.
mpred_deep_support_how(mpred_call_only_facts((P)),P):-mpred_call_only_facts(P).


%% mpred_get_support_precanonical_plus_more( +P, ?Sup) is semidet.
%
% PFC Get Support Precanonical Plus More.
%
mpred_get_support_precanonical_plus_more(P,Sup):- 
  mpred_get_support_one(P,Sup)*->true;
  ((fully_expand(mpred_get_support_precanonical_plus_more,P,PE),!,
    P\=@=PE,mpred_get_support_one(PE,Sup))).

%% mpred_get_support_one( +P, ?Sup) is semidet.
%
% PFC Get Support One.
%
mpred_get_support_one(P,Sup):- mpred_get_support(P,Sup)*->true;
  (mpred_get_support_via_clause_db(P,Sup)*->true;
     mpred_get_support_via_sentence(P,Sup)).


%% mpred_get_support_via_sentence( +Var, ?VALUE2) is semidet.
%
% PFC Get Support Via Sentence.
%
mpred_get_support_via_sentence(Var,_):-is_ftVar(Var),!,fail.
mpred_get_support_via_sentence((A,B),(FC,TC)):-!, mpred_get_support_precanonical_plus_more(A,(FA,TA)),mpred_get_support_precanonical_plus_more(B,(FB,TB)),conjoin(FA,FB,FC),conjoin(TA,TB,TC).
mpred_get_support_via_sentence(true,g):-!.
mpred_get_support_via_sentence(G,call_u(G)):- call_u(G).



%% mpred_get_support_via_clause_db( :TermP, ?OUT) is semidet.
%
% PFC Get Support Via Clause Database.
%
mpred_get_support_via_clause_db(\+ P,OUT):- mpred_get_support_via_clause_db(~(P),OUT).
mpred_get_support_via_clause_db(\+ P,(naf(g),g)):- !, predicate_property(P,number_of_clauses(_)),\+ clause(P,_Body).
mpred_get_support_via_clause_db(P,OUT):- predicate_property(P,number_of_clauses(N)),N>0,
   clause_u(P,Body),(Body==true->Sup=(g);
    (support_ok_via_clause_body(P),mpred_get_support_precanonical_plus_more(Body,Sup))),
   OUT=(Sup,g).



%% support_ok_via_clause_body( +H) is semidet.
%
% Support Ok Via Clause Body.
%
support_ok_via_clause_body(_H):-!,fail.
support_ok_via_clause_body(H):- get_functor(H,F,A),support_ok_via_clause_body(H,F,A).


%% support_ok_via_clause_body( +VALUE1, ?F, ?VALUE3) is semidet.
%
% Support Ok Via Clause Body.
%
support_ok_via_clause_body(_,(\+),1):-!,fail.
support_ok_via_clause_body(_,F,_):- lookup_u(rtArgsVerbatum(F)),!,fail.
support_ok_via_clause_body(H,F,A):- should_call_for_facts(H,F,A).

%% supporters_list_how(+F,-ListofSupporters) is det.
%
% where ListOfSupports is a list of the
% supports for one justification for fact F -- i.e. a list of facts which,
% together allow one to deduce F.  One of the facts will typically be a rule.
% The supports for a user-defined fact are: [ax].
%
supporters_list_how(F,ListO):- no_repeats_cmp(same_sets,ListO,supporters_list_each(F,ListO)).

same_sets(X,Y):-
  flatten([X],FX),sort(FX,XS),
  flatten([Y],FY),sort(FY,YS),!,
  YS=@=XS.

supporters_list_each(F,ListO):-   
   supporters_list_prefered(F,ListM),
   expand_supporters_list(ListM,ListM,ListO).

expand_supporters_list(_, [],[]):-!.
expand_supporters_list(Orig,[F|ListM],[F|NewListOO]):-
   supporters_list_prefered(F,FList),
   list_difference_variant(FList,Orig,NewList),
   % NewList\==[],
   append(Orig,NewList,NewOrig),
   append(ListM,NewList,NewListM),!,
   expand_supporters_list(NewOrig,NewListM,ListO),
   append(ListO,NewList,NewListO),
   list_to_set_variant(NewListO,NewListOO).
expand_supporters_list(Orig,[F|ListM],[F|NewListO]):-
  expand_supporters_list(Orig,ListM,NewListO).


list_to_set_variant(List, Unique) :-
    list_unique_1(List, [], Unique),!.

list_unique_1([], _, []).
list_unique_1([X|Xs], So_far, Us) :-
    memberchk_variant(X, So_far),!,
    list_unique_1(Xs, So_far, Us).
list_unique_1([X|Xs], So_far, [X|Us]) :-
    list_unique_1(Xs, [X|So_far], Us).

% dif_variant(X,Y):- freeze(X,freeze(Y, X \=@= Y )).



%%	list_difference_variant(+List, -Subtract, -Rest)
%
%	Delete all elements of Subtract from List and unify the result
%	with Rest.  Element comparision is done using =@=/2.

list_difference_variant([],_,[]).
list_difference_variant([X|Xs],Ys,L) :-
	(   memberchk_variant(X,Ys)
	->  list_difference_variant(Xs,Ys,L)
	;   L = [X|T],
	    list_difference_variant(Xs,Ys,T)
	).


%%	memberchk_variant(+Val, +List)
%
%	Deterministic check of membership using =@= rather than
%	unification.

memberchk_variant(X, [Y|Ys]) :-
   (   X =@= Y
   ->  true
   ;   memberchk_variant(X, Ys)
   ).

:- module_transparent(supporters_list_prefered/2).
supporters_list_prefered(Var,[is_ftVar(Var)]):-is_ftVar(Var),!.
supporters_list_prefered(F,OUT):-  
 pfc_with_quiet_vars_lock(supporters_list00(F,OUT)).

:- module_transparent(supporters_list00/2).
supporters_list00(F,OUT):- supporters_list1a(F,OUT) *-> true; supporters_list1b(F,OUT).

:- module_transparent(supporters_list1a/2).

supporters_list1a(F,[Fact|MoreFacts]):-
  mpred_get_support_why(F,(Fact,Trigger)),
  triggerSupports(Fact,Trigger,MoreFacts).
   

:- module_transparent(supporters_list1b/2).
supporters_list1b(Var,[is_ftVar(Var)]):- is_ftVar(Var),!.
supporters_list1b(U,[]):- axiomatic_supporter(U),!.
supporters_list1b((H:-B),[MFL]):- !, clause_match(H,B,Ref),find_hb_mfl(H,B,Ref,MFL).
supporters_list1b(\+ P, HOW):- !, supporters_list00(~ P,HOW),!.
supporters_list1b((H),[((H:-B))]):- may_cheat, clause_match(H,B,_Ref).

may_cheat:- fail.

uses_call_only(H):- predicate_property(H,foreign),!.
uses_call_only(H):- predicate_property(H,_), \+ predicate_property(H,interpreted),!.

clause_match(H,_B,uses_call_only(H)):- uses_call_only(H),!.
clause_match(H,B,Ref):- clause_asserted(H,B,Ref),!.
clause_match(H,B,Ref):- ((copy_term(H,HH),clause_u(H,B,Ref),H=@=HH)*->true;clause_u(H,B,Ref)), \+ reserved_body_helper(B).

find_mfl(C,MFL):- lookup_spft_match(C,MFL,ax).
find_mfl(C,MFL):- unwrap_litr0(C,UC) -> C\==UC -> find_mfl(UC,MFL).
find_mfl(C,MFL):- expand_to_hb(C,H,B),
   find_hb_mfl(H,B,_Ref,MFL)->true; (clause_match(H,B,Ref),find_hb_mfl(H,B,Ref,MFL)).

find_hb_mfl(_H,_B,Ref,mfl4(_VarNameZ,M,F,L)):- atomic(Ref),clause_property(Ref,line_count(L)),
 clause_property(Ref,file(F)),clause_property(Ref,module(M)). 
find_hb_mfl(H,B,_,mfl4(VarNameZ,M,F,L)):- lookup_spft_match_first( (H:-B),mfl4(VarNameZ,M,F,L),_),!.
find_hb_mfl(H,B,_Ref,mfl4(VarNameZ,M,F,L)):- lookup_spft_match_first(H,mfl4(VarNameZ,M,F,L),_),ground(B).
find_hb_mfl(H,_B,uses_call_only(H),MFL):- !,call_only_based_mfl(H,MFL).

/*


clause_match(H,_B,uses_call_only(H)):- uses_call_only(H),!.
clause_match(H,B,Ref):- clause_asserted(H,B,Ref),!.

clause_match(H,B,Ref):- no_repeats(Ref,((((copy_term(H,HH),clause_u(H,B,Ref),H=@=HH)*->true;clause_u(H,B,Ref)), \+ reserved_body_helper(B)))).

clause_match0(H,B,Ref):- no_repeats(Ref,clause_match1(H,B,Ref)).

clause_match1(H,B,Ref):- clause(H,B,Ref).
clause_match1(M:H,B,Ref):- !, (M:clause(H,B,Ref) ; clause_match2(H,B,Ref)).
clause_match1(H,B,Ref):- clause_match2(H,B,Ref).

clause_match2(H,B,Ref):- current_module(M),clause(M:H,B,Ref),(clause_property(Ref, module(MM))->MM==M;true).

find_mfl(C,MFL):-find_mfl0(C,MFL),compound(MFL),MFL=mfl4(VarNameZ,_,F,_),nonvar(F).
find_mfl0(C,MFL):- lookup_spft_match(C,MFL,ax).
% find_mfl0(mfl4(VarNameZ,M,F,L),mfl4(VarNameZ,M,F,L)):-!.
find_mfl0(C,MFL):-expand_to_hb(C,H,B),
   find_hb_mfl(H,B,_Ref,MFL)->true; (clause_match(H,B,Ref),find_hb_mfl(H,B,Ref,MFL)).
find_mfl0(C,MFL):-expand_to_hb(C,H,B),
   find_hb_mfl(H,B,_Ref,MFL)->true; (clause_match0(H,B,Ref),find_hb_mfl(H,B,Ref,MFL)).

*/
call_only_based_mfl(H,mfl4(_VarNameZ,M,F,L)):- 
  ignore(predicate_property(H,imported_from(M));predicate_property(H,module(M))),
  ignore(predicate_property(H,line_count(L))),
  ignore(source_file(M:H,F);predicate_property(H,file(F));(predicate_property(H,foreign),F=foreign)).

axiomatic_supporter(Var):-is_ftVar(Var),!,fail.
axiomatic_supporter(is_ftVar(_)).
axiomatic_supporter(clause_u(_)).
axiomatic_supporter(U):- is_file_ref(U),!.
axiomatic_supporter(ax):-!.

is_file_ref(A):-compound(A),A=mfl4(_VarNameZ,_,_,_).

triggerSupports(_,Var,[is_ftVar(Var)]):-is_ftVar(Var),!.
triggerSupports(_,U,[]):- axiomatic_supporter(U),!.
triggerSupports(FactIn,Trigger,OUT):-
  mpred_get_support(Trigger,(Fact,AnotherTrigger))*->
  (triggerSupports(Fact,AnotherTrigger,MoreFacts),OUT=[Fact|MoreFacts]);
  triggerSupports1(FactIn,Trigger,OUT).

triggerSupports1(_,X,[X]):- may_cheat.
/*
triggerSupports1(_,X,_):- mpred_db_type(X,trigger(_)),!,fail.
triggerSupports1(_,uWas(_),[]):-!.
triggerSupports1(_,U,[(U)]):- is_file_ref(U),!.
triggerSupports1(_,U,[uWas(U)]):- get_source_uu((U1,U2))->member(U12,[U1,U2]),U12=@=U.
triggerSupports1(_,X,[X]):- \+ mpred_db_type(X,trigger(_)).
*/


/*
:-module_transparent(mpred_ain/1).
:-module_transparent(mpred_aina/1).
:-module_transparent(mpred_ainz/1).
*/

% :- '$current_source_module'(M),forall(mpred_database_term(F,A,_),(abolish(pfc_lib:F/A),abolish(user:F/A),abolish(M:F/A))).
% :- initialization(ensure_abox_hybrid(baseKB)).


% % :- set_prolog_flag(mpred_pfc_file,true).
% local_testing

%:- fixup_exports.

:- set_prolog_flag(expect_pfc_file,unknown).

