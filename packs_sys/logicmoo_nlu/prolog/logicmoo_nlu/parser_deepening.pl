% ===================================================================
% File 'parser_candc.pl'
% Purpose: Attempto Controlled English to CycL conversions from SWI-Prolog  
% This implementation is an incomplete proxy for CycNL and likely will not work as well
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'parser_candc.pl' 1.0.0
% Revision:  $Revision: 1.3 $
% Revised At:   $Date: 2002/06/06 15:43:15 $
% ===================================================================
% end_of_file.
:-module(parser_deepening,[
            deepen_pos/1,
            call_until_failed/1,
            debug_chat80_if_fail/1,
            if_search_expanded/2,
            if_search_expanded_ge/2,
            if_search_expanded/1]).


:- dynamic(pdtmp:search_expands/3).
:- thread_local(pdtmp:expand_enabled/3).

if_search_expanded(N):- if_search_expanded(N,unknown).
if_search_expanded(N,_Name):- flag(pos_depth,W,W), W>N,!.
if_search_expanded(N,Name):- flag(pos_depth_skipped,W,W+1),
  nop(dmsg(skipped(if_search_expanded(N,Name)))), fail.

:-meta_predicate(call_until_failed(0)).

call_until_failed([H,(!)|T]):- !,call_until_failed([(H,!)|T]).
call_until_failed([H|T]):- !,
  call(H)*->(call_until_failed(T),!);fmt(failed(H)).
call_until_failed([]).

:-meta_predicate(deepen_local_0(+,0)).
deepen_local_0(Local, Call):-
  ( \+ retract(Local) -> setup_call_cleanup(true, one_must(Call,locally(Local,Call)), ignore(retract(Local)))  ; 
     (setup_call_cleanup(true, 
       one_must(Call,locally(Local,Call)), 
        asserta(Local)))).

%t_l:old_text.

:- thread_local(t_l:useAltPOS/0).
%t_l:useAltPOS:- fail.

:- share_mp(deepen_pos/1).
:- export(deepen_pos/1).
:-meta_predicate(deepen_pos(0)).
% temp hack
deepen_pos_old(Call):- (Call *-> true ; (deepen_pos_0(Call) *->  true ; locally(t_l:useAltPOS,deepen_pos_0(Call)))).

% Dont use this recursively
deepen_pos(Call):- flag(pos_depth,N,N),N>0, dmsg(recursive(deepen_pos(Call))),!,call(Call).
% starting fresh from here
deepen_pos(Call):- deepen_pos_fresh(Call).

deepen_pos_fresh(Call):- 
     flag(pos_depth_skipped,Was1,0),flag(pos_depth,Was2,0), 
   call_cleanup((Call *-> true ; deepen_pos_pt2(10,Call)),
     (flag(pos_depth_skipped,_,Was1),flag(pos_depth,_,Was2))).

% this must be used *atfer* Call has been tried
deepen_pos_pt2(Upto,Call):- flag(pos_depth,N,N+1),
  flag(pos_depth_skipped,EN,EN), EN\==0,
  ((N>Upto) -> (!,fail) ; (call(Call)*->true; deepen_pos_pt2(Upto,Call))).

:- share_mp(deepen_pos_0/1).
:-meta_predicate(deepen_pos_0(0)).
:- thread_local(t_l:usePlTalk/0).
deepen_pos_0(Call):- deepen_local_0(t_l:usePlTalk,Call).


:- create_prolog_flag(debug_chat80,false,[keep(true)]).
:-meta_predicate(debug_chat80_if_fail(0)).
debug_chat80_if_fail(MG):- set_prolog_flag(debug_chat80,false), strip_module(MG,M,G), locally(set_prolog_flag(no_pretty,true),debug_chat80_if_fail(M,G)).

%debug_chat80_if_fail(M,G):- current_prolog_flag(debug_chat80,true),!, debug_chat80_if_fail(Fail,IsCut,6,M,G),(IsCut==!->!;true),(Fail\==fail).
debug_chat80_if_fail(M,(G1,G2)):- !, debug_chat80_if_fail(M,G1),debug_chat80_if_fail(M,G2).
debug_chat80_if_fail(M,G):-   
  \+ current_prolog_flag(debug_chat80,true),!,
  (call(M:G)*->true;
    (locally(set_prolog_flag(debug_chat80,true), must80(M:G)))).
debug_chat80_if_fail(M,G):-must80(M:G).


debug_chat80_if_fail(Fail2,_,_,_,_):- Fail2==fail,!.
debug_chat80_if_fail(_,!,_,_,!):-!.
debug_chat80_if_fail(Fail,IsCut,N,M,(G1,G2)):- !, debug_chat80_if_fail(Fail,IsCut,N,M,G1), debug_chat80_if_fail(Fail,IsCut,N,M,G2).
debug_chat80_if_fail(Fail2,IsCut,N,M,G):- 
 \+ current_prolog_flag(debug_chat80,true),!, 
  (call(M:G)*->true;
    locally(set_prolog_flag(debug_chat80,true), 
      (debug_chat80_if_fail(Fail,IsCut,N,M,G)*->Fail\==fail;Fail2=fail))).
debug_chat80_if_fail(Fail,_,N,M,G):- N>0, predicate_property(M:G,number_of_clause(_)), O is N -1, !, 
  (clause(M:G,Body),debug_chat80_if_fail(Fail,IsCut,O,M,Body),(IsCut==!->!;true),Fail\==fail).
debug_chat80_if_fail(Fail,_IsCut,_,M,G):-  must80(M:G)*->true;Fail=fail.

/*

deepen_pos_0(Call):-
  ( \+ retract(t_l:usePlTalk) -> setup_call_cleanup(true, one_must(Call,locally(t_l:usePlTalk,Call)), ignore(retract(t_l:usePlTalk)))  ; 
     (setup_call_cleanup(true, 
       one_must(Call,locally(t_l:usePlTalk,Call)), 
        asserta(t_l:usePlTalk)))).
*/
source_loc_key(O):- prolog_load_context(term,Term),term_loc_atom(Term,T),!,gensym(T,O).
source_loc_key(O):- source_location(S,_),gensym(S,O).
source_loc_key(O):- gensym(source_loc_key,O).

term_loc_atom(T,T):- atom(T).
term_loc_atom(T,A):- \+ compound(T), term_to_atom(T,A).
term_loc_atom(_:H,T):- !, term_loc_atom(H,T).
term_loc_atom(H:-_,T):- !, term_loc_atom(H,T).
term_loc_atom(P,F):- functor(P,F,_).

if_search_expanded_ge(if_search_expanded(N),if_search_expanded(N,T)):- 
  source_loc_key(T),!,source_location(S,L),
  assert_if_new(pdtmp:search_expands(T,S,L)).

system:goal_expansion(G,O,GE,O):- compound(G),if_search_expanded_ge(G,GE).


