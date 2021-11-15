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
            if_search_expanded/1]).


if_search_expanded(N):- flag(pos_depth,W,W), W>N,!.
if_search_expanded(_):- flag(pos_depth_skipped,W,W+1),fail.

:-meta_predicate(call_until_failed(0)).
:-meta_predicate(debug_chat80_if_fail(0)).


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

:- create_prolog_flag(debug_chat80,true,[keep(true)]).

debug_chat80_if_fail(Call):- current_prolog_flag(debug_chat80,true),!,must80(Call).
debug_chat80_if_fail(Call):- call(Call)*->true;locally(set_prolog_flag(debug_chat80,true),must80(Call)).

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

/*

deepen_pos_0(Call):-
  ( \+ retract(t_l:usePlTalk) -> setup_call_cleanup(true, one_must(Call,locally(t_l:usePlTalk,Call)), ignore(retract(t_l:usePlTalk)))  ; 
     (setup_call_cleanup(true, 
       one_must(Call,locally(t_l:usePlTalk,Call)), 
        asserta(t_l:usePlTalk)))).
*/


