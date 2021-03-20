% init_why(after('/opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/episodic_memory/planner/ec_reader.pl')).
% init_why(after('/opt/logicmoo_workspace/packs_sys/small_adventure_games/prolog/episodic_memory/planner/ec_reader.pl')).
% init_why(program).
% ec_to_pl(do_ec_load, current_output, 'ectest/TestBoxRoom.e').
% ectest/TestBoxRoom.e:1
% ec_in_to_pl(do_ec_load, current_output, <stream>(0x55638dfa3da0)).
% ec_io(do_ec_load, <stream>(0x55638dfa3da0)).
% ectest/TestBoxRoom.e:1
:- module(ec).

:- discontiguous do_test/1.

do_test_gen(What) :- ec_current_domain_db(fluent(P)),functor(P,F,A),functor(What,F,A).

local_demo(L):- local_demo(L,R),dbginfo('R'=R).

local_demo(L,R):-  dbginfo('L'=L),abdemo_special(depth(0,10),L,R),!.
local_demo(L,R):-  dm('FAILED:',(L:-R)),trace,!,abdemo_special(depth(0,10),L,R).


dm(TF,P):- format('~N~n~w ~p.~n',[TF,P]).

/*

These tests Pass


*/
do_test(test_np_box_1) :-  local_demo([holds_at(directlyIn(lisa,livingRoom),t)]).
do_test(test_np_box_2) :-  local_demo([holds_at(inRoom(lisa,livingRoom),t)]).
do_test(test_np_box_3) :-  local_demo([holds_at(directlyIn(lisa,kitchen),t)]).
do_test(test_np_box_4) :-  local_demo([holds_at(inRoom(lisa,kitchen),t)]).
do_test(test_np_box_5) :-  local_demo([holds_at(directlyIn(box,kitchen),t)]).

% fix this next test and the "test_np_box_occurs" should pass
%do_test(has_occured) :-  local_demo([has_occured(move(lisa,box,livingRoom,lisa))],R).




% 
do_test(happened) :-  local_demo([happens(move(lisa,box,livingRoom,lisa),_T)]).

do_test(happened2) :-  local_demo([happens(move(lisa,box,livingRoom,lisa),_T1,_T2)]).

% 
do_test(happend2b) :-  fail, local_demo(
              [happens(move(lisa,newspaper,livingRoom,box),t_plus_01),
                before(t_plus_01, t_plus_41),
               happens(move(lisa,lisa,kitchen,livingRoom),t_plus_41)]).

do_test(happend2a) :- fail,  local_demo(
              [happens(move(lisa,newspaper,livingRoom,box),t_plus_01,t_plus_02),
                before(t_plus_01, t_plus_41),
               happens(move(lisa,lisa,kitchen,livingRoom),t_plus_41,t_plus_42)]).

do_test(happend2r) :- fail, local_demo(
              [happens(move(lisa,newspaper,livingRoom,box),t_plus_01,t_plus_02),
                before(t_plus_41, t_plus_01),
               happens(move(lisa,lisa,kitchen,livingRoom),t_plus_41,t_plus_42)]).



do_test(test_np_box_occurs) :- test_np_box_occurs.

test_np_box_occurs1(HapsList):-
 bagof(E, H^((axiom(H,B),functor(H,happens,_)),member(E,[H|B])),  UHapsList),
 predsort(compare_on_time_arg,UHapsList,HapsList),
 dbginfo('HapsList'=HapsList),!. 

test_np_box_occurs:- 
 test_np_box_occurs1(HapsList),
 /* 
   HapsList = 
         [happens(move(lisa,newspaper,livingRoom,box),0),
          happens(move(lisa,box,livingRoom,lisa),1),
          happens(move(lisa,lisa,livingRoom,kitchen),2),
          happens(move(lisa,box,lisa,kitchen),3),
          happens(move(lisa,lisa,kitchen,livingRoom),4)].
*/

 falling_edges(v2,t_plus_, t_minus_1, HapsList, [_|Edges], _Out),
 dbginfo('Edges'=Edges), !,
 /*
   Edges = [holds_at(has_occured(move(lisa,newspaper,livingRoom,box)),t_plus_01),
            before(t_plus_01,t_plus_11),
            holds_at(has_occured(move(lisa,box,livingRoom,lisa)),t_plus_11),
            before(t_plus_11,t_plus_21),
            holds_at(has_occured(move(lisa,lisa,livingRoom,kitchen)),t_plus_21),
            before(t_plus_21,t_plus_31),
            holds_at(has_occured(move(lisa,box,lisa,kitchen)),
            t_plus_31),before(t_plus_31,t_plus_41),
            holds_at(has_occured(move(lisa,lisa,kitchen,livingRoom)),t_plus_41)].

   Edges_V2 = [happens(move(lisa,newspaper,livingRoom,box),t_plus_01,t_plus_02),
               before(t_plus_02,t_plus_11),
               happens(move(lisa,box,livingRoom,lisa),t_plus_11,t_plus_12),
               before(t_plus_12,t_plus_21),
               happens(move(lisa,lisa,livingRoom,kitchen),t_plus_21,t_plus_22),
               before(t_plus_22,t_plus_31),
               happens(move(lisa,box,lisa,kitchen),t_plus_31,t_plus_32),
               before(t_plus_32,t_plus_41),
               happens(move(lisa,lisa,kitchen,livingRoom),t_plus_41,t_plus_42)].
   .


   Edges_T3 = [happens(move(lisa,newspaper,livingRoom,box),t_plus_01,t_plus_02),
               before(t_plus_01,t_plus_41),
               happens(move(lisa,lisa,kitchen,livingRoom),t_plus_41,t_plus_42)].
.


 */
 local_demo(Edges),!.

do_test(test_np_box_agent) :-  forall(do_test_gen(What), local_demo([holds_at(What,_When)])).



:-include(('../ec_test_incl')).

/*

Just to see the syntax (not related to this work)

axiom(happens(shift_pack(Agnt,P,R1,R2,R3),T1,T6),
     [happens(go_to_room(Agnt,R1,R2),T1,T2),
     before(T2,T3), not(clipped(T2,atRoom(Agnt,R2),T3)), not(clipped(T1,inRoom(P,R2),T3)),
     happens(pick_up(Agnt,P),T3), before(T3,T4), happens(go_to_room(Agnt,R2,R3),T4,T5),
     before(T5,T6), not(clipped(T3,got(Agnt,P),T6)), not(clipped(T5,atRoom(Agnt,R3),T6)),
     happens(put_down(Agnt,P),T6)]).

axiom(initiates(shift_pack(Agnt,P,R1,R2,R3),inRoom(P,R3),T),
     [holds_at(atRoom(Agnt,R1),T), holds_at(inRoom(P,R2),T)]).
*/

axiom(happens(rise_and_fall(Event),T1,T3),
     [happens(begining(Event),T1), before(T1,T2), 
      happens(ocuring(Event),T2,T3),  before(T2,T3),
      % because its ocuring the begining not clipped?
      % not(clipped(T1,begun(Event),T2)),      
      happens(ending(Event),T3)]).

axiom(terminates(begining(Event),holds_at( never_ocurred(Event),t),t), []).
axiom( initiates(begining(Event),holds_at(    just_begun(Event),t),t), []).
axiom(terminates( ocuring(Event),holds_at(    just_begun(Event),t),t), []).
axiom( initiates( ocuring(Event),holds_at(    now_occurs(Event),t),t), []).
axiom(terminates(  ending(Event),holds_at(    now_occurs(Event),t),t), []).
axiom( initiates(  ending(Event),holds_at(   has_occured(Event),t),t), []).

axiom(initially( (never_ocurred(Event))),[]):- executable(Event).
axiom(initially( neg(just_begun(Event))),[]):- executable(Event).
axiom(initially( neg(now_occurs(Event))),[]):- executable(Event).
axiom(initially(neg(has_occured(Event))),[]):- executable(Event).



:- cvt_e_pl('TestBoxRoom.e').

:- load_e('TestBoxRoom.e').

:- listing(ec_current_domain_db).


:- run_tests.

%:- halt.






