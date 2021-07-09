:-include(library('ec_planner/ec_test_incl')).
:-expects_dialect(pfc).
% Wed, 17 Mar 2021 07:06:47 GMT
:-was_s_l('/root/.local/share/swi-prolog/pack/pfc/prolog/pfc.pl',68).

 /*  loading(load_e_pl,
     	'/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e').
 */
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%; FILE: examples/Mueller2006/Chapter10/MovingNewspaperAndBox.e
%; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%;
%; Copyright (c) 2005 IBM Corporation and others.
%; All rights reserved. This program and the accompanying materials
%; are made available under the terms of the Common Public License v1.0
%; which accompanies this distribution, and is available in
%; http://www.eclipse.org/legal/cpl-v10.html
%;
%; Contributors:
%; IBM - Initial implementation
%;
%; @book{Mueller:2006,
%;   author = "Erik T. Mueller",
%;   year = "2006",
%;   title = "Commonsense Reasoning",
%;   address = "San Francisco",
%;   publisher = "Morgan Kaufmann/Elsevier",
%; }
%;

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',24).
% include foundations/Root.e
:- if(is_e_toplevel).
:- call_pel_directive(include('foundations/Root.e')).
 %  call_pel_directive(include('foundations/Root.e')).
:- endif.

% include foundations/EC.e
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',24).
:- if(is_e_toplevel).
:- call_pel_directive(include('foundations/EC.e')).
 %  call_pel_directive(include('foundations/EC.e')).
:- endif.

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',27).
% sort object
==>sort(object).

% sort agent: object
==>subsort(agent,object).

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',29).
% sort physobj: object
==>subsort(physobj,object).

% sort room: object
==>subsort(room,object).

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',32).
% fluent directlyIn(object,object)
blue=fluent(directlyIn(_,DirectlyIn)).
blue=fluent(directlyIn(object,object)).
==>mpred_prop(directlyIn(object,object),fluent).
==>meta_argtypes(directlyIn(object,object)).

% fluent inRoom(object,room)
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',32).
blue=fluent(inRoom(_,InRoom)).
blue=fluent(inRoom(object,room)).
==>mpred_prop(inRoom(object,room),fluent).
==>meta_argtypes(inRoom(object,room)).

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',34).
% noninertial inRoom
:- if(is_e_toplevel).
:- call_pel_directive(noninertial(inRoom)).
 %  call_pel_directive(noninertial(inRoom)).
:- endif.
%;; executable(move(agent,object,object,object))

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',38).
% agent Lisa
==>t(agent,lisa).

% physobj Box, Newspaper
==>t(physobj,box).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',38).
==>t(physobj,newspaper).

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',40).
% room Kitchen, LivingRoom
==>t(room,kitchen).
==>t(room,livingRoom).
%; Sigma
%; RS10
% [agent,physobj1,physobj2,room,time]
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',46).
% HoldsAt(directlyIn(agent,room),time) &
% HoldsAt(directlyIn(physobj1,room),time) &
% HoldsAt(inRoom(physobj2,room),time) ->
% Initiates(move(agent,physobj1,room,physobj2),directlyIn(physobj1,physobj2),time).

 /*  [holds_at(directlyIn(Agent, InRoom_DirectlyIn_Room), At_Time), holds_at(directlyIn(Physobj1, InRoom_DirectlyIn_Room), At_Time), holds_at(inRoom(DirectlyIn_Move_Physobj2, InRoom_DirectlyIn_Room), At_Time)] ->
         ta(At_Time,
            tvs1=[At_Time],
            tvs2=[At_Time],
            initiates_at(move(Agent,
                              Physobj1,
                              InRoom_DirectlyIn_Room,
                              DirectlyIn_Move_Physobj2),
                         directlyIn(Physobj1, DirectlyIn_Move_Physobj2),
                         At_Time)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',46).
pl=axiom(initiates_at(move(Agent,
			Physobj1,
			InRoom_DirectlyIn_Room,
			DirectlyIn_Move_Physobj2),
		   directlyIn(Physobj1,
			      DirectlyIn_Move_Physobj2),
		   Time_at_At_Time),
      [ holds_at(directlyIn(Agent,InRoom_DirectlyIn_Room),
		 Time_at_At_Time),
	holds_at(directlyIn(Physobj1,
			    InRoom_DirectlyIn_Room),
		 Time_at_At_Time),
	holds_at(inRoom(DirectlyIn_Move_Physobj2,
			InRoom_DirectlyIn_Room),
		 Time_at_At_Time)
      ]).


%; RS11
% [agent,physobj1,physobj2,room,time]
% HoldsAt(directlyIn(agent,room),time) &
% HoldsAt(directlyIn(physobj1,room),time) &
% HoldsAt(inRoom(physobj2,room),time) ->
% Terminates(move(agent,physobj1,room,physobj2),directlyIn(physobj1,room),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',52).

 /*  [holds_at(directlyIn(Agent, InRoom_DirectlyIn_Room), At_Time), holds_at(directlyIn(Physobj1, InRoom_DirectlyIn_Room), At_Time), holds_at(inRoom(Move_Physobj2, InRoom_DirectlyIn_Room), At_Time)] ->
         ta(At_Time,
            tvs1=[At_Time],
            tvs2=[At_Time],
            terminates_at(move(Agent,
                               Physobj1,
                               InRoom_DirectlyIn_Room,
                               Move_Physobj2),
                          directlyIn(Physobj1, InRoom_DirectlyIn_Room),
                          At_Time)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',52).
pl=axiom(terminates_at(move(Agent,
			 Physobj1,
			 InRoom_DirectlyIn_Room,
			 Move_Physobj2),
		    directlyIn(Physobj1,
			       InRoom_DirectlyIn_Room),
		    Time_at_At_Time),
      [ holds_at(directlyIn(Agent,InRoom_DirectlyIn_Room),
		 Time_at_At_Time),
	holds_at(directlyIn(Physobj1,
			    InRoom_DirectlyIn_Room),
		 Time_at_At_Time),
	holds_at(inRoom(Move_Physobj2,
			InRoom_DirectlyIn_Room),
		 Time_at_At_Time)
      ]).


%; RS12
% [agent,physobj1,physobj2,room,time]
% HoldsAt(directlyIn(agent,room),time) ->
% Initiates(move(agent,physobj1,physobj2,room),directlyIn(physobj1,room),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',59).

 /*  [holds_at(directlyIn(Agent, Move_DirectlyIn_Room), At_Time)] ->
         ta(At_Time,
            tvs1=[At_Time],
            tvs2=[At_Time],
            initiates_at(move(Agent,
                              Physobj1,
                              Physobj2,
                              Move_DirectlyIn_Room),
                         directlyIn(Physobj1, Move_DirectlyIn_Room),
                         At_Time)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',59).
pl=axiom(initiates_at(move(Agent,
			Physobj1,
			Physobj2,
			Move_DirectlyIn_Room),
		   directlyIn(Physobj1,
			      Move_DirectlyIn_Room),
		   Time_at_At_Time),
      [ holds_at(directlyIn(Agent,Move_DirectlyIn_Room),
		 Time_at_At_Time)
      ]).


%; RS13
% [agent,physobj1,physobj2,room,time]
% HoldsAt(directlyIn(agent,room),time) ->
% Terminates(move(agent,physobj1,physobj2,room),directlyIn(physobj1,physobj2),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',64).

 /*  [holds_at(directlyIn(Agent, Move_DirectlyIn_Room), At_Time)] ->
         ta(At_Time,
            tvs1=[At_Time],
            tvs2=[At_Time],
            terminates_at(move(Agent,
                               Physobj1,
                               DirectlyIn_Physobj2,
                               Move_DirectlyIn_Room),
                          directlyIn(Physobj1, DirectlyIn_Physobj2),
                          At_Time)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',64).
pl=axiom(terminates_at(move(Agent,
			 Physobj1,
			 DirectlyIn_Physobj2,
			 Move_DirectlyIn_Room),
		    directlyIn(Physobj1,
			       DirectlyIn_Physobj2),
		    Time_at_At_Time),
      [ holds_at(directlyIn(Agent,Move_DirectlyIn_Room),
		 Time_at_At_Time)
      ]).


%; RS14
% [agent,room1,room2,time]
% HoldsAt(directlyIn(agent,room1),time) ->
% Initiates(move(agent,agent,room1,room2),directlyIn(agent,room2),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',69).

 /*  [holds_at(directlyIn(Agent, DirectlyIn_Room1), At_Time)] ->
         ta(At_Time,
            tvs1=[At_Time],
            tvs2=[At_Time],
            initiates_at(move(Agent,
                              Agent,
                              DirectlyIn_Room1,
                              DirectlyIn_Move_Room2),
                         directlyIn(Agent, DirectlyIn_Move_Room2),
                         At_Time)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',69).
pl=axiom(initiates_at(move(Agent,
			Agent,
			DirectlyIn_Room1,
			DirectlyIn_Move_Room2),
		   directlyIn(Agent,DirectlyIn_Move_Room2),
		   Time_at_At_Time),
      [ holds_at(directlyIn(Agent,DirectlyIn_Room1),
		 Time_at_At_Time)
      ]).


%; RS15
% [agent,room1,room2,time]
% HoldsAt(directlyIn(agent,room1),time) ->
% Terminates(move(agent,agent,room1,room2),directlyIn(agent,room1),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',74).

 /*  [holds_at(directlyIn(Agent, DirectlyIn_Room1), At_Time)] ->
         ta(At_Time,
            tvs1=[At_Time],
            tvs2=[At_Time],
            terminates_at(move(Agent,
                               Agent,
                               DirectlyIn_Room1,
                               Move_Room2),
                          directlyIn(Agent, DirectlyIn_Room1),
                          At_Time)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',74).
pl=axiom(terminates_at(move(Agent,
			 Agent,
			 DirectlyIn_Room1,
			 Move_Room2),
		    directlyIn(Agent,DirectlyIn_Room1),
		    Time_at_At_Time),
      [ holds_at(directlyIn(Agent,DirectlyIn_Room1),
		 Time_at_At_Time)
      ]).


%; RS16
% [agent,physobj,room,time]
% HoldsAt(directlyIn(agent,room),time) &
% HoldsAt(directlyIn(physobj,room),time) ->
% Initiates(move(agent,physobj,room,agent),directlyIn(physobj,agent),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',79).

 /*  [holds_at(directlyIn(DirectlyIn_Move_Agent, DirectlyIn_Room), At_Time), holds_at(directlyIn(Physobj, DirectlyIn_Room), At_Time)] ->
         ta(At_Time,
            tvs1=[At_Time],
            tvs2=[At_Time],
            initiates_at(move(DirectlyIn_Move_Agent,
                              Physobj,
                              DirectlyIn_Room,
                              DirectlyIn_Move_Agent),
                         directlyIn(Physobj, DirectlyIn_Move_Agent),
                         At_Time)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',79).
pl=axiom(initiates_at(move(DirectlyIn_Move_Agent,
			Physobj,
			DirectlyIn_Room,
			DirectlyIn_Move_Agent),
		   directlyIn(Physobj,
			      DirectlyIn_Move_Agent),
		   Time_at_At_Time),
      [ holds_at(directlyIn(DirectlyIn_Move_Agent,
			    DirectlyIn_Room),
		 Time_at_At_Time),
	holds_at(directlyIn(Physobj,DirectlyIn_Room),
		 Time_at_At_Time)
      ]).


%; RS17
% [agent,physobj,room,time]
% HoldsAt(directlyIn(agent,room),time) &
% HoldsAt(directlyIn(physobj,room),time) ->
% Terminates(move(agent,physobj,room,agent),directlyIn(physobj,room),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',85).

 /*  [holds_at(directlyIn(Move_Agent, DirectlyIn_Room), At_Time), holds_at(directlyIn(Physobj, DirectlyIn_Room), At_Time)] ->
         ta(At_Time,
            tvs1=[At_Time],
            tvs2=[At_Time],
            terminates_at(move(Move_Agent,
                               Physobj,
                               DirectlyIn_Room,
                               Move_Agent),
                          directlyIn(Physobj, DirectlyIn_Room),
                          At_Time)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',85).
pl=axiom(terminates_at(move(Move_Agent,
			 Physobj,
			 DirectlyIn_Room,
			 Move_Agent),
		    directlyIn(Physobj,DirectlyIn_Room),
		    Time_at_At_Time),
      [ holds_at(directlyIn(Move_Agent,DirectlyIn_Room),
		 Time_at_At_Time),
	holds_at(directlyIn(Physobj,DirectlyIn_Room),
		 Time_at_At_Time)
      ]).


%; RS18
% [agent,physobj,room,time]
% HoldsAt(directlyIn(physobj,agent),time) &
% HoldsAt(directlyIn(agent,room),time) ->
% Initiates(move(agent,physobj,agent,room),directlyIn(physobj,room),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',91).

 /*  [holds_at(directlyIn(Physobj, DirectlyIn_Agent), At_Time), holds_at(directlyIn(DirectlyIn_Agent, Move_DirectlyIn_Room), At_Time)] ->
         ta(At_Time,
            tvs1=[At_Time],
            tvs2=[At_Time],
            initiates_at(move(DirectlyIn_Agent,
                              Physobj,
                              DirectlyIn_Agent,
                              Move_DirectlyIn_Room),
                         directlyIn(Physobj, Move_DirectlyIn_Room),
                         At_Time)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',91).
pl=axiom(initiates_at(move(DirectlyIn_Agent,
			Physobj,
			DirectlyIn_Agent,
			Move_DirectlyIn_Room),
		   directlyIn(Physobj,Move_DirectlyIn_Room),
		   Time_at_At_Time),
      [ holds_at(directlyIn(Physobj,DirectlyIn_Agent),
		 Time_at_At_Time),
	holds_at(directlyIn(DirectlyIn_Agent,
			    Move_DirectlyIn_Room),
		 Time_at_At_Time)
      ]).


%; RS19
% [agent,physobj,room,time]
% HoldsAt(directlyIn(physobj,agent),time) &
% HoldsAt(directlyIn(agent,room),time) ->
% Terminates(move(agent,physobj,agent,room),directlyIn(physobj,agent),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',97).

 /*  [holds_at(directlyIn(Physobj, DirectlyIn_Agent), At_Time), holds_at(directlyIn(DirectlyIn_Agent, Move_DirectlyIn_Room), At_Time)] ->
         ta(At_Time,
            tvs1=[At_Time],
            tvs2=[At_Time],
            terminates_at(move(DirectlyIn_Agent,
                               Physobj,
                               DirectlyIn_Agent,
                               Move_DirectlyIn_Room),
                          directlyIn(Physobj, DirectlyIn_Agent),
                          At_Time)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',97).
pl=axiom(terminates_at(move(DirectlyIn_Agent,
			 Physobj,
			 DirectlyIn_Agent,
			 Move_DirectlyIn_Room),
		    directlyIn(Physobj,DirectlyIn_Agent),
		    Time_at_At_Time),
      [ holds_at(directlyIn(Physobj,DirectlyIn_Agent),
		 Time_at_At_Time),
	holds_at(directlyIn(DirectlyIn_Agent,
			    Move_DirectlyIn_Room),
		 Time_at_At_Time)
      ]).


%; Delta


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',104).
% Happens(move(Lisa,Newspaper,LivingRoom,Box),0).

 /*  [] ->
         ta(_,
            tvs1=[start],
            tvs2=[start],
            happens_at(move(lisa, newspaper, livingRoom, box), start)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',104).
pl=axiom(happens_at(move(lisa,newspaper,livingRoom,box),start),[]).


% Happens(move(Lisa,Box,LivingRoom,Lisa),1).

 /*  [b(start, At_B_Start), ignore(start+1==At_B_Start)] ->
         ta(_,
            tvs1=[start+1],
            tvs2=[At_B_Start, start],
            happens_at(move(lisa, box, livingRoom, lisa), At_B_Start)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',104).
pl=axiom(happens_at(move(lisa,box,livingRoom,lisa),At_B_Start),
      [b(start,At_B_Start)]).


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',106).
% Happens(move(Lisa,Lisa,LivingRoom,Kitchen),2).

 /*  [b(start, At_Toffset_B_Start2), toffset(start, 2, At_Toffset_B_Start2), ignore(start+2==At_Toffset_B_Start2)] ->
         ta(_,
            tvs1=[start+2],
            tvs2=[At_Toffset_B_Start2, start],
            happens_at(move(lisa, lisa, livingRoom, kitchen),
                       At_Toffset_B_Start2)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',106).
pl=axiom(happens_at(move(lisa,lisa,livingRoom,kitchen),
		 At_Toffset_B_Start2),
      [ b(start,At_Toffset_B_Start2),
	toffset(start,2,At_Toffset_B_Start2)
      ]).


% Happens(move(Lisa,Box,Lisa,Kitchen),3).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',106).

 /*  [b(start, At_Toffset_B_Start3), toffset(start, 3, At_Toffset_B_Start3), ignore(start+3==At_Toffset_B_Start3)] ->
         ta(_,
            tvs1=[start+3],
            tvs2=[At_Toffset_B_Start3, start],
            happens_at(move(lisa, box, lisa, kitchen), At_Toffset_B_Start3)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',106).
pl=axiom(happens_at(move(lisa,box,lisa,kitchen),At_Toffset_B_Start3),
      [ b(start,At_Toffset_B_Start3),
	toffset(start,3,At_Toffset_B_Start3)
      ]).


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',108).
% Happens(move(Lisa,Lisa,Kitchen,LivingRoom),4).

 /*  [b(start, At_Toffset_B_Start4), toffset(start, 4, At_Toffset_B_Start4), ignore(start+4==At_Toffset_B_Start4)] ->
         ta(_,
            tvs1=[start+4],
            tvs2=[At_Toffset_B_Start4, start],
            happens_at(move(lisa, lisa, kitchen, livingRoom),
                       At_Toffset_B_Start4)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',108).
pl=axiom(happens_at(move(lisa,lisa,kitchen,livingRoom),
		 At_Toffset_B_Start4),
      [ b(start,At_Toffset_B_Start4),
	toffset(start,4,At_Toffset_B_Start4)
      ]).


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',110).
%; Psi
%; RS1
% [object,time]
 % !HoldsAt(directlyIn(object,object),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',112).

 /*  [] ->
         ta(Time,
            tvs1=[Time],
            tvs2=[Time],
            holds_at(neg(directlyIn(DirectlyIn_Object,
                                    DirectlyIn_Object)),
                     Time)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',112).
pl=axiom(holds_at(neg(directlyIn(DirectlyIn_Object,
			      DirectlyIn_Object)),
	       Time),
      []).


%; RS2
% [object1,object2,time]
% HoldsAt(directlyIn(object1,object2),time) ->
% !HoldsAt(directlyIn(object2,object1),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',116).

 /*  [holds_at(directlyIn(DirectlyIn_Object1, DirectlyIn_Object2), Time)] ->
         ta(Time,
            tvs1=[Time],
            tvs2=[Time],
            holds_at(neg(directlyIn(DirectlyIn_Object2,
                                    DirectlyIn_Object1)),
                     Time)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',116).
pl=axiom(holds_at(neg(directlyIn(DirectlyIn_Object2,
			      DirectlyIn_Object1)),
	       Time),
      [ holds_at(directlyIn(DirectlyIn_Object1,
			    DirectlyIn_Object2),
		 Time)
      ]).


%; RS3
% [object1,object2,object3,time]
% HoldsAt(directlyIn(object1,object2),time) &
% HoldsAt(directlyIn(object2,object3),time) ->
% !HoldsAt(directlyIn(object1,object3),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',121).

 /*  [holds_at(directlyIn(Object1, DirectlyIn_Object2), Time), holds_at(directlyIn(DirectlyIn_Object2, DirectlyIn_Object3), Time)] ->
         ta(Time,
            tvs1=[Time],
            tvs2=[Time],
            holds_at(neg(directlyIn(Object1, DirectlyIn_Object3)),
                     Time)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',121).
pl=axiom(holds_at(neg(directlyIn(Object1,DirectlyIn_Object3)),
	       Time),
      [ holds_at(directlyIn(Object1,DirectlyIn_Object2),
		 Time),
	holds_at(directlyIn(DirectlyIn_Object2,
			    DirectlyIn_Object3),
		 Time)
      ]).


%; RS4
% [object,object1,object2,time]
% HoldsAt(directlyIn(object,object1),time) &
% HoldsAt(directlyIn(object,object2),time) ->
% object1=object2.
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',127).

 /*  [holds_at(directlyIn(Object, DirectlyIn_Object1), Time), holds_at(directlyIn(Object, Equals_DirectlyIn_Object2), Time)] ->
         ta(Time,
            tvs1=[Time],
            tvs2=[Time],
            equals(DirectlyIn_Object1, Equals_DirectlyIn_Object2)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',127).
pl=axiom(equals(DirectlyIn_Object1,Equals_DirectlyIn_Object2),
      [ holds_at(directlyIn(Object,DirectlyIn_Object1),
		 Time),
	holds_at(directlyIn(Object,
			    Equals_DirectlyIn_Object2),
		 Time)
      ]).


%; RS7
% [object,room,time]
% HoldsAt(directlyIn(object,room),time) ->
% HoldsAt(inRoom(object,room),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',133).

 /*  [holds_at(directlyIn(Object, InRoom_DirectlyIn_Room), Time)] ->
         ta(Time,
            tvs1=[Time],
            tvs2=[Time],
            holds_at(inRoom(Object, InRoom_DirectlyIn_Room), Time)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',133).
pl=axiom(holds_at(inRoom(Object,InRoom_DirectlyIn_Room),
	       Time),
      [ holds_at(directlyIn(Object,InRoom_DirectlyIn_Room),
		 Time)
      ]).


%; RS8
% [object1,object2,room,time]
% HoldsAt(directlyIn(object1,object2),time) &
% HoldsAt(inRoom(object2,room),time) ->
% HoldsAt(inRoom(object1,room),time).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',138).

 /*  [holds_at(directlyIn(Object1, DirectlyIn_Object2), Time), holds_at(inRoom(DirectlyIn_Object2, Room), Time)] ->
         ta(Time,
            tvs1=[Time],
            tvs2=[Time],
            holds_at(inRoom(Object1, Room), Time)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',138).
pl=axiom(holds_at(inRoom(Object1,Room),Time),
      [ holds_at(directlyIn(Object1,DirectlyIn_Object2),
		 Time),
	holds_at(inRoom(DirectlyIn_Object2,Room),
		 Time)
      ]).


%; RS9
% [object,room1,room2,time]
% HoldsAt(inRoom(object,room1),time) &
% HoldsAt(inRoom(object,room2),time) ->
% room1=room2.
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',144).

 /*  [holds_at(inRoom(Object, InRoom_Room1), Time), holds_at(inRoom(Object, Equals_InRoom_Room2), Time)] ->
         ta(Time,
            tvs1=[Time],
            tvs2=[Time],
            equals(InRoom_Room1, Equals_InRoom_Room2)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',144).
pl=axiom(equals(InRoom_Room1,Equals_InRoom_Room2),
      [ holds_at(inRoom(Object,InRoom_Room1),Time),
	holds_at(inRoom(Object,Equals_InRoom_Room2),
		 Time)
      ]).


%; Gamma


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',151).
% HoldsAt(directlyIn(Lisa,LivingRoom),0).

 /*  [] ->
         ta(_, tvs1=[], tvs2=[], initially(directlyIn(lisa, livingRoom))).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',151).
pl=axiom(initially(directlyIn(lisa,livingRoom)),[]).


% HoldsAt(directlyIn(Newspaper,LivingRoom),0).

 /*  [] ->
         ta(_, tvs1=[], tvs2=[], initially(directlyIn(newspaper, livingRoom))).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',151).
pl=axiom(initially(directlyIn(newspaper,livingRoom)),[]).


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',153).
% HoldsAt(directlyIn(Box,LivingRoom),0).

 /*  [] ->
         ta(_, tvs1=[], tvs2=[], initially(directlyIn(box, livingRoom))).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',153).
pl=axiom(initially(directlyIn(box,livingRoom)),[]).


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',155).
%; added:                                                 
%; DMILES REMOVED [room1,room2,time] !HoldsAt(inRoom(room1,room2),time).
%; DMILES REMOVED [room,object,time] !HoldsAt(directlyIn(room,object),time).
%; entailed:


% HoldsAt(directlyIn(Lisa,LivingRoom),5).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',159).

 /*  [b(start, Time_at_Toffset_B_Start5), toffset(start, 5, Time_at_Toffset_B_Start5), ignore(start+5==Time_at_Toffset_B_Start5)] ->
         ta(_,
            tvs1=[start+5],
            tvs2=[Time_at_Toffset_B_Start5, start],
            holds_at(directlyIn(lisa, livingRoom), Time_at_Toffset_B_Start5)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',159).
pl=axiom(holds_at(directlyIn(lisa,livingRoom),
	       Time_at_Toffset_B_Start5),
      [ b(start,Time_at_Toffset_B_Start5),
	toffset(start,5,Time_at_Toffset_B_Start5)
      ]).


:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',161).
% HoldsAt(directlyIn(Box,Kitchen),5).

 /*  [b(start, Time_at_Toffset_B_Start5), toffset(start, 5, Time_at_Toffset_B_Start5), ignore(start+5==Time_at_Toffset_B_Start5)] ->
         ta(_,
            tvs1=[start+5],
            tvs2=[Time_at_Toffset_B_Start5, start],
            holds_at(directlyIn(box, kitchen), Time_at_Toffset_B_Start5)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',161).
pl=axiom(holds_at(directlyIn(box,kitchen),Time_at_Toffset_B_Start5),
      [ b(start,Time_at_Toffset_B_Start5),
	toffset(start,5,Time_at_Toffset_B_Start5)
      ]).


% HoldsAt(inRoom(Newspaper,Kitchen),5).
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',161).

 /*  [b(start, Time_at_Toffset_B_Start5), toffset(start, 5, Time_at_Toffset_B_Start5), ignore(start+5==Time_at_Toffset_B_Start5)] ->
         ta(_,
            tvs1=[start+5],
            tvs2=[Time_at_Toffset_B_Start5, start],
            holds_at(inRoom(newspaper, kitchen), Time_at_Toffset_B_Start5)).
 */
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',161).
pl=axiom(holds_at(inRoom(newspaper,kitchen),
	       Time_at_Toffset_B_Start5),
      [ b(start,Time_at_Toffset_B_Start5),
	toffset(start,5,Time_at_Toffset_B_Start5)
      ]).

% completion Happens
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',163).
:- if(is_e_toplevel).
:- call_pel_directive(completion(happens_at)).
 %  call_pel_directive(completion(happens_at)).
:- endif.

:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',166).
% range time 0 5
:- if(is_e_toplevel).
:- call_pel_directive(range(time, 0, 5)).
 %  call_pel_directive(range(time,0,5)).
:- endif.

% range offset 1 1
:-was_s_l('/opt/logicmoo_workspace/packs_sys/logicmoo_ec/test/ec_planner/ectest/TestBoxRoom.e',166).
:- if(is_e_toplevel).
:- call_pel_directive(range(offset, 1, 1)).
 %  call_pel_directive(range(offset,1,1)).
:- endif.
%; End of file.
