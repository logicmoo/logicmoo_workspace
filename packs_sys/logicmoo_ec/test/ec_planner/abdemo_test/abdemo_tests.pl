
:- style_check(-discontiguous).

:- set_prolog_flag(ec_loader,true).


/*
abdemo_type(_Type, Goal, [PS, NS, BS]):-
  abdemo(Goal, [P0, Bs], N), !,
  (merge_events_and_fluents(P0,N,P)-> true ; P=P0),
  sort_plan(Bs, P, PS), 
  sort_plan(Bs, N, NS), 
  % ignore(N=NS), 
  sort_plan(Bs, Bs, BS), !.

merge_events_and_fluents([H|T],[H2|T2],[[H|H2]|List):- 
   merge_events_and_fluents(T,T2,List).
merge_events_and_fluents([],[],[]):- !.

sort_plan(Bs, P, PS):- predsort(sort_r_keys(Bs), P, PS), !.
sort_plan(_, P, P).

sort_r_keys(Bs, R, A, B):- get_t_arg(A, A1, A2), get_t_arg(B, B1, B2), which_first(Bs, R, A1, A2, B1, B2), R \== (=), !.
sort_r_keys(_, R, A, B):- compare(R, A, B).

which_first(Bs, ( R ), A1, A2, B1, B2):- \+ atomic(A2), atomic(A1), !, which_first(Bs, ( R ), A1, A1, B1, B2).
which_first(Bs, ( R ), A1, A2, B1, B2):- \+ atomic(B2), atomic(B1), !, which_first(Bs, ( R ), A1, A2, B1, B1).
which_first(Bs, ( R ), A1,  _, B1,  _):- which_first2(Bs, ( R ), A1, B1), (R \== (=)), !.
which_first(Bs, ( R ),  _, A2,  _, B2):- which_first2(Bs, ( R ), A2, B2), !.

which_first2( _, ( R ), A1, B1):- (\+atomic(A1) ; \+atomic(A1)), !, compare(R, A1, B1).
%which_first2( _, ( = ), A1, B1):- A1==B1, !.
which_first2(Bs, ( < ), A1, B1):- member(b(A1, B1), Bs), !.
which_first2(Bs, ( > ), A1, B1):- member(b(B1, A1), Bs), !.
which_first2(Bs, ( < ), A1, B1):- member(b(A1, M1), Bs), member(b(M1, B1), Bs), !.
which_first2(Bs, ( < ), A1, B1):- member(b(A1, M1), Bs), member(b(M1, N1), Bs), member(b(N1, B1), Bs), !.
which_first2(Bs, ( > ), A1, B1):- which_first2(Bs, (<), B1, A1).
which_first2( _, ( R ), A1, B1):- compare(R, A1, B1).

get_t_arg(B, B1, B2):- \+ compound(B), !, B1= B, B2= B.
get_t_arg([B|_], B1, B2):- !, get_t_arg(B, B1, B2).
get_t_arg(clipped(B1, _, B1), B1, B2):- !.
get_t_arg(b(B1, B2), B1, B2):-!.
get_t_arg(beq(B1, B2), B1, B2):-!.
get_t_arg(B, B1, B2):- functor(B, _, A), arg(A, B, B1), A2 is A -1, arg(A2, B, B2).
*/
:- set_prolog_flag(ec_loader,true).


:- if(false).

/*
   Formulae for the shopping example from Russell and Norvig

*/
demo_test(shop_1, common, [holds(have(A0,o(1)), T)]).
demo_test(shop_3, common, [holds(have(A0,o(1)), T), holds(have(A0,o(2)), T), holds(have(A0,o(3)), T)]).
demo_test(shop_4, common, [holds(have(A0,o(1)), T), holds(have(A0,o(2)), T), holds(have(A0,o(3)), T), holds(have(A0,o(4)), T)]).
demo_test(shop_5, common, [holds(have(A0,o(1)), T), holds(have(A0,o(2)), T), holds(have(A0,o(3)), T), holds(have(A0,o(4)), T), holds(have(A0,o(5)), T)]).
demo_test(shop_6, slow, [holds(have(A0,o(1)), T), holds(have(A0,o(2)), T), holds(have(A0,o(3)), T), holds(have(A0,o(4)), T), holds(have(A0,o(5)), T), holds(have(A0,o(6)), T)]).
demo_test(shop_7, slow, [holds(have(A0,o(1)), T), holds(have(A0,o(2)), T), holds(have(A0,o(3)), T), holds(have(A0,o(4)), T), holds(have(A0,o(5)), T), holds(have(A0,o(6)), T), holds(have(A0,o(7)), T)]).
demo_test(shop_8, slow, [holds(have(A0,o(1)), T), holds(have(A0,o(2)), T), holds(have(A0,o(3)), T), holds(have(A0,o(4)), T), holds(have(A0,o(5)), T), holds(have(A0,o(6)), T), holds(have(A0,o(7)), T), holds(have(A0,o(8)), T)]).
demo_test(shop_9, slow, [holds(have(A0,o(1)), T), holds(have(A0,o(2)), T), holds(have(A0,o(3)), T), holds(have(A0,o(4)), T), holds(have(A0,o(5)), T), holds(have(A0,o(6)), T), holds(have(A0,o(7)), T), holds(have(A0,o(8)), T), holds(have(A0,o(9)), T)]).
demo_test(shop_10, slow, [holds(have(A0,o(1)), T), holds(have(A0,o(2)), T), holds(have(A0,o(3)), T), holds(have(A0,o(4)), T), holds(have(A0,o(5)), T), holds(have(A0,o(6)), T), holds(have(A0,o(7)), T), holds(have(A0,o(8)), T), holds(have(A0,o(9)), T), holds(have(A0,o(10)), T)]).
demo_test(shop_12, slow, [holds(have(A0,o(1)), T), holds(have(A0,o(2)), T), holds(have(A0,o(3)), T), holds(have(A0,o(4)), T), holds(have(A0,o(5)), T), holds(have(A0,o(6)), T), holds(have(A0,o(7)), T), holds(have(A0,o(8)), T), holds(have(A0,o(9)), T), holds(have(A0,o(10)), T), holds(have(A0,o(11)), T), holds(have(A0,o(12)), T)]).
demo_test(shop_14, slow, [holds(have(A0,o(1)), T), holds(have(A0,o(2)), T), holds(have(A0,o(3)), T), holds(have(A0,o(4)), T), holds(have(A0,o(5)), T), holds(have(A0,o(6)), T), holds(have(A0,o(7)), T), holds(have(A0,o(8)), T), holds(have(A0,o(9)), T), holds(have(A0,o(10)), T), holds(have(A0,o(11)), T), holds(have(A0,o(12)), T), holds(have(A0,o(13)), T), holds(have(A0,o(14)), T)]).
demo_test(shop_16, slow, [holds(have(A0,o(1)), T), holds(have(A0,o(2)), T), holds(have(A0,o(3)), T), holds(have(A0,o(4)), T), holds(have(A0,o(5)), T), holds(have(A0,o(6)), T), holds(have(A0,o(7)), T), holds(have(A0,o(8)), T), holds(have(A0,o(9)), T), holds(have(A0,o(10)), T), holds(have(A0,o(11)), T), holds(have(A0,o(12)), T), holds(have(A0,o(13)), T), holds(have(A0,o(14)), T), holds(have(A0,o(15)), T), holds(have(A0,o(16)), T)]).

%:- rtrace.

% 
% :- cls.

event(act(_,_)).
axiom(initiates(buy(A0,X), have(A0,X), T), [sells(Y, X), holds( at_loc(A0,Y), T)]).
axiom(initiates(go(A0,X), at_loc(A0,X), _T), []).
axiom(terminates(go(A0,X), at_loc(A0,Y), T), [difstate(f,X, Y)]).
axiom(sells(s(X), o(X)), []).

event(go(A0,X)).
event(buy(A0,X)).

demo_test(shop_2_2, common, [holds(state(f,2), T)]).
demo_test(shop_2_4, common, [holds(state(f,4), T)]).
demo_test(shop_2_6, common, [holds(state(f,6), T)]).
demo_test(shop_2_8, common, [holds(state(f,8), T)]).
demo_test(shop_2_10, slow, [holds(state(f,10), T)]).
demo_test(shop_2_12, slow, [holds(state(f,12), T)]).
demo_test(shop_2_14, slow, [holds(state(f,14), T)]).
demo_test(shop_2_16, slow, [holds(state(f,16), T)]).


axiom(initiates(act(a,1), state(f,1), _T), []).

axiom(initiates(act(a,2), state(f,2), T), [holds(state(f,1), T)]).
axiom(initiates(act(a,3), state(f,3), T), [holds(state(f,2), T)]).
axiom(initiates(act(a,4), state(f,4), T), [holds(state(f,3), T)]).
axiom(initiates(act(a,5), state(f,5), T), [holds(state(f,4), T)]).
axiom(initiates(act(a,6), state(f,6), T), [holds(state(f,5), T)]).
axiom(initiates(act(a,7), state(f,7), T), [holds(state(f,6), T)]).
axiom(initiates(act(a,8), state(X,8), T), [holds(state(X,7), T)]).
axiom(initiates(act(a,9), state(f,9), T), [holds(state(f,8), T)]).
axiom(initiates(act(a,10), state(f,10), T), [holds(state(f,9), T)]).
axiom(initiates(act(a,11), state(f,11), T), [holds(state(f,10), T)]).
axiom(initiates(act(a,12), state(f,12), T), [holds(state(f,11), T)]).
axiom(initiates(act(a,13), state(f,13), T), [holds(state(f,12), T)]).
axiom(initiates(act(a,14), state(f,14), T), [holds(state(f,13), T)]).
axiom(initiates(act(a,15), state(f,15), T), [holds(state(f,14), T)]).
axiom(initiates(act(a,16), state(f,16), T), [holds(state(f,15), T)]).

axiom(initially(f1(1)), []).

axiom(holds(state(f,X),T), [holds(f1(X),T)]).



% 

axiom(holds(plant_safe, T), [holds(tank_empty, T), holds(temperature_low, T)]).
axiom(initiates(drain_tank, tank_empty, T), [holds(pressure_normal, T)]).
axiom(initiates(cool_tank, temperature_low, _T), []).
axiom(holds(pressure_normal, T), [holds(valve_open, T)]).
axiom(holds(pressure_normal, T), [holds(boiler_off, T)]).
axiom(initiates(open_valve, valve_open, _T), []).
axiom(initiates(turn_off_boiler, boiler_off, _T), []).

event(drain_tank).
event(cool_tank).
event(open_valve).
event(turn_off_boiler).

demo_test(plant_safe1, common, [holds(plant_safe, t)]).



:-set_prolog_flag(redundant_rules, true).

% Formulae for the shopping example from Russell and Norvig
% Here's a demo_test(shop_ query.)
demo_test(shopping4, common, [holds(have(A0,drill), T), holds(have(A0,milk), T), holds(have(A0,banana), T), holds( at_loc(A0,home), T)]).
 % Here's a simpler one.
demo_test(shopping2, common, [holds(have(A0,drill), T), holds(have(A0,milk), T)]).

axiom(initiates(go(A0,X), at_loc(A0,X), _T), []):-current_prolog_flag(redundant_rules, true).
axiom(terminates(go(A0,X), at_loc(A0,Y), T), [difstate(f,X, Y)]):- current_prolog_flag(redundant_rules, true).
axiom(initiates(buy(A0,X), have(A0,X), T), [sells(Y, X), holds( at_loc(A0,Y), T)]):-current_prolog_flag(redundant_rules, true).
axiom(sells(hws, drill), []).
axiom(sells(sm, milk), []).
axiom(sells(sm, banana), []).


event(go(A0,X)):-current_prolog_flag(redundant_rules, true).
event(buy(A0,X)):-current_prolog_flag(redundant_rules, true).

:- endif.

:- if(true).


/*
 Formulae for the mail delivery domain.
 Example queries:
*/
demo_test(mail_1, loops, [holds(in(p(1), r(3)), T)]).
demo_test(mail_2TT, loops, [holds(in(p(1), r(3)), T), holds(not(got(robot,p(1))), T)]).
demo_test(mail_1t, loops, [holds(in(p(1), r(2)), t)]).
demo_test(mail_2T, loops, [holds(in(p(1), r(3)), T)]).
demo_test(mail_2t, loops, [holds(in(p(1), r(3)), t)]).

/* There should probably be some releases clauses for compound actions */
/* Compound actions */

axiom(happens(shift_pack(P, R), T1, T4), [happens(retrieve_pack(P), T1, T2), b(T2, T3), happens(deliver_pack(P, R), T3, T4), not(clipped(T2, got(robot,P), T3))]).
axiom(initiates(shift_pack(P, R), in(P, R), _T), []).

axiom(happens(retrieve_pack(P), T1, T2), [holds(in(P, R), T1), happens(go_to_room(robot,R), T1), happens(put_up(robot,P), T2), b(T1, T2), not(clipped(T1, in(robot, R), T2))]).
axiom(initiates(retrieve_pack(P), got(robot,P), _T), []).

axiom(happens(deliver_pack(P, R), T1, T2), [happens(go_to_room(robot,R), T1), happens(put_down(robot,P), T2), b(T1, T2), not(clipped(T1, in(robot, R), T2))]).
axiom(initiates(deliver_pack(P, R), in(P, R), T), [holds(got(robot,P), T)]).

/* Primitive actions */

axiom(initiates(put_up(robot,P), got(robot,P), T), [difstate(f,P, robot), holds(in(P, R), T), holds(in(robot, R), T)]).
axiom(releases(put_up(robot,P), in(P, R), T), [difstate(f,P, robot), holds(in(P, R), T), holds(in(robot, R), T)]).

axiom(initiates(put_down(robot,P), in(P, R), T), [difstate(f,P, robot), holds(got(robot,P), T), holds(in(robot, R), T)]).
axiom(terminates(put_down(robot,P), got(robot,P), _T), []).

axiom(initiates(go_to_room(robot,R), in(robot, R), _T), []).
axiom(terminates(go_to_room(robot,R1), in(robot, R2), T), [difstate(f,R1, R2)]).

/* Domain constraints */

axiom(holds(in(P, R), T), [difstate(f,P, robot), holds(got(robot,P), T), holds(in(robot, R), T)]).

/* Narrative */

axiom(initially(in(robot, r(1))), []).
axiom(initially(not(in(robot, r(2)))), []).
axiom(initially(not(in(robot, r(3)))), []).
axiom(initially(in(p(1), r(2))), []).
axiom(initially(not(in(p(1), r(1)))), []).
axiom(initially(not(in(p(1), r(3)))), []).

event(put_up(robot,P)).
event(put_down(robot,P)).
event(go_to_room(robot,R)).


/*
% axiom(happens(deliver_pack(P, R), T1, T2), [happens(go_to_room(robot,R), T1), happens(put_down(robot,P), T2), b(T1, T2), not(clipped(T1, in(robot, R), T2))]).
event(made_true(_)).
%axiom(initiates(made_true(G), G, T), []).

%axiom_local(happens(made_true(G), T1, T2), [holds(G, T1), b(T1, T2), not(clipped(T1, G, T2))]).
axiom_local(initiates(made_true(G), became_true(G), T), []):- nonvar(G).
axiom_local(initiates(made_true(G), G, T), []):- nonvar(G).
%axiom_local(holds(G, T), [holds(became_true(G), T)]):- nonvar(G).

%demo_test(became_true_1a, advanced, [holds(in(p(1), r(3)), T)]).
%demo_test(became_true_1a, advanced, [holds(made_true(in(p(1), r(3)), T))]).
%demo_test(became_true_1b, advanced, [holds(became_true(in(p(1), r(3)), T))]).
%demo_test(became_true_2b, advanced, [happens(put_up(robot,P),T)]).
%demo_test(became_true_2a, advanced, [happens(made_true(foo),T)]).
%demo_test(became_true_3a, advanced, [made_true(in(p(1), r(3)))]).
%demo_test(became_true_3b, advanced, [holds(became_true(in(p(1), r(3))),T)]).
demo_test(became_true_3b, advanced, [holds(became_true(foo),T)]).
demo_test(became_true_3c, advanced, [holds(foo,T)]).
*/  
:- endif.
:- set_prolog_flag(ec_loader,true).

/* Abduction policy */
predicate(dummy).

:- set_prolog_flag(ec_loader,false).

:- nortrace.
demo_test :- 
   time((mmake, forall(ec:demo_test(_), true), 
  % time(with_output_to(string(_),forall(between(1,50,_),forall(demo_test(_), true)))),
   forall(ec:demo_test(_), true))).

:- system:show_ec_current_domain_db.

