
% :- style_check(-discontiguous).
ec_trace(on, 0).

init_gensym(_).
ticks(Z1):-statistics(runtime, [Z1, _]).
dbginfo(R):- pprint_ecp_cmt(yellow, R).

run_demo_test(Test, Type, Goal):- 
  Name = run_demo_test(Test, Type, Goal), 
  pprint_ecp_cmt(blue, begin(Name)), 
  (once(abdemo_type(Type, Goal, R)) -> pprint_ecp_cmt(green, R) ; pprint_ecp_cmt(pink, failed(Name))). 

abdemo(Gs, R, N) :-
     ticks(Z1), abdemo(Gs, [[], []], R, [], N), ticks(Z2), 
     Z is (Z2-Z1)/60, write('Total time taken '), writeln(Z), nl.

abdemo_type(_Type, Goal, [PS, NS, BS]):-
  abdemo(Goal, [PS, BS], NS), ! .
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

demo_test(Name):- 
  demo_test(Name, Type, Goal), 
  Type \== slow, 
  run_demo_test(Name, Type, Goal).
 

/*
   Formulae for the shopping example from Russell and Norvig

*/
demo_test(shop_1, common, [holds_at(have(A0,o1), T)]).
demo_test(shop_3, common, [holds_at(have(A0,o1), T), holds_at(have(A0,o2), T), holds_at(have(A0,o3), T)]).
demo_test(shop_4, common, [holds_at(have(A0,o1), T), holds_at(have(A0,o2), T), holds_at(have(A0,o3), T), holds_at(have(A0,o4), T)]).
demo_test(shop_5, common, [holds_at(have(A0,o1), T), holds_at(have(A0,o2), T), holds_at(have(A0,o3), T), holds_at(have(A0,o4), T), holds_at(have(A0,o5), T)]).
demo_test(shop_6, slow, [holds_at(have(A0,o1), T), holds_at(have(A0,o2), T), holds_at(have(A0,o3), T), holds_at(have(A0,o4), T), holds_at(have(A0,o5), T), holds_at(have(A0,o6), T)]).
demo_test(shop_7, slow, [holds_at(have(A0,o1), T), holds_at(have(A0,o2), T), holds_at(have(A0,o3), T), holds_at(have(A0,o4), T), holds_at(have(A0,o5), T), holds_at(have(A0,o6), T), holds_at(have(A0,o7), T)]).
demo_test(shop_8, slow, [holds_at(have(A0,o1), T), holds_at(have(A0,o2), T), holds_at(have(A0,o3), T), holds_at(have(A0,o4), T), holds_at(have(A0,o5), T), holds_at(have(A0,o6), T), holds_at(have(A0,o7), T), holds_at(have(A0,o8), T)]).
demo_test(shop_9, slow, [holds_at(have(A0,o1), T), holds_at(have(A0,o2), T), holds_at(have(A0,o3), T), holds_at(have(A0,o4), T), holds_at(have(A0,o5), T), holds_at(have(A0,o6), T), holds_at(have(A0,o7), T), holds_at(have(A0,o8), T), holds_at(have(A0,o9), T)]).
demo_test(shop_10, slow, [holds_at(have(A0,o1), T), holds_at(have(A0,o2), T), holds_at(have(A0,o3), T), holds_at(have(A0,o4), T), holds_at(have(A0,o5), T), holds_at(have(A0,o6), T), holds_at(have(A0,o7), T), holds_at(have(A0,o8), T), holds_at(have(A0,o9), T), holds_at(have(A0,o10), T)]).
demo_test(shop_12, slow, [holds_at(have(A0,o1), T), holds_at(have(A0,o2), T), holds_at(have(A0,o3), T), holds_at(have(A0,o4), T), holds_at(have(A0,o5), T), holds_at(have(A0,o6), T), holds_at(have(A0,o7), T), holds_at(have(A0,o8), T), holds_at(have(A0,o9), T), holds_at(have(A0,o10), T), holds_at(have(A0,o11), T), holds_at(have(A0,o12), T)]).
demo_test(shop_14, slow, [holds_at(have(A0,o1), T), holds_at(have(A0,o2), T), holds_at(have(A0,o3), T), holds_at(have(A0,o4), T), holds_at(have(A0,o5), T), holds_at(have(A0,o6), T), holds_at(have(A0,o7), T), holds_at(have(A0,o8), T), holds_at(have(A0,o9), T), holds_at(have(A0,o10), T), holds_at(have(A0,o11), T), holds_at(have(A0,o12), T), holds_at(have(A0,o13), T), holds_at(have(A0,o14), T)]).
demo_test(shop_16, slow, [holds_at(have(A0,o1), T), holds_at(have(A0,o2), T), holds_at(have(A0,o3), T), holds_at(have(A0,o4), T), holds_at(have(A0,o5), T), holds_at(have(A0,o6), T), holds_at(have(A0,o7), T), holds_at(have(A0,o8), T), holds_at(have(A0,o9), T), holds_at(have(A0,o10), T), holds_at(have(A0,o11), T), holds_at(have(A0,o12), T), holds_at(have(A0,o13), T), holds_at(have(A0,o14), T), holds_at(have(A0,o15), T), holds_at(have(A0,o16), T)]).

axiom(initiates(go(A0,X), at_loc(A0,X), T), []).
axiom(terminates(go(A0,X), at_loc(A0,Y), T), [diff(X, Y)]).
axiom(initiates(buy(A0,X), have(A0,X), T), [sells(Y, X), holds_at( at_loc(A0,Y), T)]).
axiom(sells(s1, o1), []).
axiom(sells(s2, o2), []).
axiom(sells(s3, o3), []).
axiom(sells(s4, o4), []).
axiom(sells(s5, o5), []).
axiom(sells(s6, o6), []).
axiom(sells(s7, o7), []).
axiom(sells(s8, o8), []).
axiom(sells(s9, o9), []).
axiom(sells(s10, o10), []).
axiom(sells(s11, o11), []).
axiom(sells(s12, o12), []).
axiom(sells(s13, o13), []).
axiom(sells(s14, o14), []).
axiom(sells(s15, o15), []).
axiom(sells(s16, o16), []).

/* Abduction policy */
abducible(dummy).
executable(go(A0,X)).
executable(buy(A0,X)).

demo_test(shop_2_2, common, [holds_at(f2, T)]).
demo_test(shop_2_4, common, [holds_at(f4, T)]).
demo_test(shop_2_6, common, [holds_at(f6, T)]).
demo_test(shop_2_8, common, [holds_at(f8, T)]).
demo_test(shop_2_10, slow, [holds_at(f10, T)]).
demo_test(shop_2_12, slow, [holds_at(f12, T)]).
demo_test(shop_2_14, slow, [holds_at(f14, T)]).
demo_test(shop_2_16, slow, [holds_at(f16, T)]).



axiom(initiates(a1, f1, T), []).
axiom(initiates(a2, f2, T), [holds_at(f1, T)]).
axiom(initiates(a3, f3, T), [holds_at(f2, T)]).
axiom(initiates(a4, f4, T), [holds_at(f3, T)]).
axiom(initiates(a5, f5, T), [holds_at(f4, T)]).
axiom(initiates(a6, f6, T), [holds_at(f5, T)]).
axiom(initiates(a7, f7, T), [holds_at(f6, T)]).
axiom(initiates(a8, f8, T), [holds_at(f7, T)]).
axiom(initiates(a9, f9, T), [holds_at(f8, T)]).
axiom(initiates(a10, f10, T), [holds_at(f9, T)]).
axiom(initiates(a11, f11, T), [holds_at(f10, T)]).
axiom(initiates(a12, f12, T), [holds_at(f11, T)]).
axiom(initiates(a13, f13, T), [holds_at(f12, T)]).
axiom(initiates(a14, f14, T), [holds_at(f13, T)]).
axiom(initiates(a15, f15, T), [holds_at(f14, T)]).
axiom(initiates(a16, f16, T), [holds_at(f15, T)]).

executable(a1).
executable(a2).
executable(a3).
executable(a4).
executable(a5).
executable(a6).
executable(a7).
executable(a8).
executable(a9).
executable(a10).
executable(a11).
executable(a12).
executable(a13).
executable(a14).
executable(a15).
executable(a16).

demo_test(shop__demo(plant_safe1), common, [holds_at(plant_safe, t)]).
axiom(holds_at(plant_safe, T), [holds_at(tank_empty, T), holds_at(temperature_low, T)]).
axiom(initiates(drain_tank, tank_empty, T), [holds_at(pressure_normal, T)]).
axiom(initiates(cool_tank, temperature_low, T), []).
axiom(holds_at(pressure_normal, T), [holds_at(valve_open, T)]).
axiom(holds_at(pressure_normal, T), [holds_at(boiler_off, T)]).
axiom(initiates(open_valve, valve_open, T), []).
axiom(initiates(turn_off_boiler, boiler_off, T), []).

executable(drain_tank).
executable(cool_tank).
executable(open_valve).
executable(turn_off_boiler).


:-set_prolog_flag(redundant_rules, true).

% Formulae for the shopping example from Russell and Norvig
% Here's a demo_test(shop_ query.)
demo_test(shopping4, common, [holds_at(have(A0,drill), T), holds_at(have(A0,milk), T), holds_at(have(A0,banana), T), holds_at( at_loc(A0,home), T)]).
 % Here's a simpler one.
demo_test(shopping2, common, [holds_at(have(A0,drill), T), holds_at(have(A0,milk), T)]).
axiom(initiates(go(A0,X), at_loc(A0,X), T), []):-current_prolog_flag(redundant_rules, true).
axiom(terminates(go(A0,X), at_loc(A0,Y), T), [diff(X, Y)]):- current_prolog_flag(redundant_rules, true).
axiom(initiates(buy(A0,X), have(A0,X), T), [sells(Y, X), holds_at( at_loc(A0,Y), T)]):-current_prolog_flag(redundant_rules, true).
axiom(sells(hws, drill), []).
axiom(sells(sm, milk), []).
axiom(sells(sm, banana), []).


executable(go(A0,X)):-current_prolog_flag(redundant_rules, true).
executable(buy(A0,X)):-current_prolog_flag(redundant_rules, true).



/*
 Formulae for the mail delivery domain.
 Example queries:
*/
demo_test(mail_1, loops, [holds_at(in(p1, r3), T)]).
demo_test(mail_2TT, loops, [holds_at(in(p1, r3), T), holds_at(neg(got(p1)), T)]).
demo_test(mail_1t, loops, [holds_at(in(p1, r2), t)]).
demo_test(mail_2T, loops, [holds_at(in(p1, r3), T)]).
demo_test(mail_2t, loops, [holds_at(in(p1, r3), t)]).

/* There should probably be some releases clauses for compound actions */
/* Compound actions */

axiom(happens(shift_pack(P, R), T1, T4), [happens(retrieve_pack(P), T1, T2), b(T2, T3), happens(deliver_pack(P, R), T3, T4), not(clipped(T2, got(P), T3))]).
axiom(initiates(shift_pack(P, R), in(P, R), T), []).

axiom(happens(retrieve_pack(P), T1, T2), [holds_at(in(P, R), T1), happens(go_to_room(R), T1), happens(pick_up(P), T2), b(T1, T2), not(clipped(T1, in(robot, R), T2))]).
axiom(initiates(retrieve_pack(P), got(P), T), []).

axiom(happens(deliver_pack(P, R), T1, T2), [happens(go_to_room(R), T1), happens(put_down(P), T2), b(T1, T2), not(clipped(T1, in(robot, R), T2))]).
axiom(initiates(deliver_pack(P, R), in(P, R), T), [holds_at(got(P), T)]).

/* Primitive actions */

axiom(initiates(pick_up(P), got(P), T), [diff(P, robot), holds_at(in(P, R), T), holds_at(in(robot, R), T)]).
axiom(releases(pick_up(P), in(P, R), T), [diff(P, robot), holds_at(in(P, R), T), holds_at(in(robot, R), T)]).

axiom(initiates(put_down(P), in(P, R), T), [diff(P, robot), holds_at(got(P), T), holds_at(in(robot, R), T)]).
axiom(terminates(put_down(P), got(P), T), []).

axiom(initiates(go_to_room(R), in(robot, R), T), []).
axiom(terminates(go_to_room(R1), in(robot, R2), T), [diff(R1, R2)]).

/* Domain constraints */

axiom(holds_at(in(P, R), T), [diff(P, robot), holds_at(got(P), T), holds_at(in(robot, R), T)]).

/* Narrative */

axiom(initially(in(robot, r1)), []).
axiom(initially(neg(in(robot, r2))), []).
axiom(initially(neg(in(robot, r3))), []).
axiom(initially(in(p1, r2)), []).
axiom(initially(neg(in(p1, r1))), []).
axiom(initially(neg(in(p1, r3))), []).

executable(pick_up(P)).
executable(put_down(P)).
executable(go_to_room(R)).

/*
% From unknown:0

 /*   [ [ happens(put_down(p1), t126769, t126769), 
            happens(go_to_room(r3), t126768, t126768), 
            happens(pick_up(p1), t126765, t126765), 
            happens(go_to_room(r2), t126764, t126764)
          ], 
          [ b(t126769, t126751), 
            b(t126765, t126768), 
            b(t126768, t126769), 
            b(t126764, t126765)
          ]
        ].
 */


% From unknown:0

 /*  begin(run_demo_test(mail_2, 
                    loops, 
                    [ holds_at(in(p1, r3), Time_at), 
                      holds_at(neg(got(p1)), Time_at)
                    ])).
 */
Depth: 0

Depth: 1

Depth: 2

Abstract plan: [happens(put_down(p1), t126827, t126827), happens(shift_pack(p1, r3), t126823, t126824)][b(t126827, t126826), b(t126824, t126826)]

Depth: 2

Depth: 3

Abstract plan: [happens(deliver_pack(p1, r3), t126836, t126837), happens(retrieve_pack(p1), t126833, t126834), happens(put_down(p1), t126827, t126827)][b(t126836, t126827), b(t126837, t126826), b(t126834, t126836), b(t126827, t126826)]

Depth: 3

Depth: 4

Abstract plan: [happens(pick_up(p1), t126841, t126841), happens(go_to_room(r2), t126840, t126840), happens(deliver_pack(p1, r3), t126836, t126837), happens(put_down(p1), t126827, t126827)][b(t126841, t126836), b(t126840, t126841), b(t126836, t126827), b(t126837, t126826), b(t126827, t126826)]

Depth: 4

Total time taken 0.016666666666666666



% From unknown:0

 /*   [ [ happens(go_to_room(r3), t126842, t126842), 
            happens(pick_up(p1), t126841, t126841), 
            happens(go_to_room(r2), t126840, t126840), 
            happens(put_down(p1), t126827, t126827)
          ], 
          [ b(t126841, t126842), 
            b(t126842, t126827), 
            b(t126840, t126841), 
            b(t126827, t126826)
          ]
        ].
 */

% 62, 574, 738 inferences, 5.131 CPU in 5.133 seconds (100% CPU, 12194774 Lips)
true.

?- ^D
% halt

real    0m22.226s
user    0m6.226s
sys     0m0.065s
(base) root@mail:/opt/logicmoo_workspa
*/
demo_test :- time((forall(demo_test(_), true), forall(demo_test(_), true))).