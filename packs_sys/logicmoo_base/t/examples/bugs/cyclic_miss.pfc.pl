
:- module(cyclic_miss,[next/2]).


% remove positive version of P when ~P is asserted
next( ~(P) , \+P).

% remove negative (~) version of P when P is asserted
next( P , \+ ~(P)).

% Allow defintional assertions to run early
next(on_start(G)/definitional(G),G).
next((on_start(G),running_program),G).


end_of_file.


% update cause database "next" forward cycle
:- dynamic(fwd_next/1).
fwd_next(P)==>P.

% remove positive version of P when ~P is asserted
~P ==> \+P.

% remove negative (~) version of P when P is asserted
 P ==> \+ ~P.

% warn user if there is conflict
 ~P, P==> {print_message(warn,conflict(P))}.

% warn user if clause removals fail
\+P, P==> {print_message(warn,weak_conflict(P))}.

% Allow defintional assertions to create side effects before the program runs
:- require(definitional/1).
:- require(running_program/0).
:- dynamic(on_start/1).
on_start(G)/definitional(G) ==>G.
on_start(G),running_program ==>G.





?- [cyclic_checks].
true.

?- listing(==>).
:- dynamic (==>)/2.

fwc(A)==>A.
on_start(A), running_program==>A.
on_start(A)/definitional(A)==>A.
\+A, A==>pfc_warn(weak_conflict(A)).
~A, A==>pfc_warn(conflict(A)).
A==> \+ ~A.
~A==> \+A.

true.

?-  set_prolog_flag(occurs_check,true).
true.

?- clause(A ==> \+ ~A,true,R),nl,clause(H,B,R).

A = fwc(\+ ~A),
R = <clause>(0x15fe950),
H =  (fwc(_2542716)==>_2542716),
B = true ;

A =  (on_start(\+ ~A), running_program),
R = <clause>(0x15d8eb0),
H =  (on_start(_2542722), running_program==>_2542722),
B = true ;

A = on_start(_S1)/definitional(_S1), % where
    _S1 =  (\+ ~ (on_start(_S1)/definitional(_S1))),
R = <clause>(0x15c4b30),
H =  (on_start(_2542726)/definitional(_2542726)==>_2542726),
B = true ;

R = <clause>(0x1597750),
H =  (_2542718==> \+ ~_2542718),
B = true ;

A = ~ (~A),
R = <clause>(0x1597090),
H =  (~_2542724==> \+_2542724),
B = true.

?-


?- set_prolog_flag(occurs_check,true).


?- clause(A ==> \+ ~A,true,R),nl,clause(H,B,R).

A = pfclog(\+ ~A),
R = <clause>(0x5becdb0),
H =  (pfclog(_10254)==>_10254),
B = true ;

A = onStart(_S1)/definitional(_S1), % where
    _S1 =  (\+ ~ (onStart(_S1)/definitional(_S1))),
R = <clause>(0x2e48990),
H =  (onStart(_10264)/definitional(_10264)==>_10264),
B = true ;

A = ~ (~A),
R = <clause>(0x2d4e280),
H =  (~_10262==> \+_10262),
B = true ;

R = <clause>(0x2d464f0),
H =  (_10256==> \+ ~_10256),
B = true.




?- set_prolog_flag(occurs_check,true).

?- clause_asserted(A ==> \+ ~A,true,R).
Foreign predicate clause/3 did not clear exception: error(occurs_check(_48682462,\+ ~_48682472),context(system:clause/3,_48682478))
R = <clause>(0x2d464f0).

?- clause_asserted(A ==> \+ ~A,true,R).


?- clause(A ==> \+ ~A,true,R),nl,clause(H,B,R).

A = pfclog(\+ ~A),
R = <clause>(0x5becdb0),
H =  (pfclog(_48682588)==>_48682588),
B = true ;

A = onStart(_S1)/definitional(_S1), % where
    _S1 =  (\+ ~ (onStart(_S1)/definitional(_S1))),
R = <clause>(0x2e48990),
H =  (onStart(_48682598)/definitional(_48682598)==>_48682598),
B = true ;

A = ~ (~A),
R = <clause>(0x2d4e280),
H =  (~_48682596==> \+_48682596),
B = true ;

R = <clause>(0x2d464f0),
H =  (_48682590==> \+ ~_48682590),
B = true.


clause_asserted(M:H,B,R):- 
   copy_term(M:H:-B,MHB),
   system:clause(M:H,B,R),
   variant(M:H:-B,MHB).




?- clause(A ==> \+ ~A,true).

,clause(H,B,R).

%% expand_to_hb( ?Clause, ?H, ?B) is semidet.
%
% Split a Head+Body from Clause.
%
expand_to_hb( H,  H,  true) :- var(H),!.
% expand_to_hb( Var, H, B):- var(Var),!,dmsg(warn(expand_to_hb( Var, H, B))), when(nonvar(Var),expand_to_hb( Var, H, B)).
expand_to_hb(M:(M2:H :- B),HH,BB):-M==M2,!,expand_to_hb((M:H :- B),HH,BB).
expand_to_hb((H :- B),H,B):-!.
expand_to_hb( M:HB,  M:H,B):- !,expand_to_hb(HB,H,B).
expand_to_hb(  ~(HB),   ~(H),B):- !,expand_to_hb(HB,H,B).
expand_to_hb( H,  H,  true).



get_mpred_assertion_status(P,Was):-
 copy_term(P,PP),
 (clause_asserted(P)-> Was=identical;
  clause(PP,true))-> 
        (Was= partial(PPP));Was= unique)))))).


get_mpred_assertion_status(P,PP,Was):-
  maybe_notrace(((clause_asserted_u(P)-> Was=identical;
    ( (((locally(set_prolog_flag(occurs_check,true),clause_u(PP)),cyclic_break((PPP)))-> 
        (Was= partial(PPP));Was= unique)))))).


