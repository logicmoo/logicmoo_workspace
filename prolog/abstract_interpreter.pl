/*  Part of Extended Libraries for Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xlibrary
    Copyright (C): 2015, Process Design Center, Breda, The Netherlands.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(abstract_interpreter, [abstract_interpreter/3,
                                 abstract_interpreter/4,
                                 abstract_interpreter/5,
                                 get_state/3,
                                 put_state/3,
                                 match_head/4,
                                 match_head_body/3,
                                 bottom/2,
                                 match_ai/5,
                                 call_ai/1,
                                 eval_ai/1,
                                 skip_ai/1,
                                 intr_ai/1,
                                 match_noloops/4]).

:- use_module(library(qualify_meta_goal)).
:- use_module(library(resolve_calln)).
:- use_module(library(term_size)).
:- use_module(library(terms_share)).
:- use_module(library(neck)).

:- meta_predicate
    match_head(0,*,*,*),
    match_head_body(0,*,*),
    match_ai(*,0,*,*,*),
    match_noloops(0,*,*,*),
    abstract_interpreter(0,4,?),
    abstract_interpreter(0,4,+,-),
    abstract_interpreter(+,+,4,?,?),
    call_ai(0),
    eval_ai(0),
    skip_ai(0).

:- multifile
    replace_goal_hook/3,
    replace_body_hook/3,
    evaluable_goal_hook/2,
    evaluable_body_hook/3.

:- dynamic
    evaluable_goal_hook/2.

:- discontiguous
    abstract_interpreter_body/5.

evaluable_body_hook(absolute_file_name(A, _, O), _, (ground(A), ground(O))).
evaluable_body_hook(atom_concat(A, B, C), _,
                    ( nonvar(A), nonvar(B)
                    ; nonvar(A), nonvar(C)
                    ; nonvar(B), nonvar(C)
                    )).
evaluable_body_hook(atomic_list_concat(A, _), _, ground(A)).
evaluable_body_hook(atomic_list_concat(A, B, C), _,
                    ( ground(A), ground(B)
                    ; ground(B), ground(C)
                    )).
evaluable_body_hook(atom_length(A, _), _, ground(A)).
evaluable_body_hook(upcase_atom(A, _), _, ground(A)).
evaluable_body_hook(downcase_atom(A, _), _, ground(A)).
evaluable_body_hook(string_lower(A, _), _, ground(A)).
evaluable_body_hook(string_upper(A, _), _, ground(A)).
evaluable_body_hook(nb_current(A, _), _, ground(A)).
evaluable_body_hook(_ is A, _, ground(A)).
evaluable_body_hook(A > B, _, (ground(A),ground(B))).
evaluable_body_hook(A >= B, _, (ground(A),ground(B))).
evaluable_body_hook(A < B, _, (ground(A),ground(B))).
evaluable_body_hook(A =< B, _, (ground(A),ground(B))).
evaluable_body_hook(A =:= B, _, (ground(A),ground(B))).
evaluable_body_hook(atom_codes(A, B), _, (ground(A);ground(B))).
evaluable_body_hook(atom_chars(A, B), _, (ground(A);ground(B))).
evaluable_body_hook(member(_, L), _, is_list(L)).
evaluable_body_hook(select(_, L, _), _, is_list(L)).
evaluable_body_hook(option(O, L), _, (is_list(L), nonvar(O))).
evaluable_body_hook(nth0(I, L, _), _, (is_list(L);nonvar(I))).
evaluable_body_hook(nth1(I, L, _), _, (is_list(L);nonvar(I))).
evaluable_body_hook(arg(_, C, _), _, nonvar(C)).
evaluable_body_hook(var(V),     _, nonvar(V)).
evaluable_body_hook(nonvar(V),  _, nonvar(V)).
evaluable_body_hook(atomic(A),  _, nonvar(A)).
evaluable_body_hook(atom(A),    _, nonvar(A)).
evaluable_body_hook(is_list(A), _, (ground(A);is_list(A))).
evaluable_body_hook(number(A),  _, nonvar(A)).
evaluable_body_hook(float(A),   _, nonvar(A)).
evaluable_body_hook(integer(A), _, nonvar(A)).
evaluable_body_hook(clause(A, _), _, nonvar(A)).
evaluable_body_hook(clause(A, _, _), _, nonvar(A)).
evaluable_body_hook(format(Out, Format, Args), _,
                    (compound(Out), nonvar(Format), ground(Args))).
evaluable_body_hook(sort(A, _), _, (is_list(A), maplist(nonvar, A))).
evaluable_body_hook(A==B, _, (A==B;A\=B)).

replace_goal_hook(retractall(_), _, true).
replace_goal_hook(retract(_),    _, true).
replace_goal_hook(assertz(_),    _, true).
replace_goal_hook(asserta(_),    _, true).
replace_goal_hook(assert( _),    _, true).
replace_goal_hook(call_ai(G), abstract_interpreter, G).
replace_goal_hook(eval_ai(G), abstract_interpreter, G).
replace_goal_hook(skip_ai(_), abstract_interpreter, true).
replace_goal_hook(V is A, _, (ground(A)->V is A; var(V))).
replace_goal_hook(nb_getval(A, V), _, ignore((nonvar(A), nb_current(A, V)))).
replace_goal_hook(nb_setarg(_, _, _), _, true).

replace_body_hook(with_value(G, _, _), context_values, G).
replace_body_hook(with_value(G, _, _, _), context_values, G).
replace_body_hook(rtcheck_lit(_, G, _), _, G).
replace_body_hook(start_rtcheck(_, G), rtchecks_rt, G).
replace_body_hook('$with_asr'( G, _), ctrtchecks, G).
replace_body_hook('$with_gloc'(G, _), ctrtchecks, G).
replace_body_hook('$with_ploc'(G, _), ctrtchecks, G).
replace_body_hook(intr_ai(G), _, G).

% call during abstract interpretation and execution
call_ai(G) :- call(G).
% call during abstract interpretation but ignore during execution
eval_ai(_).
% ignore during abstract interpretation but call during execution
skip_ai(G) :- call(G).
% abstract interpret but ignore during execution
intr_ai(_).

mod_qual(M, G as R, I:H as B:C) :- !,
    strip_module(M:G, N, H),
    predicate_property(N:H, implementation_module(I)),
    strip_module(M:R, A, C),
    predicate_property(A:C, implementation_module(B)).
mod_qual(M, G, I:F/A) :-
    strip_module(M:G, N, F/A),
    functor(H, F, A),
    predicate_property(N:H, implementation_module(I)).

:- public
    default_on_error/1.

default_on_error(Error) :-
    print_message(error, Error),
    backtrace(40 ).

abstract_interpreter(M:Goal, Abstraction, Options, State) :-
    option(location(Loc),   Options, context(toplevel, Goal)),
    option(evaluable(Eval), Options, []),
    option(on_error(OnErr), Options, abstract_interpreter:default_on_error),
    ( is_list(Eval)->EvalL = Eval ; EvalL = [Eval]), % make it easy
    maplist(mod_qual(M), EvalL, MEvalL),
    abstract_interpreter(Goal, M, Abstraction,
                              state(Loc, MEvalL, M:OnErr, [], [], [], []),
                              State).

abstract_interpreter(Goal, M, Abstraction) -->
    cut_to(abstract_interpreter_body(Goal, M, Abstraction)).

abstract_interpreter(MGoal, Abstraction, Options) :-
    abstract_interpreter(MGoal, Abstraction, Options, _).

:- meta_predicate catch(2, ?, ?, ?, ?).
catch(DCG, Ex, H, S1, S) :-
    catch(call(DCG, S1, S), Ex, H).

cut_to(Goal) --> catch(Goal, cut_from, true).

cut_from.
cut_from :- throw(cut_from).

/*
% alternative (and more efficient) implementation follows:
% Note: this does not work since the choice points could be removed
% by a further cut operation, causing odd behavior
%
:- use_module(library(intercept)).
:- use_module(library(safe_prolog_cut_to)).

:- meta_predicate intercept(2, ?, ?, ?, ?).
intercept(DCG, Ex, H, S1, S) :-
    intercept(call(DCG, S1, S), Ex, H).

cut_to(Goal) -->
    {prolog_current_choice(CP)},
    intercept(Goal, cut_from, catch(safe_prolog_cut_to(CP), _, true)).

cut_from :- send_signal(cut_from).
*/

abstract_interpreter_body(Goal, M, _) -->
    {var(Goal) ; var(M)}, bottom, !.
abstract_interpreter_body(M:Goal, _, Abs) -->
    !,
    abstract_interpreter_body(Goal, M, Abs).
abstract_interpreter_body(@(M:Goal, CM), _, Abs) -->
    !,
    cut_to(abstract_interpreter_lit(Goal, M, CM, Abs)).

abstract_interpreter_body(call(Goal), M, Abs) --> !,
    cut_to(abstract_interpreter_body(Goal, M, Abs)).
abstract_interpreter_body(\+ A, M, Abs) --> !,
    abstract_interpret_body_not(A, M, Abs).

abstract_interpret_body_not(A, M, Abs) -->
    ( cut_to(abstract_interpreter_body(A, M, Abs))
    ->( \+ is_bottom
      ->!,
        {fail}
      ; {fail}
      )
    ; !
    ).
abstract_interpret_body_not(_, _, _) --> bottom.

get_conts(ContL, State, State) :-
    State = state(_, _, _, _, _, ContL, _),
    neck.

put_conts(ContL,
          state(Loc, EvalL, OnErr, CallL, Data, _, Result),
          state(Loc, EvalL, OnErr, CallL, Data, ContL, Result)).

abstract_interpreter_body(catch(Goal, Ex, Handler), M, Abs, S1, S) :-
    !,
    catch(abstract_interpreter_body(Goal, M, Abs, S1, S), Ex,
          ( Handler,
            S = S1
          )).
abstract_interpreter_body(once(Goal), M, Abs, S1, S) :- !,
    once(abstract_interpreter_body(Goal, M, Abs, S1, S)).
abstract_interpreter_body(distinct(Goal), M, Abs, S1, S) :-
    predicate_property(M:distinct(_), implementation_module(solution_sequences)), !,
    distinct(Goal, abstract_interpreter_body(Goal, M, Abs, S1, S)).
abstract_interpreter_body(distinct(Witness, Goal), M, Abs, S1, S) :-
    predicate_property(M:distinct(_, _), implementation_module(solution_sequences)), !,
    distinct(Witness, abstract_interpreter_body(Goal, M, Abs, S1, S)).

ord_spec(asc(_)).
ord_spec(desc(_)).

abstract_interpreter_body(order_by(Spec, Goal), M, Abs, S1, S) :- !,
    ( is_list(Spec),
      Spec \= [],
      maplist(nonvar, Spec),
      maplist(ord_spec, Spec)
    ->order_by(Spec, abstract_interpreter_body(Goal, M, Abs, S1, S))
    ; abstract_interpreter_body(Goal, M, Abs, S1, S)
    ).
abstract_interpreter_body(setup_call_cleanup(S, C, E), M, Abs, S1, S) :- !,
    setup_call_cleanup(abstract_interpreter_body(S, M, Abs, S1, S2),
                       abstract_interpreter_body(C, M, Abs, S2, S3),
                       abstract_interpreter_body(E, M, Abs, S3, S)).
abstract_interpreter_body(call_cleanup(C, E), M, Abs, S1, S) :- !,
    call_cleanup(abstract_interpreter_body(C, M, Abs, S1, S2),
                 abstract_interpreter_body(E, M, Abs, S2, S)).
abstract_interpreter_body((A, B), M, Abs) -->
    !,
    { \+ terms_share(A, B)
    ->CutOnFail = true
    ; CutOnFail = fail
    },
    get_conts(ContL),
    put_conts([B|ContL]),
    abstract_interpreter_body(A, M, Abs),
    put_conts(ContL),
    ( abstract_interpreter_body(B, M, Abs)
    *->[]
    ; { CutOnFail = true
      ->!, fail                 % The whole body will fail
      }
    ).
abstract_interpreter_body((A*->B;C), M, Abs) --> !,
    { \+ terms_share(A, B)
    ->CutOnFail = true
    ; CutOnFail = fail
    },
    ( get_conts(ContL),
      put_conts([B|ContL]),
      abstract_interpreter_body(A, M, Abs)
    *->
      ( put_conts(ContL),
        abstract_interpreter_body(B, M, Abs)
      *->[]
      ; { CutOnFail = true
        ->!, fail                 % The whole body will fail
        }
      )
    ; abstract_interpreter_body(C, M, Abs)
    ).
abstract_interpreter_body((A->B;C), M, Abs) --> !,
    {SCE = s(no)},
    ( interpret_local_cut(A, B, M, Abs, CutElse),
      {nb_setarg(1, SCE, CutElse)}
    ; ( {SCE = s(no)}
      ->abstract_interpreter_body(C, M, Abs)
      )
    ).
abstract_interpreter_body((A;B), M, Abs) --> !,
    ( abstract_interpreter_body(A, M, Abs)
    ; abstract_interpreter_body(B, M, Abs)
    ).
abstract_interpreter_body(A->B, M, Abs) --> !,
    interpret_local_cut(A, B, M, Abs, _).
abstract_interpreter_body(CallN, M, Abs) -->
    {do_resolve_calln(CallN, Goal)}, !,
    cut_to(abstract_interpreter_body(Goal, M, Abs)).

push_top(Prev,
         state(Loc, EvalL, OnErr, CallL, Data, Cont, Prev),
         state(Loc, EvalL, OnErr, CallL, Data, Cont, [])).

pop_top(bottom,
        state(Loc, EvalL, OnErr, CallL, Data, Cont, _),
        state(Loc, EvalL, OnErr, CallL, Data, Cont, bottom)).
pop_top([]) --> [].

% CutElse make the failure explicit wrt. B
interpret_local_cut(A, B, M, Abs, CutElse) -->
    { \+ terms_share(A, B)
    ->CutOnFail = true
    ; CutOnFail = fail
    },
    push_top(Prev),
    get_conts(ContL),
    put_conts([B|ContL]),
    cut_to(abstract_interpreter_body(A, M, Abs)), % loose of precision
    put_conts(ContL),
    ( \+ is_bottom
    ->!,
      { CutElse = yes }
    ; { CutElse = no  }
    ),
    pop_top(Prev),
    ( abstract_interpreter_body(B, M, Abs)
    *->
      []
    ; ( {CutOnFail = true}
      ->cut_if_no_bottom
      ; []
      )
    ).
abstract_interpreter_body(!,    _, _) --> !, cut_if_no_bottom.
abstract_interpreter_body(A=B,  _, _) --> !, {A=B}.
abstract_interpreter_body(A\=B, _, _) -->
    !,
    ( {A\=B}
    ->[]
    ; {A==B}
    ->{fail}
    ; bottom
    ).
abstract_interpreter_body(BinExpr, _, _) -->
    { member(BinExpr, [A=\=B,
                       A=:=B,
                       A>B,
                       A<B,
                       A>=B,
                       A=<B])
    },
    neck,
    !,
    ( { ground(A),
        ground(B)
      }
    ->{BinExpr}
    ; bottom
    ).
abstract_interpreter_body(memberchk(A, B), _, _) -->
    !,
    ( {is_list(B)}
    ->( {nonvar(A)}
      ->{memberchk(A, B)}
      ; {member(A, B)},
        bottom
      )
    ; { append(_, [A|T], B),
        ( var(T)
        ->!
        ; true
        )
      },
      bottom
    ).
abstract_interpreter_body(true, _, _) --> !.
abstract_interpreter_body(fail, _, _) --> !, {fail}.
abstract_interpreter_body(A, M, _) -->
    get_state(state(Loc, _, OnError, _, _, _, _)),
    {evaluable_body_hook(A, M, Condition)},
    !,
    ( {call(Condition)}
    ->{catch(M:A,
             Error,
             ( call(OnError, at_location(Loc, Error)),
               fail
             ))
      }
    ; bottom
    ).

abstract_interpreter_body(G, M, _) -->
    get_state(state(_, EvalL, _, _, _, _, _)),
    { predicate_property(M:G, implementation_module(IM)),
      ( ( evaluable_goal_hook(G, IM)
        ; functor(G, F, A),
          memberchk(IM:F/A, EvalL)
        ),
        R = G
      ; replace_goal_hook(G, IM, R)
      ; memberchk((IM:G as R), EvalL)
      )
    },
    !,
    {call(M:R)}.
abstract_interpreter_body(H, M, Abs) -->
    cut_to(abstract_interpreter_lit(H, M, M, Abs)).

is_bottom(State, State) :-
    State = state(_, _, _, _, _, _, bottom),
    neck.

cut_if_no_bottom -->
    ( \+ is_bottom
    ->{cut_from}
    ; []
    ).

get_state(State, State, State).

put_state(State, _, State).

abstract_interpreter_lit(H, M, CM, Abs) -->
    { predicate_property(M:H, meta_predicate(Meta))
    ->qualify_meta_goal(CM:H, Meta, Goal)
    ; Goal = H
    },
    {predicate_property(M:Goal, implementation_module(IM))},
    get_state(state(Loc, EvalL, OnError, CallL, Data, Cont, Result)),
    ( {member(MCall, CallL),
       MCall =@= IM:Goal
      }
    ->bottom
    ; {copy_term(IM:Goal, MCall)},
      put_state(state(Loc, EvalL, OnError, [MCall|CallL], Data, Cont, Result)),
      ( { replace_body_hook(Goal, IM, Body)
        ; copy_term(EvalL, EvalC), % avoid undesirable unifications
          memberchk((IM:Goal :- Body), EvalC)
        }
      ->cut_to(abstract_interpreter_body(Body, M, Abs))
      ; { \+ predicate_property(M:Goal, defined) }
      ->{ call(OnError, error(existence_error(procedure, M:Goal), Loc)),
          % TBD: information to error
          fail
        }
      ; call(Abs, M:Goal, BM:Body),
        cut_to(abstract_interpreter_body(Body, BM, Abs))
      )
    ).

% top: empty set
% bottom: I don't know, universe set.
% true: exact result

bottom(state(Loc, EvalL, OnErr, CallL, D, Cont, _),
       state(Loc, EvalL, OnErr, CallL, D, Cont, bottom)).

:- multifile match_ai/5.

match_ai(head,    MG, Body) --> match_head(   MG, Body).
match_ai(noloops, MG, Body) --> match_noloops(MG, Body).

match_head(MGoal, M:true) -->
    {predicate_property(MGoal, interpreted)},
    !,
    {strip_module(MGoal, M, _)},
    get_state(state(_, EvalL, OnErr, CallL, D, Cont, Result)),
    put_state(state(Loc, EvalL, OnErr, CallL, D, Cont, Result)),
    {match_head_body(MGoal, Body, Loc)},
    ( {Body = _:true}
    ->[]
    ; bottom %% loose of precision
    ).
match_head(MGoal, M:true) -->
    {strip_module(MGoal, M, _)},
    bottom.

match_head_body(MGoal, CMBody, From) :-
    strip_module(MGoal, M, Goal),
    ( extra_clauses(Goal, M, CMBody, From)
    ; From = clause(Ref),
      clause(MGoal, Body, Ref),
      clause_property(Ref, module(CM)),
      CMBody = CM:Body
    ).

:- use_module(library(interface), []).

:- multifile extra_clauses/4.

extra_clauses(Goal, CM, I:Goal, _From) :-
    predicate_property(CM:Goal, implementation_module(M)),
    functor(Goal, F, A),
    ( interface:'$interface'(M, DIL, IIL),
      ( memberchk(F/A, DIL)
      ; memberchk(F/A, IIL)
      )
    ->interface:'$implementation'(I, M)
    ).

match_noloops(MGoal, Body) -->
    {predicate_property(MGoal, interpreted)},
    !,
    {strip_module(MGoal, M, Goal)},
    get_state(state(Loc1, EvalL, OnErr, CallL, S, Cont, Result1)),
    { functor(Goal, F, A),
      term_size(Goal, Size),
      \+ ( memberchk(M:F/A-Size1, S),
           Size1=<Size
         )
    ->match_head_body(MGoal, Body, Loc),
      Result = Result1
    ; Loc = Loc1,
      Result = bottom %% loose of precision
    },
    put_state(state(Loc, EvalL, OnErr, CallL, [M:F/A-Size|S], Cont, Result)).
match_noloops(MGoal, M:true) -->
    {strip_module(MGoal, M, _)},
    bottom.
