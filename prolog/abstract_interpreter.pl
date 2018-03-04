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
                                 match_head/7,
                                 match_head_body/4,
                                 bottom/2,
                                 match_ai/8,
                                 match_noloops/7,
                                 terms_share/2]).

:- use_module(library(implementation_module)).
:- use_module(library(qualify_meta_goal)).
:- use_module(library(resolve_calln)).
:- use_module(library(term_size)).

:- meta_predicate
    match_head(*,*,*,*,*, *,*),
    match_head_body(*,*,*,*),
    match_ai(*,*,*,*,*,*,*,*),
    match_noloops(*,*,*,*,*,*,*),
    abstract_interpreter(0,7,?),
    abstract_interpreter(0,7,+,-),
    abstract_interpreter(0,7,+,+,-).

:- multifile
    replace_goal_hook/3,
    replace_body_hook/3,
    evaluable_goal_hook/2.

:- dynamic
    evaluable_goal_hook/2.

:- discontiguous
    abstract_interpreter_body/6.

evaluable_goal_hook(absolute_file_name(A, _, O), _) :-
    ground(A),
    ground(O).
evaluable_goal_hook(memberchk(E, L), _) :-
    is_list(L),
    nonvar(E).
evaluable_goal_hook(member(_, L), _) :-
    is_list(L).
evaluable_goal_hook(option(O, L), _) :-
    is_list(L),
    nonvar(O).
evaluable_goal_hook(nth0(I, L, _), _) :-
    once(( is_list(L)
         ; nonvar(I)
         )).
evaluable_goal_hook(nth1(I, L, _), _) :-
    once(( is_list(L)
         ; nonvar(I)
         )).
evaluable_goal_hook(var(V),     _) :- nonvar(V).
evaluable_goal_hook(nonvar(V),  _) :- nonvar(V).
evaluable_goal_hook(atomic(A),  _) :- nonvar(A).
evaluable_goal_hook(atom(A),    _) :- nonvar(A).
evaluable_goal_hook(number(A),  _) :- nonvar(A).
evaluable_goal_hook(float(A),   _) :- nonvar(A).
evaluable_goal_hook(integer(A), _) :- nonvar(A).
evaluable_goal_hook(format(Out, Format, Args), _) :-
    nonvar(Out), nonvar(Format), ground(Args).
evaluable_goal_hook(_ is B, _) :- ground(B).
evaluable_goal_hook(atom_concat(A, B, C), _) :-
    once(( nonvar(A), nonvar(B)
         ; nonvar(A), nonvar(C)
         ; nonvar(B), nonvar(C)
         )).

replace_goal_hook(retractall(_), _, true).
replace_goal_hook(retract(_),    _, true).
replace_goal_hook(assertz(_),    _, true).
replace_goal_hook(asserta(_),    _, true).
replace_goal_hook(assert( _),    _, true).

mod_qual(M, G as R, I:H as B:C) :- !,
    strip_module(M:G, N, H),
    implementation_module(N:H, I),
    strip_module(M:R, A, C),
    implementation_module(A:C, B).
mod_qual(M, G, I:F/A) :-
    strip_module(M:G, N, F/A),
    functor(H, F, A),
    implementation_module(N:H, I).

abstract_interpreter(M:Goal, Abstraction, Options, Result) :-
    option(location(Loc),   Options, context(toplevel, Goal)),
    option(evaluable(Eval), Options, []),
    option(on_error(OnErr), Options, print_message(error)),
    ( is_list(Eval)->EvalL = Eval ; EvalL = [Eval]), % make it easy
    maplist(mod_qual(M), EvalL, MEvalL),
    abstract_interpreter(M:Goal, Abstraction,
                         state(Loc, MEvalL, M:OnErr, [], [], []), [], Result).

abstract_interpreter(MGoal, Abstraction, Options) :-
    abstract_interpreter(MGoal, Abstraction, Options, _).

:- meta_predicate catch(2, ?, ?, ?, ?).
catch(DCG, Ex, H, S0, S) :-
    catch(call(DCG, S0, S), Ex, H).

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
intercept(DCG, Ex, H, S0, S) :-
    intercept(call(DCG, S0, S), Ex, H).

cut_to(Goal) -->
    {prolog_current_choice(CP)},
    intercept(Goal, cut_from, catch(safe_prolog_cut_to(CP), _, true)).

cut_from :- send_signal(cut_from).
*/

abstract_interpreter_body(Goal, M, _, _) -->
    {var(Goal) ; var(M)}, bottom, !.
abstract_interpreter_body(M:Goal, _, Abs, State) --> !,
    abstract_interpreter_body(Goal, M, Abs, State).
abstract_interpreter_body(call(Goal), M, Abs, State) --> !,
    cut_to(abstract_interpreter_body(Goal, M, Abs, State)).
abstract_interpreter_body(\+ A, M, Abs, State) --> !,
    abstract_interpret_body_not(A, M, Abs, State).

abstract_interpret_body_not(A, M, Abs, State) -->
    ( cut_to(abstract_interpreter_body(A, M, Abs, State))
    ->( \+ is_bottom
      ->!,
        {fail}
      ; {fail}
      )
    ; !
    ).
abstract_interpret_body_not(_, _, _, _) --> bottom.

add_cont(Cont,
         state(Loc, EvalL, OnErr, CallL, Data, ContL),
         state(Loc, EvalL, OnErr, CallL, Data, [Cont|ContL])).

abstract_interpreter_body(once(Goal), M, Abs, State, S0, S) :- !,
    once(abstract_interpreter_body(Goal, M, Abs, State, S0, S)).
abstract_interpreter_body(distinct(Goal), M, Abs, State, S0, S) :-
    implementation_module(M:distinct(_), solution_sequences), !,
    distinct(Goal, abstract_interpreter_body(Goal, M, Abs, State, S0, S)).
abstract_interpreter_body(distinct(Witness, Goal), M, Abs, State, S0, S) :-
    implementation_module(M:distinct(_, _), solution_sequences), !,
    distinct(Witness, abstract_interpreter_body(Goal, M, Abs, State, S0, S)).
abstract_interpreter_body(setup_call_cleanup(S, C, E), M, Abs, State, S0, S) :- !,
    setup_call_cleanup(abstract_interpreter_body(S, M, Abs, State, S0, S1),
                       abstract_interpreter_body(C, M, Abs, State, S1, S2),
                       abstract_interpreter_body(E, M, Abs, State, S2, S)).
abstract_interpreter_body(call_cleanup(C, E), M, Abs, State, S0, S) :- !,
    call_cleanup(abstract_interpreter_body(C, M, Abs, State, S0, S1),
                 abstract_interpreter_body(E, M, Abs, State, S1, S)).
abstract_interpreter_body((A, B), M, Abs, State) --> !,
    { \+ terms_share(A, B)
    ->CutOnFail = true
    ; CutOnFail = fail
    },
    {add_cont(B, State, State2)},
    abstract_interpreter_body(A, M, Abs, State2),
    ( abstract_interpreter_body(B, M, Abs, State)
    *->[]
    ; { CutOnFail = true
      ->!, fail                 % The whole body will fail
      }
    ).

abstract_interpreter_body((A->B;C), M, Abs, State) --> !,
    {SCE = s(no)},
    ( interpret_local_cut(A, B, M, Abs, State, CutElse),
      {nb_setarg(1, SCE, CutElse)}
    ; ( {SCE = s(no)}
      ->abstract_interpreter_body(C, M, Abs, State)
      )
    ).
abstract_interpreter_body((A;B), M, Abs, State) --> !,
    ( abstract_interpreter_body(A, M, Abs, State)
    ; abstract_interpreter_body(B, M, Abs, State)
    ).
abstract_interpreter_body(A->B, M, Abs, State) --> !,
    interpret_local_cut(A, B, M, Abs, State, _).
abstract_interpreter_body(CallN, M, Abs, State) -->
    {do_resolve_calln(CallN, Goal)}, !,
    cut_to(abstract_interpreter_body(Goal, M, Abs, State)).

push_top(Prev, Prev, []).

pop_top(bottom, _, bottom).
pop_top([], Curr, Curr).

% CutElse make the failure explicit wrt. B
interpret_local_cut(A, B, M, Abs, State, CutElse) -->
    { \+ terms_share(A, B)
    ->CutOnFail = true
    ; CutOnFail = fail
    },
    {add_cont(B, State, State2)},
    push_top(Prev),
    cut_to(abstract_interpreter_body(A, M, Abs, State2)), % loose of precision
    ( \+ is_bottom
    ->!,
      { CutElse = yes }
    ; { CutElse = no  }
    ),
    pop_top(Prev),
    ( abstract_interpreter_body(B, M, Abs, State)
    *->
      []
    ; ( {CutOnFail = true}
      ->cut_if_no_bottom
      ; []
      )
    ).
abstract_interpreter_body(!,    _, _, _) --> !, cut_if_no_bottom.
abstract_interpreter_body(A=B,  _, _, _) --> !, {A=B}.
abstract_interpreter_body(A\=B, _, _, _) --> !, ( \+ is_bottom -> {A\=B} ; {A\==B} ).
abstract_interpreter_body(true, _, _, _) --> !.
abstract_interpreter_body(fail, _, _, _) --> !, {fail}.
abstract_interpreter_body(H, M, Abs, State) -->
    cut_to(abstract_interpreter_lit(H, M, Abs, State)).

terms_share(A, B) :-
    term_variables(A, VarsA),
    VarsA \= [], % Optimization
    term_variables(B, VarsB),
    ( member(VA, VarsA),
      member(VB, VarsB),
      VA==VB
    ), !.

is_bottom(bottom, bottom).

cut_if_no_bottom -->
    ( \+ is_bottom
    ->{cut_from}
    ; []
    ).

abstract_interpreter(MH, Abs, State) -->
    {strip_module(MH, M, H)},
    abstract_interpreter_lit(H, M, Abs, State).

abstract_interpreter_lit(H, M, Abs, State0 ) -->
    { predicate_property(M:H, meta_predicate(Meta))
    ->qualify_meta_goal(M:H, Meta, Goal)
    ; Goal = H
    },
    { State0 = state(Loc, EvalL, OnError, CallL, Data, Cont),
      implementation_module(M:Goal, IM)
    },
    ( {member(MCall, CallL),
       MCall =@= IM:Goal
      }
    ->bottom
    ; { copy_term(IM:Goal, MCall),
        State1 = state(Loc, EvalL, OnError, [MCall|CallL], Data, Cont)
      },
      ( { ( evaluable_goal_hook(Goal, IM)
          ; functor(Goal, F, A),
            memberchk(IM:F/A, EvalL)
          ),
          MRepl = M:Goal
        ; ( replace_goal_hook(Goal, IM, Repl)
          ; memberchk((IM:Goal as Repl), EvalL)
          ),
          MRepl = M:Repl
        }
      ->{call(MRepl)}
      ; { replace_body_hook(Goal, IM, Body)
        ; copy_term(EvalL, EvalC), % avoid undesirable unifications
          memberchk((IM:Goal :- Body), EvalC)
        }
      ->cut_to(abstract_interpreter_body(Body, M, Abs, State1))
      ; { \+ predicate_property(M:Goal, defined) }
      ->{ call(OnError, error(existence_error(procedure, M:Goal), Loc)),
                                % TBD: information to error
          fail
        }
      ; call(Abs, Goal, M, CM:Body, State1, State),
        cut_to(abstract_interpreter_body(Body, CM, Abs, State))
      )
    ).

% top: empty set
% bottom: I don't know, universe set.
% true: exact result

bottom(_, bottom).

:- multifile match_ai/8.

match_ai(head,    G, M, Body, S0, S) --> match_head(   G, M, Body, S0, S).
match_ai(noloops, G, M, Body, S0, S) --> match_noloops(G, M, Body, S0, S).

match_head(Goal, M, M:true, state(_, EvalL, OnErr, CallL, D, Cont), S) -->
    {predicate_property(M:Goal, interpreted)}, !,
    { match_head_body(Goal, M, Body, Loc)
    *->S = state(Loc, EvalL, OnErr, CallL, D, Cont)
    ; fail
    },
    ( {Body = _:true}
    ->[]
    ; bottom %% loose of precision
    ).
match_head(_,    M, M:true, S, S) --> bottom.

match_head_body(Goal, M, CMBody, From) :-
    ( extra_clauses(Goal, M, CMBody, From)
    ; From = clause(Ref),
      clause(M:Goal, Body, Ref),
      clause_property(Ref, module(CM)),
      CMBody = CM:Body
    ).

:- use_module(library(interface), []).

:- multifile extra_clauses/4.

extra_clauses(Goal, CM, I:Goal, _From) :-
    implementation_module(CM:Goal, M),
    functor(Goal, F, A),
    ( interface:'$interface'(M, DIL, IIL),
      ( memberchk(F/A, DIL)
      ; memberchk(F/A, IIL)
      )
    ->interface:'$implementation'(I, M)
    ).

match_noloops(Goal, M, Body, state(Loc0, EvalL, OnErr, CallL, S, Cont),
              state(Loc, EvalL, OnErr, CallL, [M:F/A-Size|S], Cont)) -->
    {predicate_property(M:Goal, interpreted)}, !,
    ( { functor(Goal, F, A),
        term_size(Goal, Size),
        \+ ( memberchk(M:F/A-Size1, S),
             Size1=<Size
           )
      }
    ->{ match_head_body(Goal, M, Body, Loc) },
      []
    ; { Loc = Loc0 },
      bottom %% loose of precision
    ).
match_noloops(_,    M, M:true, S, S) --> bottom.
