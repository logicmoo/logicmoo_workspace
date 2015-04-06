:- module(abstract_interpreter, [abstract_interpreter/2,
				 abstract_interpreter/4,
				 match_head/7,
				 match_ai/8,
				 match_noloops/7]).

:- use_module(library(extra_location)).
:- use_module(library(term_size)).
:- use_module(library(implementation_module)).
:- use_module(library(qualify_meta_goal)).
:- use_module(library(solution_sequences)).
% :- use_module(library(tabling)).

:- dynamic inferred/6.

:- meta_predicate abstract_interpreter(+,+,7,-).
abstract_interpreter(Goal, M, Abstraction, data(0, [], Result)) :-
    ( catch(abstract_interpreter(Goal, M, Abstraction, [], [], Result),
	    fail_branch,
	    fail)
    *->true
    ; Result = fail
    ).

:- meta_predicate abstract_interpreter(?,7).
abstract_interpreter(M:Goal, Abstraction) :-
    abstract_interpreter(Goal, M, Abstraction, [], [], _).

abstract_interpreter_body(Goal, M, Abs, State, R0, R) :-
    distinct_result(abstract_interpreter_body_(Goal, M, Abs, State, R0, R)).

:- meta_predicate distinct_result(0).
distinct_result(Goal) :-
    distinct(H, ((Goal),variant_sha1(Goal,H))).

abstract_interpreter_body_(Goal, M, _, _) -->
    {var(Goal) ; var(M)}, bottom, !.
abstract_interpreter_body_(M:Goal, _, Abs, State) --> !,
    abstract_interpreter_body(Goal, M, Abs, State).
abstract_interpreter_body_(call(Goal), M, Abs, State) --> !,
    abstract_interpreter_body(Goal, M, Abs, State).
abstract_interpreter_body_(\+ A, M, Abs, State) --> !,
    ( abstract_interpreter_body(A, M, Abs, State)
    ->bottom %% We can not say that is always true
    ; []
    ).
abstract_interpreter_body_((A, B), M, Abs, State) --> !,
    { \+ terms_share(A, B)
    ->CutOnFail = true
    ; CutOnFail = fail
    },
    abstract_interpreter_body(A, M, Abs, State),
    ( abstract_interpreter_body(B, M, Abs, State)
    *->[]
    ; { CutOnFail == true
      ->!, fail			% The whole body will fail
      }
    ).
abstract_interpreter_body_((A;B), M, Abs, State) --> !,
    ( catch(abstract_interpreter_body(A, M, Abs, State), fail_branch, fail)
    ; abstract_interpreter_body(B, M, Abs, State)
    ).
abstract_interpreter_body_(A->B, M, Abs, State) --> !,
    {prolog_current_choice(CP)},
    { \+ terms_share(A, B)
    ->CutOnFail = true
    ; CutOnFail = fail
    },
    abstract_interpreter_body(A, M, Abs, State),
    cut_if_no_bottom(CP),	% loose of precision
    ( catch(abstract_interpreter_body(B, M, Abs, State),
	    fail_branch,
	    CutOnFail = true)
    *->[]
    ; { CutOnFail == true
      ->!, fail
      }
    ).
abstract_interpreter_body_(H, M, Abs, State) -->
    abstract_interpreter(H, M, Abs, State).

terms_share(A, B) :-
    term_variables(A, VarsA),
    term_variables(B, VarsB),
    ( member(VA, VarsA),
      member(VB, VarsB),
      VA==VB
    ), !.

cut_if_no_bottom(_, bottom, bottom) :- !.
cut_if_no_bottom(CP) --> {prolog_cut_to(CP)}.

abstract_interpreter(H, M, Abs, State, R0, R) :-
    distinct_result(abstract_interpreter_(H, M, Abs, State, R0, R)).

:- meta_predicate catch(2, ?, ?, ?, ?).
catch(DCG, Ex, H, S0, S) :-
    catch(call(DCG, S0, S), Ex, H).

abstract_interpreter_(H, M, Abs, State0 ) --> 
    { predicate_property(M:H, meta_predicate(Meta))
    ->qualify_meta_goal(M:H, Meta, Goal)
    ; Goal = H
    },
    call(Abs, Goal, M, Body, State0, State),
    ( {Body = true}
    ->[]
    ; {get_context_body(Goal, M, CM)},
      catch(abstract_interpreter_body(Body, CM, Abs, State), fail_branch, fail)
    ).

get_context_body(Goal, M, CM) :-
      ( predicate_property(M:Goal, transparent)
      ->CM = M
      ; predicate_property(M:Goal, imported_from(IM))
      ->CM = IM
      ; CM = M
      ).

% top: empty set
% bottom: I don't know, universe set.
% true: exact result

bottom(_, bottom).

:- multifile match_ai/8.

match_ai(head,    G, M, Body, S0, S) --> match_head(   G, M, Body, S0, S).
match_ai(noloops, G, M, Body, S0, S) --> match_noloops(G, M, Body, S0, S).

match_head(Goal, M, true, _, _) -->
    {predicate_property(M:Goal, interpreted)}, !,
    { match_head_body(Goal, M, Body)
    *->true
    ; throw(fail_branch)
    },
    ( {Body = true}
    ->[]
    ; bottom %% loose of precision
    ).
match_head(A=B,  _, true, _, _) --> !,
    { A=B
    ->true
    ; throw(fail_branch)
    }.
match_head(fail, _, _,    _, _) --> !, {throw(fail_branch)}.
match_head(true, _, true, _, _) --> !, [].
match_head(!,    _, true, _, _) --> !, [].
match_head(_,    _, true, _, _) --> bottom.

match_head_body(Goal, M, Body) :-
    ( extra_clauses(Goal, M, Body)
    ; clause(M:Goal, Body)
    ).

:- use_module(library(interface), []).

:- multifile extra_clauses/3.

extra_clauses(Goal, CM, true) :-
    predicate_property(M:Goal, dynamic),
    implementation_module(CM:Goal, M),
    loc_dynamic(Goal, M, dynamic(def, _, _), _).
extra_clauses(Goal, CM, I:Goal) :-
    implementation_module(CM:Goal, M),
    functor(Goal, F, A),
    ( interface:'$interface'(M, DIL, IIL),
      ( memberchk(F/A, DIL)
      ; memberchk(F/A, IIL)
      )
    ->interface:'$implementation'(I, M)
    ).

match_noloops(Goal, M, Body, S, [M:F/A-Size|S]) -->
    {predicate_property(M:Goal, interpreted)}, !,
    ( { functor(Goal, F, A),
	term_size(Goal, Size),
	\+ ( memberchk(M:F/A-Size1, S),
	     Size1=<Size
	   )
      }
    ->{ match_head_body(Goal, M, Body) },
      []
    ; bottom %% loose of precision
    ).
match_noloops(fail, _, _,    _, _) --> !, {throw(fail_branch)}.
match_noloops(true, _, true, S, S) --> !, [].
match_noloops(!,    _, true, S, S) --> !, [].
match_noloops(_,    _, true, S, S) --> bottom.
