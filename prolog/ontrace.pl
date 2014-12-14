:- module(ontrace, [ontrace/3]).

:- use_module(library(maplist_dcg)).
:- use_module(library(prolog_clause), []).
:- use_module(library(prolog_codewalk), []).

:- meta_predicate ontrace(0,5,+).

ontrace(Goal, OnTrace, OptionL) :-
    State=state(_, _, _),	% Allow destructive assignment
    call_inoutex(Goal,
	setup_trace(State, OnTrace, OptionL),
	cleanup_trace(State)).

:- meta_predicate call_inoutex(0,0,0).
call_inoutex(Goal, OnIn, OnOut) :-
    catch(call_inout(Goal, OnIn, OnOut),
	  E,  (OnOut, throw(E))).

call_inout(Goal, OnIn, OnOut) :-
    (OnIn;OnOut,fail),
    prolog_current_choice(C0),
    Goal,
    prolog_current_choice(C1),
    (OnOut;OnIn,fail),
    (C0==C1 -> ! ;true).

r_true(_).

%% setup_trace(!State, :OnTrace, +OptL) is det.
setup_trace(State, M:OnTrace, OptL) :-
    select_option(goal(SkipGoal), OptL,  OptL1, ontrace:r_true),
    select_option(file(SkipFile), OptL1, _,     ontrace:r_true),
    asserta((user:prolog_trace_interception(Port, Frame, PC, continue)
	    :- ignore(\+trace_port(Port, Frame, PC, M:OnTrace, M:SkipGoal,
				   M:SkipFile))),
	    Ref),
    maplist_dcg(port_mask, [call, exit, fail, redo, unify, exception], 0, Mask),
    '$visible'(Visible, Mask),
    '$leash'(Leash, Mask),
    nb_setarg(1, State, Visible),
    nb_setarg(2, State, Leash),
    nb_setarg(3, State, Ref),
    trace.

%% cleanup_state(+State) is det.
cleanup_trace(state(Visible, Leash, Ref)) :-
    nodebug,
    '$visible'(_, Visible),
    '$leash'(_, Leash),
    erase(Ref),
    !.
cleanup_trace(State) :-
    print_message(error, format('Failed when saving tracer data', [State])),
    fail.

port_mask(Port, Mask0, Mask) :- '$syspreds':port_name(Port, Bit),
    Mask is Mask0\/Bit.

user_defined_module(M) :-
    M \= ontrace,
    module_property(M, class(user)).

:- public trace_port/6.
:- meta_predicate trace_port(+,+,+,5,1,1).
trace_port(Port, Frame, PC, OnTrace, SkipGoal, SkipFile) :-
    prolog_frame_attribute(Frame,  goal, M:H), % M:H to skip local predicates
    \+ \+ call(SkipGoal, M:H),
    find_parent_subloc(Port, Frame, M, SkipFile, ParentL, SubLoc),
    once(( member(F, [Frame|ParentL]),
    	   ( prolog_frame_attribute(F, goal, PM:_),
    	     user_defined_module(PM)
    	   ))),
    call(OnTrace, Port, Frame, PC, ParentL, SubLoc).

find_parent_subloc(Port, Frame, Module, SkipFile, ParentL, SubLoc) :-
    ( % Due to a bug in SWI-Prolog, we can not rely on redo(PC) +
      % $clause_term_position(Cl,PC,List), in any case, the coverage of builtins
      % is not big deal, compared to the coverage of custom predicates --EMM
      member(Port, [redo(_PC), unify /*, exit*/]) % TODO: if exit placed here,
                                                  % then it is marked in the
                                                  % clause, else in the literal
    ->ParentL = [],
      prolog_frame_attribute(Frame, clause, Cl),
      List = []
    ; find_parent_with_pc(Frame, PC, [], ParentL),
      [Parent|_] = ParentL,
      prolog_frame_attribute(Parent, clause, Cl),
      ('$clause_term_position'(Cl, PC, List)
      ->true
      ; List = []
      )
    ),
    clause_subloc(Module, SkipFile, Cl, List, SubLoc).

find_parent_with_pc(Frame, PC, List0, List) :-
    prolog_frame_attribute(Frame, parent, Parent),
    ( prolog_frame_attribute(Frame, pc, PC)
    ->List = [Parent|List0 ]
    ; find_parent_with_pc(Parent, PC, [Parent|List0 ], List)
    ).

%% clause_subloc(+Module, :SkipFile, +ClauseRef, +List, -SubLoc) is det.
%
% We need to use the TermPos as far as possible, that is why we call this method
% even setting List=[] in its caller, and should not be simplified earlier.
%
clause_subloc(Module, SkipFile, Cl, List, SubLoc) :-
    ( clause_property(Cl, file(File)),
      clause_property(Cl, line_count(Line))
    -> \+ \+ call(SkipFile, File),
      ( prolog_clause:read_term_at_line(File, Line, Module, Term, TermPos, _)
      ->( ( prolog_clause:ci_expand(Term, ClauseL, Module, TermPos, ClausePos),
	    match_clause(Cl, ClauseL, Module, List2, List),
	    nonvar(ClausePos)
	  ->( maplist_dcg(find_subgoal, List2, ClausePos, SubPos),
	      nonvar(SubPos)
	    ->SubLoc = file_term_position(File, SubPos)
	    ; SubLoc = file_term_position(File, ClausePos)
	    )
	  ; SubLoc = file_term_position(File, TermPos)
	  )
	)
      ; SubLoc = file(File, Line, -1, _)
      )
    ; SubLoc = clause(Cl)
    ).

list_pos(term_position(_, _, _, _, PosL), PosL).
list_pos(list_position(_, _, PosL, _), PosL).

find_subgoal(A, TermPos, Pos) :-
    list_pos(TermPos, PosL),
    is_list(PosL),
    nth1(A, PosL, Pos),
    nonvar(Pos), !.

match_clause(Ref, ClauseL, Module, List, Tail) :-
    % format(user_error, '~N~w',[match_clause(Ref, ClauseL, Module, List, Tail)]),
    ( is_list(ClauseL),
      clause(Head, Body, Ref),
      nth1(Pos, ClauseL, Clause),
      % format(user_error, '~N~w',[normalize_cl(Clause, Module, Module, NClause)]),
      normalize_cl(Clause, Module, Module, NClause),
      NClause =@= (Head :- Body)
    ->List = [Pos|Tail]
    ; List = Tail
    ).

normalize_cl(M:Clause, _, CM, NClause) :- !,
    normalize_cl(Clause, M, CM, NClause).
normalize_cl((Head :- Body), M, CM, (MHead :- NBody)) :- !,
    strip_mod(Head, M, MHead),
    strip_mod(Body, CM, MBody),
    ( MBody = M:Body
    ->NBody = Body
    ; NBody = MBody
    ).
normalize_cl(Head, M, CM, NClause) :-
    normalize_cl((Head :- true), M, CM, NClause).

strip_mod(M:Term, _, MTerm) :-
    strip_mod(Term, M, MTerm).
strip_mod(Term, M, M:Term).
