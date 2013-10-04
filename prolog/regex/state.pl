:- module(regex_state, [ adjust_case/3
                       , new_state/3
                       , push_capture/3
                       , singleline_mode/1
                       , numbered_captures/2
                       ]).
:- use_module(library(apply), [ foldl/4 ]).
:- use_module(library(assoc)).
:- use_module(library(record)).

:- record state(i='-', s='-', capture_count=0, captures).

%% new_state(+OptionSugar, +CaptureSugar, -State) is semidet
%
%  True if State is an opaque value representing the regular expression
%  state described by OptionSugar and CapturesSugar. OptionSugar
%  should be a list or an atom. If it's an atom it should
%  be something like 'ims', 'xi', etc. Fails if OptionSugar contains an
%  unknown option.
new_state(OptionSugar, CaptureSugar, State) :-
    atom(OptionSugar),
    OptionSugar \== [],  % empty list atom needs no expansion
    !,
    atom_chars(OptionSugar, Chars),
    new_state(Chars, CaptureSugar, State).
new_state(OptionList, CaptureSugar, State) :-
    var(CaptureSugar),
    !,
    new_state(OptionList, [], State).
new_state(OptionList, CaptureSugar, State) :-
    default_state(State0),

    % set all options
    foldl(set_option, OptionList, State0, State1),

    % prepare capture values
    empty_assoc(Captures),
    set_captures_of_state(Captures, State1, State2),
    foldl(push_pattern, CaptureSugar, State2, State3),

    % next round of captures resumes numbering at the beginning
    set_capture_count_of_state(0, State3, State).


%% set_option(+Option, +State0, -State) is semidet
%
%  Sets the option Option, giving a new State value.
set_option(i) -->
    set_i_of_state('+').
set_option(s) -->
    set_s_of_state('+').


%% push_pattern(+Capture, +State0, -State) is semidet
%
%  Adds Capture to State0 giving a new State. Capture may be
%  `Name=Value` or just `Value`
push_pattern(Capture, State0, State) :-
    var(Capture),
    !,
    push_numbered(Capture, State0, State).
push_pattern(Capture, State0, State) :-
    Capture = (Name=Value),
    !,
    ground(Name),
    push_named(Name, Value, State0, State).
push_pattern(Value, State0, State) :-
    % \+ var(Value)
    % \+ Value=(Name=_)
    push_numbered(Value, State0, State).


%% push_capture(+Capture, +State0, -State) is semidet
%
%  Adds Capture to State0 giving a new State. Capture may be
%  `Name=Value` or just `Value`. Pushing a named capture pushes both a
%  named and a numbered capture.
push_capture(Capture, State0, State) :-
    var(Capture),
    !,
    push_numbered(Capture, State0, State).
push_capture(Capture, State0, State) :-
    Capture = (Name=Value),
    !,
    ground(Name),
    push_named(Name, Value, State0, State1),
    push_numbered(Value, State1, State).
push_capture(Value, State0, State) :-
    % \+ var(Value)
    % \+ Value=(Name=_)
    push_numbered(Value, State0, State).


%% push_numbered(+Value, +State0, -State) is semidet.
%
%  Add Value to State0 as a numbered capture producing a new State.
push_numbered(Value, State0, State) :-
    % retrieve current values
    state_capture_count(State0, Count0),
    state_captures(State0, Captures0),

    % create new values
    insert_pair(Count0, Value, Captures0, Captures1),
    succ(Count0, Count),

    % bundle them into a new state value
    set_capture_count_of_state(Count, State0, State1),
    set_captures_of_state(Captures1, State1, State).


%% push_named(+Name, +Value, +State0, -State) is semidet
%
%  Add Name and Value pair to State0 producing a new State.
push_named(Name, Value, State0, State) :-
    % retrieve current values
    state_captures(State0, Captures0),

    % create new value
    insert_pair(Name, Value, Captures0, Captures),

    % bundle into a new state value
    set_captures_of_state(Captures, State0, State).


% unify a value with a named value; create a named value if the name
% doesn't yet exist
insert_pair(Name, Value, Assoc, Assoc) :-
    get_assoc(Name, Assoc, CurrentValue),
    !,
    Value = CurrentValue.
insert_pair(Name, Value, Assoc0, Assoc) :-
    put_assoc(Name, Assoc0, Value, Assoc).


%% numbered_captures(+State, -Captures:list) is det.
%
%  True if Captures is a list of numbered captures in State.
numbered_captures(State, List) :-
    state_capture_count(State, N),
    state_captures(State, Captures),
    numbered_captures(N, Captures, List).
numbered_captures(0, _, []) :-
    !.
numbered_captures(N0, Captures, [Value|Tail]) :-
    succ(N, N0),
    get_assoc(N,Captures,Value),
    numbered_captures(N,Captures,Tail).


%% adjust_case(+Options, +Code0, -Code) is det.
%
%  True if Code represents the same letter as Code0 but with case
%  adjusted to compensate for the 'i' regular expression option (aka
%  case insensitive).
adjust_case(Options, Code0, Code) :-
    ( state_i(Options, '+') ->
          code_type(Code, to_lower(Code0))
    ; % otherwise ->
          Code = Code0
    ).

%%	singleline_mode(+Options) is semidet.
%
%	True if Options request single-line mode (`/s`).
singleline_mode(Options) :-
    state_s(Options, '+').
