:- module(regex_engine_pp, [engine_match/5]).
:- use_module(library(regex/state)).

% regular expression interpreter

% engine_match(RE, Selected, S, Unmatched) is true if RE matches
% a string Prefix such that S = [Prefix|Unmatched], and
% Selected is the list of substrings of Prefix that matched
% the parenthesized components of RE.

engine_match(union(RE1, _RE2), State0, State) -->
    engine_match(RE1, State0, State).
engine_match(union(_RE1, RE2), State0, State) -->
    engine_match(RE2, State0, State).

engine_match(conc(RE1, RE2), State0, State) -->
    engine_match(RE1, State0, State1),
    engine_match(RE2, State1, State).

% match a specific number of times
engine_match(count(RE,N0,M0), State0, State) -->
    { N0 > 0 },
    engine_match(RE, State0, State1),    % try for minimum matches
    { succ(N, N0) },
    { succ(M, M0) },
    engine_match(count(RE,N,M), State1, State).
engine_match(count(RE,0,M0), State0, State) -->
    { M0 > 0 },
    engine_match(RE, State0, State1),    % prefer more matches
    { succ(M, M0) },
    engine_match(count(RE,0,M), State1, State).
engine_match(count(_,0,_), State, State) -->
    { true }.

% Match a capturing group
engine_match(group(RE), State0, State, S, U) :-
    push_capture(P, State0, State1),
    engine_match(RE, State1, State, S, U),
    append(P, U, S).

% Match a named group. Try saving the capture under a name; otherwise,
% treat the group as a numbered capture.
engine_match(named_group(Name,RE), State0, State, S, U) :-
    push_capture(Name=P,State0,State1),
    engine_match(RE, State1, State, S, U),
    append(P, U, S).

engine_match(any, State, State) -->
    [C],
    {
        ( C = 0'\n ->
            singleline_mode(State)
        ; % not a new line ->
            true
        )
    }.

% matches both regular characters and metacharacters
engine_match(char(C), State, State) -->
    [C0],
    { adjust_case(State, C0, C) }.

engine_match(eos, State, State, [], []).

engine_match(neg_set(Set), State, State) -->
    [C0],
    { adjust_case(State,C0,C) },
    { \+ char_set_member(C, Set) }.

engine_match(pos_set(Set), State, State) -->
    [C0],
    { adjust_case(State,C0,C) },
    { char_set_member(C, Set) }.


char_set_member(C, [char(C) | _]).
char_set_member(C, [range(C1, C2) | _]) :-
    C1 =< C,
    C =< C2.
char_set_member(C, [_|T]) :-
    char_set_member(C, T).
