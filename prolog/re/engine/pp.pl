:- module(re_engine_pp, [engine_match/5]).
:- use_module(library(re/options), [adjust_case/3]).

% regular expression interpreter

% engine_match(RE, Selected, S, Unmatched) is true if RE matches
% a string Prefix such that S = [Prefix|Unmatched], and
% Selected is the list of substrings of Prefix that matched
% the parenthesized components of RE.

engine_match(anywhere(RE), Opt, Selected) -->
    engine_match(RE, Opt, Selected).
engine_match(anywhere(RE), Opt, Selected) -->
    [_],
    engine_match(anywhere(RE), Opt, Selected).

engine_match(union(RE1, _RE2), Opt, Selected) -->
    engine_match(RE1, Opt, Selected).
engine_match(union(_RE1, RE2), Opt, Selected) -->
    engine_match(RE2, Opt, Selected).

engine_match(conc(RE1, RE2), Opt, Selected) -->
    engine_match(RE1, Opt, Sel1),
    engine_match(RE2, Opt, Sel2),
    { append(Sel1, Sel2, Selected) }.

% match a specific number of times
engine_match(count(RE,N0,M0), Opt, Selected) -->
    { N0 > 0 },
    engine_match(RE, Opt, Sel1),    % try for minimum matches
    { succ(N, N0) },
    { succ(M, M0) },
    engine_match(count(RE,N,M), Opt, Sel2),
    { append(Sel1, Sel2, Selected) }.
engine_match(count(RE,0,M0), Opt, Selected) -->
    { M0 > 0 },
    engine_match(RE, Opt, Sel1),    % prefer more matches
    { succ(M, M0) },
    engine_match(count(RE,0,M), Opt, Sel2),
    { append(Sel1, Sel2, Selected) }.
engine_match(count(_,0,_), _Opt, []) -->
    { true }.

% Match a group and add it to the end of
% list of selected items from the submatch.
engine_match(group(RE), Opt, Selected, S, U) :-
    engine_match(RE, Opt, Sel1, S, U),
    append(P, U, S),
    append(Sel1, [P], Selected).

engine_match(any, _Opt, []) -->
    [_].

% matches both regular characters and metacharacters
engine_match(char(C), Opt, []) -->
    [C0],
    { adjust_case(Opt, C0, C) }.

engine_match(eos, _Opt, [], [], []).

engine_match(neg_set(Set), _Opt, []) -->
    [C],
    { \+ char_set_member(C, Set) }.

engine_match(pos_set(Set), _Opt, []) -->
    [C],
    { char_set_member(C, Set) }.


char_set_member(C, [char(C) | _]).
char_set_member(C, [range(C1, C2) | _]) :-
    C1 =< C,
    C =< C2.
char_set_member(C, [_|T]) :-
    char_set_member(C, T).
