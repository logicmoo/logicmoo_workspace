:- module(re_engine_pp, [engine_match/4]).

% regular expression interpreter

% engine_match(RE, Selected, S, Unmatched) is true if RE matches
% a string Prefix such that S = [Prefix|Unmatched], and
% Selected is the list of substrings of Prefix that matched
% the parenthesized components of RE.

engine_match(union(RE1, _RE2), Selected) -->
    engine_match(RE1, Selected).
engine_match(union(_RE1, RE2), Selected) -->
    engine_match(RE2, Selected).

engine_match(conc(RE1, RE2), Selected) -->
    engine_match(RE1, Sel1),
    engine_match(RE2, Sel2),
    { append(Sel1, Sel2, Selected) }.

engine_match(star(RE), Selected) -->
    % Try longest match first.
    engine_match(RE, Sel1),
    engine_match(star(RE), Sel2),
    { append(Sel1, Sel2, Selected) }.
engine_match(star(_RE), []) -->
    { true }.

engine_match(plus(RE), Selected) -->
    engine_match(RE, Sel1),
    engine_match(star(RE), Sel2),
    { append(Sel1, Sel2, Selected) }.

engine_match(optional(RE), Selected) -->
    engine_match(RE, Selected).
engine_match(optional(_), []) -->
    [].

% match a specific number of times
engine_match(count(RE,N0,M0), Selected) -->
    { N0 > 0 },
    engine_match(RE, Sel1),    % try for minimum matches
    { succ(N, N0) },
    { succ(M, M0) },
    engine_match(count(RE,N,M), Sel2),
    { append(Sel1, Sel2, Selected) }.
engine_match(count(RE,0,M0), Selected) -->
    { M0 > 0 },
    engine_match(RE, Sel1),    % prefer more matches
    { succ(M, M0) },
    engine_match(count(RE,0,M), Sel2),
    { append(Sel1, Sel2, Selected) }.
engine_match(count(_,0,_), []) -->
    { true }.

% Match a group and add it to the end of
% list of selected items from the submatch.
engine_match(group(RE), Selected, S, U) :-
    engine_match(RE, Sel1, S, U),
    append(P, U, S),
    append(Sel1, [P], Selected).

engine_match(any, []) -->
    [_].

% matches both regular characters and metacharacters
engine_match(char(C), []) -->
    [C].

engine_match(eos, [], [], []).

engine_match(neg_set(Set), []) -->
    [C],
    { \+ char_set_member(C, Set) }.

engine_match(pos_set(Set), []) -->
    [C],
    { char_set_member(C, Set) }.


char_set_member(C, [char(C) | _]).
char_set_member(C, [range(C1, C2) | _]) :-
    C1 =< C,
    C =< C2.
char_set_member(C, [_|T]) :-
    char_set_member(C, T).
