:- module(re_engine_pp, [rematch1/4]).

% regular expression interpreter

% rematch1(RE, S, Unmatched, Selected) is true if RE matches
% a string Prefix such that S = [Prefix|Unmatched], and
% Selected is the list of substrings of Prefix that matched
% the parenthesized components of RE.

rematch1(union(RE1, _RE2), S, U, Selected) :-
    rematch1(RE1, S, U, Selected).
rematch1(union(_RE1, RE2), S, U, Selected) :-
    rematch1(RE2, S, U, Selected).

rematch1(conc(RE1, RE2), S, U, Selected) :-
    rematch1(RE1, S, U1, Sel1),
    rematch1(RE2, U1, U, Sel2),
    append(Sel1, Sel2, Selected).

rematch1(star(RE), S, U, Selected) :-
    % Try longest match first.
    rematch1(RE, S, U1, Sel1),
    rematch1(star(RE), U1, U, Sel2),
    append(Sel1, Sel2, Selected).
rematch1(star(_RE), S, S, []).

rematch1(plus(RE), S, U, Selected) :-
    rematch1(RE, S, U1, Sel1),
    rematch1(star(RE), U1, U, Sel2),
    append(Sel1, Sel2, Selected).

rematch1(optional(RE), S, U, Selected) :-
    rematch1(RE, S, U, Selected).
rematch1(optional(_), S, S, []).

% Match a group and add it to the end of
% list of selected items from the submatch.
rematch1(group(RE), S, U, Selected) :-
    rematch1(RE, S, U, Sel1),
    append(P, U, S),
    append(Sel1, [P], Selected).

rematch1(any, [_C1|U], U, []).

% matches both regular characters and metacharacters
rematch1(char(C), [C|U], U, []).

rematch1(eos, [], [], []).

rematch1(neg_set(Set), [C|U], U, []) :-
    \+ char_set_member(C, Set).

rematch1(pos_set(Set), [C|U], U, []) :-
    char_set_member(C, Set).


char_set_member(C, [char(C) | _]).
char_set_member(C, [range(C1, C2) | _]) :-
    C1 =< C,
    C =< C2.
char_set_member(C, [_|T]) :-
    char_set_member(C, T).
