:- use_module(library(regex)).
:- use_module(library(when), [when/2]).

:- set_prolog_flag(double_quotes, codes).

% relates an even number to its codes representation
% (used for when/2 tests below)
codes_even(Codes, Even) :-
    number_codes(Even, Codes),
    0 =:= Even mod 2.

:- use_module(library(tap)).

% no captures
regex('\\d+', [], '932', []).
regex('.*', [], howdy, []).

'single capture (entire)' :-
    regex('(\\d+)', [], '932', ["932"]).
'single capture (partial)' :-
    regex('a (\\d+)rd', [], 'a 3rd', ["3"]).

'two captures' :-
    regex('(\\S+) (\\S+)', [], 'hello world', ["hello", "world"]).


'one, explicit named capture' :-
    regex('Hi (?<Name>\\w+)', i, 'Hi Ed', ['Name'=Name]),
    Name == "Ed".

'two, explicit named captures' :-
    regex('(?<A>\\d) (?<B>\\d)', [], 'a 1 2 b', ['B'=B,'A'=A]),
    A == "1",
    B == "2".

'two named captures, only one is used' :-
    regex('(?<A>\\d) (?<B>\\d)', [], 'a 1 2 b', ['A'=A]),
    A == "1".

'one named capture, two names given' :-
    regex('(?<A>a)bcd', [], 'abcd', ['B'=B,'A'=A]),
    A == "a",
    var(B).

'numbered, constrained captures' :-
    when(ground(A),(number_codes(N,A), 1 =:= N mod 2)),
    regex("odd: ([0-9]+)", i, "Odd: 77", [A]).

'named, constrained captures' :-
    when(ground(A),(number_codes(N,A), 0 =:= N mod 2)),
    regex("even: (?<A>[0-9]+)", [], "even: 42", ['A'=A]).

'named capture treated as a numbered capture' :-
    regex('(?<A>a)bcd', [], 'abcd', Captures),
    Captures == ["a"].

'all captures initially unbound' :-
    regex('hello (\\w+)', i, 'Hello Sue', Captures),
    Captures == ["Sue"].

'single capture initially unbound' :-
    regex('hello (\\w+)', i, 'Hello Joe', [Whom]),
    Whom == "Joe".

'pattern matches but captures fail unification'(fail) :-
    Whom = "Thomas",
    regex('hello ([a-z]+)', i, 'Hello Tom', [Whom]).


'implicit named captures: one' :-
    N = _, % avoid singleton warning
    "num: 42" =~ "num: (?<N>\\d+)$",
    N == "42".

'implicit named captures: two' :-
    X = _, Y = _, % avoid singleton warnings
    "hi:hola" =~ "(?<X>\\w+):(?<Y>\\w+)",
    Y == "hola",
    X == "hi".

'implicit named captures: extra in-scope variables' :-
    X = X,  % an extra, in-scope variable
    Name = _, % avoid singleton warnings
    "Hi John" =~ "hi (?<Name>[a-z]+)"/i,
    Name == "John".

'implicit named captures: contraints before match OK' :-
    when( ground(X), codes_even(X,N) ),
    "even: 42" =~ "even: (?<X>[0-9]+)$",
    N =:= 42.

'implicit named captures: contraints before match FAIL'(fail) :-
    when( ground(X), codes_even(X,_) ),
    "even: 43" =~ "even: (?<X>[0-9]+)$".
