:- module(re_parser, [re//1]).
:- use_module(library(dcg/basics), [integer//1]).

% DCG parser for regular expressions
re(Z) -->
    "^",
    !,  % don't backtrack into anywhere/1 rule
    basic_re(W),
    re_tail(W, Z).
re(anywhere(Z)) -->
    basic_re(W),
    re_tail(W, Z).


re_tail(W, Z) -->
    "|",
    basic_re(X),
    re_tail(union(W,X), Z).
re_tail(W, W) -->
    { true }.


basic_re(Z) -->
    simple_re(W),
    basic_re_tail(W, Z).

basic_re_tail(W, Z) -->
    simple_re(X),
    basic_re_tail(conc(W,X), Z).
basic_re_tail(W, W) -->
    { true }.


simple_re(Z) -->
    elemental_re(W),
    simple_re_tail(W, Z).

simple_re_tail(W, count(W,0,999_999_999)) -->
    "*".
simple_re_tail(W, count(W,1,999_999_999)) -->
    "+".
simple_re_tail(W, count(W,0,1)) -->
    "?".
simple_re_tail(W, count(W,N,N)) -->
    % {n}
    "{",
    integer(N),
    { N >= 0 },
    "}".
simple_re_tail(W, count(W,N,999_999_999)) -->
    % {n,}
    "{",
    integer(N),
    { N >= 0 },
    ",",
    "}".
simple_re_tail(W, count(W,N,M)) -->
    % {n,m}
    "{",
    integer(N),
    { N >= 0 },
    ",",
    integer(M),
    { M >= N },
    "}".
simple_re_tail(W, W) -->
    { true }.


elemental_re(any) -->
    ".".
elemental_re(group(X)) -->
    "(",
    re(X),
    ")".
elemental_re(eos) -->
    "$".
elemental_re(char(C)) -->
    [C],
    { \+ re_metachar([C]) }.
elemental_re(RE) -->
    "\\",
    [C],
    { perl_character_class(C, RE) }.
elemental_re(char(C)) -->
    "\\",
    [C],
    { re_metachar([C]) }.
elemental_re(neg_set(X)) -->
    "[^",
    !,  % don't backtrack into pos_set/1 clause below
    set_items(X),
    "]".
elemental_re(pos_set(X)) -->
    "[",
    set_items(X),
    "]".


re_metachar("^").
re_metachar("\\").
re_metachar("|").
re_metachar("*").
re_metachar("+").
re_metachar(".").
re_metachar("?").
re_metachar("[").
re_metachar("$").
re_metachar("(").
re_metachar(")").


% define Perl character classes as character sets
perl_character_class(0'd, pos_set([ range(48,57) ])).  % 0-9
perl_character_class(0'w, pos_set([ range(48,57)       % 0-9
                                  , range(65,90)       % A-Z
                                  , range(97,122)      % a-z
                                  , char(95)           % underscore
                                  ])).
perl_character_class(0's, pos_set([ char(0'\t)  % tab
                                  , char(0'\n)  % newline
                                  , char(0'\f)  % form feed
                                  , char(0'\r)  % carriage return
                                  , char(0' )   % space
                                  ])).
perl_character_class(Upper,neg_set(Set)) :-
    code_type(Lower, lower(Upper)),
    perl_character_class(Lower, pos_set(Set)).


set_items([Item1|MoreItems]) -->
    set_item(Item1),
    set_items(MoreItems).
set_items([Item1]) -->
    set_item(Item1).

set_item(char(C)) -->
    [C],
    { \+ set_metachar([C]) }.
set_item(char(C)) -->
    "\\",
    [C],
    { set_metachar([C]) }.
set_item(range(A,B)) -->
    set_item(char(A)),
    "-",
    set_item(char(B)).


set_metachar("\\").
set_metachar("]").
set_metachar("-").

