:- module(regex_parser, [re//2]).
:- use_module(library(dcg/basics), [integer//1, string//1]).
:- use_module(library(regex/options), [adjust_case/3]).

% DCG parser for regular expressions
re(Opt, Z) -->
    basic_re(Opt,W),
    re_tail(Opt,W,Z).


re_tail(Opt, W, Z) -->
    "|",
    basic_re(Opt,X),
    re_tail(Opt,union(W,X), Z).
re_tail(_Opt, W, W) -->
    { true }.


basic_re(Opt, Z) -->
    simple_re(Opt,W),
    basic_re_tail(Opt,W,Z).

basic_re_tail(Opt, W, Z) -->
    simple_re(Opt,X),
    basic_re_tail(Opt,conc(W,X), Z).
basic_re_tail(_Opt, W, W) -->
    { true }.


simple_re(Opt, Z) -->
    elemental_re(Opt,W),
    simple_re_tail(Opt,W,Z).

simple_re_tail(_Opt, W, count(W,0,999_999_999)) -->
    "*".
simple_re_tail(_Opt, W, count(W,1,999_999_999)) -->
    "+".
simple_re_tail(_Opt, W, count(W,0,1)) -->
    "?".
simple_re_tail(_Opt, W, count(W,N,N)) -->
    % {n}
    "{",
    integer(N),
    { N >= 0 },
    "}".
simple_re_tail(_Opt, W, count(W,N,999_999_999)) -->
    % {n,}
    "{",
    integer(N),
    { N >= 0 },
    ",",
    "}".
simple_re_tail(_Opt, W, count(W,N,M)) -->
    % {n,m}
    "{",
    integer(N),
    { N >= 0 },
    ",",
    integer(M),
    { M >= N },
    "}".
simple_re_tail(_Opt, W, W) -->
    { true }.


elemental_re(_Opt, any) -->
    ".".
%elemental_re(_Opt, caret) -->
%    "^".
elemental_re(Opt, group(X)) -->
    "(",
    re(Opt, X),
    ")".
elemental_re(Opt, named_group(Name, X)) -->
    "(?<",
    string(NameCodes),
    { atom_codes(Name, NameCodes) },
    ">",
    re(Opt, X),
    ")".
elemental_re(_Opt, eos) -->
    "$".
elemental_re(Opt, char(C)) -->
    [C0],
    { \+ re_metachar(C0) },
    { adjust_case(Opt, C0, C) }.
elemental_re(_Opt, RE) -->
    "\\",
    [C],
    { perl_character_class(C, RE) }.
elemental_re(_Opt, char(C)) -->
    "\\",
    [C],
    { re_metachar(C) }.
elemental_re(_Opt, neg_set(X)) -->
    "[^",
    !,  % don't backtrack into pos_set/1 clause below
    set_items(X),
    "]".
elemental_re(_Opt, pos_set(X)) -->
    "[",
    set_items(X),
    "]".


% true if argument is a code for a regular expression meta character
re_metachar(0'^).
re_metachar(0'\\).
re_metachar(0'|).
re_metachar(0'*).
re_metachar(0'+).
re_metachar(0'.).
re_metachar(0'?).
re_metachar(0'[).
re_metachar(0'$).
re_metachar(0'().
re_metachar(0')).


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

