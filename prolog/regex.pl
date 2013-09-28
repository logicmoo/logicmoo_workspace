:- module(regex, [ (=~)/2
                 , (\~)/2
                 , op(700,xfx,=~)
                 , op(700,xfx,\~)
                 , regex/4
                 ]).
:- use_module(library(error), [domain_error/2]).
:- use_module(library(regex/captures), [new_captures/2, finalize_captures/1]).
:- use_module(library(regex/options), [new_options/2]).
:- use_module(library(regex/parser), [re//2]).
:- use_module(library(regex/engine/pp), [engine_match/5]).


% operators for matching strings against regular expressions.
% the syntax is the same used by Perl and Haskell, but Prolog
% doesn't like '!' in operators so I had to use '\' instead.
:- op(700,xfx,=~).
:- op(700,xfx,\~).


%%  =~(+Text, +Pattern) is semidet.
%
%   True if Text matches regular expression Pattern. Only the first
%   match is considered.  Text and Pattern can be atoms or code lists.
Text =~ Pattern :-
    \+ Pattern = _/_,  % no options
    !,                 % next clause can't match
    regex(Pattern,[],Text,_).
Text =~ Pattern/Options :-
    regex(Pattern,Options,Text,_).


%%  \~(+Text, +Pattern) is semidet.
%
%   Like `\+ Text =~ Pattern`.
Text \~ Pattern :-
    \+ Text =~ Pattern.


%%  regex(+Pattern:text,+Options,+Text:text,?Captures:list) is semidet
%
%   True if Text matches the regular expression Pattern. The pattern's
%   behavior is influenced by Options (see below). The values of any
%   capturing subgroups are unified with Captures (see below). A `text`
%   value may either be an atom or a list of codes.
%
%   Options can either be an atom or a list of options. If an atom, it's
%   split into a list of single character atoms which is used as the
%   Options value.  This allows on to use `is`, for example, instead of
%   `[i,s]`.  Acceptable options are:
%
%     * `i` - case-insensitive (default false)
%     * `s` - let `.` match `\n` (default false)
%
%   Captures is unified with a list of captured values, with the
%   leftmost capture first, etc. Each captured value is a list of codes.
%   For example,
%
%       ?- regex('(a+)(b*)', [], 'aaabbbbb', [A,B]).
%       A = "aaa",
%       B = "bbbbb".
%
%   A brief word on argument order. Prolog convention prefers to place
%   an Options argument as the final argument or as the last one before
%   outputs. However, widely followed regular expression
%   convention places options immediately after the pattern. I chose to
%   follow the latter convention. This argument order
%   benefits higher-order calls like maplist/3 which can do things
%   like:
%
%       ?- maplist(regex('(a+)(b+)', i), [ab, aab, abb], L).
%       L = [["a", "b"], ["aa", "b"], ["a", "bb"]].
regex(Pattern,Options,Text,Captures) :-
    % normalize text representations
    text_codes(Text, T),
    text_codes(Pattern, P),

    % normalize options representation
    new_options(Options, O),

    % normalize captures representation
    new_captures(Captures, C),

    % compile Pattern
    ( phrase(re(O,Re),P) ->
        once(regex_no_sugar(Re, O, C, T, _)),
        finalize_captures(C)
    ; % invalid pattern ->
        atom_codes(A, P),
        domain_error(regex, A)
    ).


% the heart and soul of regex/4
regex_no_sugar(Re, Options, Captures) -->
    engine_match(Re, Options, Captures).
regex_no_sugar(Re, Options, Captures) -->
    [_],
    regex_no_sugar(Re, Options, Captures).


%%  text_codes(+Text, -Codes)
%
%   Convert Text (atom or codes) into Codes.
text_codes(Atom, Codes) :-
    atom(Atom),
    !,
    atom_codes(Atom, Codes).
text_codes(Codes, Codes).

