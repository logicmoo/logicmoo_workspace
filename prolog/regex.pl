:- module(regex, [ (=~)/2
                 , (\~)/2
                 , op(700,xfx,=~)
                 , op(700,xfx,\~)
                 ]).
:- use_module(library(error), [domain_error/2]).
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
    Text =~ Pattern/''.
Text =~ Pattern/OptionAtom :-
    new_options(OptionAtom, Options),
    text_codes(Text, T),
    text_codes(Pattern, P),
    ( phrase(re(Options, Re),P) ->
        once(engine_match(Re, Options, _, T, _))
    ; % bad pattern ->
        atom_codes(A, P),
        domain_error(regex, A)
    ).


%%  \~(+Text, +Pattern) is semidet.
%
%   Like `\+ Text =~ Pattern`.
Text \~ Pattern :-
    \+ Pattern = _/_,  % no options
    !,                 % next clause can't match
    Text \~ Pattern/''.
Text \~ Pattern/OptionAtom :-
    new_options(OptionAtom, Options),
    text_codes(Text, T),
    text_codes(Pattern, P),
    ( phrase(re(Options,Re),P) ->
        \+ engine_match(Re, Options, _, T, _)
    ; % bad pattern ->
        atom_codes(A, P),
        domain_error(regex, A)
    ).


%%  text_codes(+Text, -Codes)
%
%   Convert Text (atom or codes) into Codes.
text_codes(Atom, Codes) :-
    atom(Atom),
    !,
    atom_codes(Atom, Codes).
text_codes(Codes, Codes).

