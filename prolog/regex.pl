:- module(regex, [ (=~)/2
                 , (\~)/2
                 , op(700,xfx,=~)
                 , op(700,xfx,\~)
                 , regex/4
                 ]).
:- use_module(library(error), [domain_error/2]).
:- use_module(library(regex/state), [new_state/3, numbered_captures/2]).
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
%
%   Named captures are automatically bound to corresponding named
%   variables in the surrounding scope.  For example,
%
%       "Hi John" =~ "hi (?<Name>[a-z]+)"/i,
%       Name == "John".
Text =~ Pattern :-
    expand_equalstilde(Text =~ Pattern, _, Goal),
    call(Goal).

expand_equalstilde(Text =~ Pattern, Vars, regex(P,Options,Text,Vars)) :-
    ( Pattern = P/Options ->
        true
    ; % no explicit options ->
        P = Pattern,
        Options = []
    ).

% macro expansion giving access to in-scope variables.
user:goal_expansion(Text =~ Pattern, Goal) :-
    % is goal expansion wanted?
    prolog_load_context(module, Module),
    Module \== regex,  % we don't want string interpolation ourselves
    predicate_property(Module:(_=~_),imported_from(regex)),

    prolog_load_context(variable_names, Vars),
    expand_equalstilde(Text =~ Pattern, Vars, Goal).


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
%   Named captures are also supported. In that case, Captures must be
%   a list of pairs like `['A'=A,'B'=B]`. Every named capture in the
%   pattern must have a corresponding key in Captures. (This is a
%   temporary restriction and will be removed later).
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
    text_codes(Pattern, P0),
    starts_with_caret(P0,P,StartingCaret),

    % normalize options and captures into a state value
    new_state(Options, Captures, State0),

    % compile Pattern
    ( phrase(re(State0,Re),P) ->
        ( StartingCaret=yes ->
            once(engine_match(Re, State0, State, T, _))
        ; otherwise ->
            once(regex_no_sugar(Re, State0, State, T, _))
        ),
        ( var(Captures) ->
            numbered_captures(State, Captures)
        ; % captures already bound ->
            true
        )
    ; % invalid pattern ->
        atom_codes(A, P),
        domain_error(regex, A)
    ).


starts_with_caret([0'^|P],P,yes) :- % ' syntax highlighter
    !.
starts_with_caret(P,P,no).


% the heart and soul of regex/4
regex_no_sugar(Re, State0, State) -->
    engine_match(Re, State0, State).
regex_no_sugar(Re, State0, State) -->
    [_],
    regex_no_sugar(Re, State0, State).


%%  text_codes(+Text, -Codes)
%
%   Convert Text (atom or codes) into Codes.
text_codes(Atom, Codes) :-
    atom(Atom),
    !,
    atom_codes(Atom, Codes).
text_codes(String, Codes) :-
    string(String),
    !,
    string_codes(String, Codes).
text_codes(Codes, Codes).
