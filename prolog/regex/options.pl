:- module(regex_options, [ adjust_case/3
                         , singleline_mode/1
                         , new_options/2
                         ]).
:- use_module(library(apply), [ foldl/4 ]).
:- use_module(library(record)).

:- record options(i='-',s='-').

%% new_options(+Atom, -Options) is semidet
%
%  True if Options is an opaque value representing the regular expression
%  options described by Atom.  Atom should be something like 'ims',
%  'xi', etc.  Fails if Atom contains an unknown option.
new_options(Atom, Options) :-
    atom_chars(Atom, Chars),
    default_options(Options0),
    foldl(set_option, Chars, Options0, Options).


%% set_option(+Option, +Options0, -Options) is semidet
%
%  Sets the option Option, giving a new Options value.
set_option(i) -->
    set_i_of_options('+').
set_option(s) -->
    set_s_of_options('+').


%% adjust_case(+Options, +Code0, -Code) is det.
%
%  True if Code represents the same letter as Code0 but with case
%  adjusted to compensate for the 'i' regular expression option (aka
%  case insensitive).
adjust_case(Options, Code0, Code) :-
    ( options_i(Options, '+') ->
          code_type(Code, to_lower(Code0))
    ; % otherwise ->
          Code = Code0
    ).

%%	singleline_mode(+Options) is semidet.
%
%	True if Options request single-line mode (`/s`).
singleline_mode(Options) :-
    options_s(Options, '+').
