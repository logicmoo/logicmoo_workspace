/*  Part of SWI-Prolog

    Author:        Jan Wielemaker and Peter Ludemann
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2017-2022, VU University Amsterdam
                              SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(pcre,
          [ re_match/2,           % +Regex, +String
            re_match/3,           % +Regex, +String, +Options
            re_matchsub/3,        % +Regex, +String, -Subs
            re_matchsub/4,        % +Regex, +String, -Subs, +Options
            re_foldl/6,           % :Goal, +Regex, +String, ?V0, ?V, +Options
            re_split/3,           % +Pattern, +String, -Split:list
            re_split/4,           % +Pattern, +String, -Split:list, +Options
            re_replace/4,         % +Pattern, +With, +String, -NewString
            re_replace/5,         % +Pattern, +With, +String, -NewString, +Options
            re_compile/3,         % +Pattern, -Regex, +Options
            re_flush/0,
            re_config/1           % ?Config
          ]).
:- autoload(library(apply), [maplist/2, maplist/3]).
:- autoload(library(error), [must_be/2, existence_error/2]).
:- autoload(library(dcg/basics), [eos/2, digit/3, digits/3]).
:- autoload(library(lists), [append/3]).

:- use_foreign_library(foreign(pcre4pl)).

:- meta_predicate
    re_foldl(3, +, +, ?, ?, +).

/** <module> Perl compatible regular expression matching for SWI-Prolog

This module provides an interface  to  the [PCRE2](http://www.pcre.org/)
(Perl Compatible Regular Expression)  library.   This  Prolog  interface
provides an almost comprehensive wrapper around PCRE2  (the successor to
PCRE)  with as much backward compatibility to PCRE as possible,  because
the original implementation was for PCRE (also known as PCRE1).

Regular  expressions  are  created  from   a  pattern  and  options  and
represented as  a SWI-Prolog _blob_.   This implies they are  subject to
(atom) garbage  collection. Compiled  regular expressions can  safely be
used  in multiple  threads. Most  predicates accept  both an  explicitly
compiled regular  expression, a pattern,  or a term  Pattern/Flags.  The
semantics of the pattern can be additionally modified by options. In the
latter two cases a regular expression  _blob_ is created and stored in a
cache. The cache can be cleared using re_flush/0.

@see `man pcre2api` or https://www.pcre.org/current/doc/html/pcre2api.html
     for details of the PCRE2 syntax and options.
*/

:- predicate_options(re_match/3, 3,
                     [ start(integer), % Not part of pcre2 API
                       % These are in the same order as in pcre4pl.c, to make it easy to compare them
                       anchored(boolean),    % Also re_compile/3
                       utf_check(boolean),   % Also re_compile/3
                       endanchored(boolean), % Also re_compile/3
                       bol(boolean),
                       eol(boolean),
                       empty(boolean),
                       empty_atstart(boolean),
                       partial_soft(bool),
                       partial_hard(bool),
                       % dfa_restart(bool),  % TODO: if pcre2_dfa_match() is supported
                       % dfa_shortest(bool), % TODO: if pcre2_dfa_match() is supported
                       jit(boolean),
                       copy_matched_subject(boolean)
                     ]).
:- predicate_options(re_compile/3, 3,
                     [ capture_type(oneof([atom,string,range])), % Not part of pcre2 API
                       % These are in the same order as in pcre4pl.c, to make it easy to compare them
                       anchored(boolean),    % Also re_match/3
                       utf_check(boolean),   % Also re_match/3
                       endanchored(boolean), % Also re_match/3
                       allow_empty_class(boolean),
                       alt_bsux(boolean),
                       auto_callout(boolean),
                       caseless(boolean),
                       dollar_endonly(boolean),
                       dotall(boolean),
                       dupnames(boolean),
                       extended(boolean),
                       firstline(boolean),
                       match_unset_backref(boolean),
                       multiline(boolean),
                       never_ucp(boolean),
                       never_utf(boolean),
                       auto_capture(boolean),
                       no_auto_capture(boolean), % backwards compatibility
                       auto_possess(boolean),
                       dotstar_anchor(boolean),
                       start_optimize(boolean),
                       ucp(boolean),
                       greedy(boolean),
                       ungreedy(boolean), % Backwards compatibility
                       utf(boolean),
                       never_backslash_c(boolean),
                       alt_circumflex(boolean),
                       alt_verbnames(boolean),
                       use_offset_limit(boolean),
                       extended_more(boolean),
                       literal(boolean),
                       match_invalid_utf(boolean),
                       jit_complete(boolean),
                       jit_partial_soft(boolean),
                       jit_partial_hard(boolean),
                       jit_invalid_utf(boolean),
                       bsr(oneof([anycrlf,unicode])),
                       bsr2(oneof([anycrlf,unicode])),
                       compat(oneof([])), % Obsolete
                       newline(oneof([any,anycrlf,cr,lf,crlf,nul])),
                       newline2(oneof([any,anycrlf,cr,lf,crlf,nul]))
                     ]).
:- predicate_options(re_matchsub/4, 4,
                     [ pass_to(re_match/3, 3)
                     ]).
:- predicate_options(re_foldl/6, 6,
                     [ pass_to(re_match/3, 3)
                     ]).
:- predicate_options(re_split/4, 4,
                     [ pass_to(re_match/3, 3)
                     ]).
:- predicate_options(re_replace/5, 5,
                     [ pass_to(re_match/3, 3)
                     ]).

%!  re_match(+Regex, +String) is semidet.
%!  re_match(+Regex, +String, +Options) is semidet.
%
%   Succeeds if String matches Regex.  For example:
%
%     ```
%     ?- re_match("^needle"/i, "Needle in a haystack").
%     true.
%     ```
%
%   Defined  Options  are  given  below.   For  details,  see  the  PCRE
%   documentation.  If  an option is  repeated, the first value  is used
%   and  subsequent  values  are   ignored.   Unrecognized  options  are
%   ignored.   Unless otherwise  specified, boolean  options default  to
%   `false`.
%
%   If Regex is a text pattern  (optionally with flags), then any of the
%   Options for  re_compile/3 can  be used, in  addition to  the Options
%   listed below. If Regex is the  result of re_compile/3, then only the
%   following execution-time  Options are recognized and  any others are
%   ignored. Some options may not exist on your system, depending on the
%   PCRE2 version and  how it was built - these  unsupported options are
%   silently ignored.
%
%     * start(From)
%     Start at the given character index
%     * anchored(Bool)
%     If `true`, match only at the first position
%     * bol(Bool)
%     String is the beginning of a line (default `true`) -
%       affects behavior of circumflex metacharacter (`^`).
%     * empty(Bool)
%     An empty string is a valid match (default `true`)
%     * empty_atstart(Bool)
%     An empty string at the start of the subject is a valid match
%     (default `true`)
%     * eol(Bool)
%     String is the end of a line -
%       affects behavior of dollar metacharacter (`$`)
%       (default `true`).
%     * newline(Mode)
%     If `any`, recognize any Unicode newline sequence,
%     if `anycrlf`, recognize CR, LF, and CRLF as newline
%     sequences, if `cr`, recognize CR, if `lf`, recognize
%     LF, if `crlf` recognize CRLF as newline.
%     The default is determined by how PCRE was built, and
%     can be found by re_config(newline2(NewlineDefault)).
%     * newline2(Mode) - synonym for newline(Mode).
%     * utf_check(Bool) - see [PCRE2 API documentation](https://www.pcre.org/current/doc/html/pcre2api.html)
%     You should not need this because SWI-Prolog ensures that the UTF8 strings are valid,
%     so the default is `false`.
%     * endanchored(Bool) - see [PCRE2 API documentation](https://www.pcre.org/current/doc/html/pcre2api.html)
%     * partial_soft(Bool) - see [PCRE2 API documentation](https://www.pcre.org/current/doc/html/pcre2api.html)
%     * partial_hard(Bool) - see [PCRE2 API documentation](https://www.pcre.org/current/doc/html/pcre2api.html)
%     * dfa_restart(Bool) - see [PCRE2 API documentation](https://www.pcre.org/current/doc/html/pcre2api.html)
%     * dfa_shortest(Bool) - see [PCRE2 API documentation](https://www.pcre.org/current/doc/html/pcre2api.html)
%
%   @arg  Regex is  the  output of  re_compile/3, a  pattern  or a  term
%   Pattern/Flags, where Pattern is an atom or string. The defined flags
%   and their related option for re_compile/3 are below.
%     - *x*: extended(true)
%     - *i*: caseless(true)
%     - *m*: multiline(true)
%     - *s*: dotall(true)
%     - *a*: capture_type(atom)
%     - *r*: capture_type(range)
%     - *t*: capture_type(term)
%
%   If Regex is the output  of re_compile/3, any compile-time options in
%   Options or Flags are ignored and only match-time options are used.
%
%   The options  that are  derived from flags  take precedence  over the
%   options in the  Options list. In the case of  conflicting flags, the
%   first one is used (e.g., `ra` results in `capture_type(range)`).

re_match(Regex, String) :-
    re_match(Regex, String, []).
re_match(Regex, String, Options) :-
    re_compiled(Regex, Compiled, Options),
    re_match_(Compiled, String, Options).

%!  re_matchsub(+Regex, +String, -Sub:dict) is semidet.
%!  re_matchsub(+Regex, +String, -Sub:dict, +Options) is semidet.
%
%   Match String  against Regex.  On  success, Sub is a  dict containing
%   integer keys  for the numbered capture  group and atom keys  for the
%   named capture groups. The entire match  string has the key `0`.  The
%   associated  value is  determined  by  the capture_type(Type)  option
%   passed  to  re_compile/3, or  by  flags  if  Regex  is of  the  form
%   Pattern/Flags;  and may  be  specified at  the  level of  individual
%   captures  using  a  naming  convention for  the  caption  name.  See
%   re_compile/3 for details.
%
%   The  example  below  exploits  the  typed groups  to  parse  a  date
%   specification:
%
%     ```
%     ?- re_matchsub("(?<date> (?<year_I>(?:\\d\\d)?\\d\\d) -
%                     (?<month_I>\\d\\d) - (?<day_I>\\d\\d) )"/x,
%                    "2017-04-20", Sub, []).
%     Sub = re_match{0:"2017-04-20", date:"2017-04-20",
%                    day:20, month:4, year:2017}.
%
%     ```
%
%   @arg Both compilation and execution options are processed.  See
%   re_compile/3 and re_match/3 for the set of options. In addition,
%   some compilation options may passed as ``/Flags`` to Regex - see
%   re_match/3 for the list of flags.
%
%   @arg Regex  See re_match/2 for a description of this argument.

re_matchsub(Regex, String, Subs) :-
    re_matchsub(Regex, String, Subs, []).

re_matchsub(Regex, String, Subs, Options) :-
    re_compiled(Regex, Compiled, Options),
    re_matchsub_(Compiled, String, Pairs, Options),
    dict_pairs(Subs, re_match, Pairs).

%!  re_foldl(:Goal, +Regex, +String, ?V0, ?V, +Options) is semidet.
%
%   Fold all matches of Regex on String.  Each match is represented by a
%   dict as specified  for re_matchsub/4.  V0 and V are  related using a
%   sequence of invocations of Goal as illustrated below.
%
%       ```
%       call(Goal, Dict1, V0, V1),
%       call(Goal, Dict2, V1, V2),
%       ...
%       call(Goal, Dictn, Vn, V).
%       ```
%
%   This predicate is used to implement re_split/4 and re_replace/4. For
%   example, we  can count all matches  of a Regex on  String using this
%   code:
%
%     ```
%     re_match_count(Regex, String, Count) :-
%         re_foldl(increment, Regex, String, 0, Count, []).
%
%     increment(_Match, V0, V1) :-
%         V1 is V0+1.
%     ```
%
%   After which we can query
%
%     ```
%     ?- re_match_count("a", "aap", X).
%     X = 2.
%     ```
%
%  Here is an example Goal for extracting all the matches with their
%  offsets within the string:
%
%  ```
%  range_match(Dict, StringIndex-[MatchStart-Substring|List], StringIndex-List) :-
%      Dict.(StringIndex.index) = MatchStart-MatchLen,
%      sub_string(StringIndex.string, MatchStart, MatchLen, _, Substring).
%  ```
%  And can be used with this query (note the capture_type(range) option,
%  which is needed by `range_match/3`, and greedy(false) to invert the
%  meaning of `*?`):
%  ```
%  ?- String = "{START} Mary {END} had a {START} little lamb {END}",
%     re_foldl(range_match,
%              "{START} *?(?<piece>.*) *?{END}",
%              String, _{string:String,index:piece}-Matches, _-[],
%              [capture_type(range),greedy(false)]).
%  Matches = [8-"Mary", 33-"little lamb"].
%  ```
re_foldl(Goal, Regex, String, V0, V, Options) :-
    re_compiled(Regex, Compiled, Options),
    re_foldl_(Compiled, String, Goal, V0, V, Options).

:- public re_call_folder/4. % prevent code obfusication name mangling
:- meta_predicate re_call_folder(2, +, ?, ?).

%   re_call_folder(:Goal, +Pairs, ?V0, ?V1).
%   Used by re_foldl_/6 to call Goal with a dict.
%     DO NOT use "%!" comment - that would add it to the docs
re_call_folder(Goal, Pairs, V0, V1) :-
    dict_pairs(Dict, re_match, Pairs),
    call(Goal, Dict, V0, V1).


%!  re_split(+Pattern, +String, -Splits:list) is det.
%!  re_split(+Pattern, +String, -Splits:list, +Options) is det.
%
%   Split String using the regular  expression Pattern. Splits is a list
%   of strings holding alternating matches  of Pattern and skipped parts
%   of the String, starting with a  skipped part.  The Splits lists ends
%   with a  string of  the content  of String after  the last  match. If
%   Pattern does not  appear in String, Splits is a  list holding a copy
%   of String. This implies the number of elements in Splits is _always_
%   odd.  For example:
%
%     ```
%     ?- re_split("a+", "abaac", Splits, []).
%     Splits = ["","a","b","aa","c"].
%     ?- re_split(":\\s*"/n, "Age: 33", Splits, []).
%     Splits = ['Age', ': ', 33].
%     ```
%
%   @arg  Pattern is  the pattern  text, optionally  follows by  /Flags.
%   Similar to re_matchsub/4, the final output type can be controlled by
%   a flag `a` (atom), `s` (string, default) or `n` (number if possible,
%   atom otherwise).

re_split(Pattern, String, Splits) :-
    re_split(Pattern, String, Splits, []).
re_split(Pattern, String, Splits, Options) :-
    split_range_regex(Pattern, Compiled, Type, Options),
    State = state(String, 0, Type),
    re_foldl(split(State), Compiled, String, Splits, [Last], Options),
    arg(2, State, LastSkipStart),
    typed_sub(Type, String, LastSkipStart, _, 0, Last).

split_range_regex(Pattern/Flags, Compiled, Type, Options) =>
    split_range_regex(Pattern, Flags, Compiled, Type, Options).
split_range_regex(Pattern, Compiled, Type, Options) =>
    split_range_regex(Pattern, '', Compiled, Type, Options).

split_range_regex(Pattern, Flags, Compiled, Type, Options) =>
    regex_capture_type_flag_chars(Flags, Chars, Options),
    split_flags(Chars, Chars1, Type),
    atom_chars(RFlags, [r|Chars1]),
    re_flags_options(RFlags, ROptions),
    append(ROptions, Options, Options2),
    re_compiled(Pattern/RFlags, Compiled, Options2).

split_flags([], [], Type) :-
    default(Type, string).
split_flags([H|T0], T, Type) :-
    split_type(H, Type),
    !,
    split_flags(T0, T, Type).
split_flags([H|T0], [H|T], Type) :-
    split_flags(T0, T, Type).

split_type(a, atom).
split_type(s, string).
split_type(n, name).

split(State, Dict, [Skipped,Sep|T], T) :-
    matched(State, Dict.0, Sep),
    skipped(State, Dict.0, Skipped).

matched(state(String, _, Type), Start-Len, Matched) :-
    typed_sub(Type, String, Start, Len, _, Matched).

skipped(State, Start-Len, Skipped) :-
    State = state(String, Here, Type),
    SkipLen is Start-Here,
    typed_sub(Type, String, Here, SkipLen, _, Skipped),
    NextSkipStart is Start+Len,
    nb_setarg(2, State, NextSkipStart).

typed_sub(string, Haystack, B, L, A, String) :-
    sub_string(Haystack, B, L, A, String).
typed_sub(atom, Haystack, B, L, A, String) :-
    sub_atom(Haystack, B, L, A, String).
typed_sub(name, Haystack, B, L, A, Value) :-
    sub_string(Haystack, B, L, A, String),
    (   number_string(Number, String)
    ->  Value = Number
    ;   atom_string(Value, String)
    ).

%!  re_replace(+Pattern, +With, +String, -NewString) is det.
%!  re_replace(+Pattern, +With, +String, -NewString, +Options) is det.
%
%   Replace matches  of the  regular expression  Pattern in  String with
%   With (possibly containing references to captured substrings).
%
%   Throws  an error  if With  uses  a name  that doesn't  exist in  the
%   Pattern.
%
%   @arg Pattern  is the  pattern text,  optionally followed  by /Flags.
%   Flags  may include  `g`, replacing  all occurences  of Pattern.   In
%   addition, similar  to re_matchsub/4,  the final  output type  can be
%   controlled  by a  flag `a`  (atom)  or `s`  (string, default).   The
%   output  type can  also be  specified by  the `capture_type`  option.
%   Capture  type  suffixes  can   modify  behavior;  for  example,  the
%   following  will  change an  ISO  8601  format date  (YYYY-MM-DD)  to
%   American style (m/d/y),  and also remove leading zeros  by using the
%   `_I` suffix:
%
%   ```
%   re_replace("(?<date> (?<year_I>(?:\\d\\d)?\\d\\d) -
%               (?<month_I>\\d\\d) - (?<day_I>\\d\\d) )"/x,
%              "$month-$day-$year",
%              ISODate, AmericanDate)`
%   ```
%
%   @arg  With  is  the  replacement text.  It  may  reference  captured
%   substrings using \N or $Name. Both N  and Name may be written as {N}
%   and {Name} to avoid ambiguities. If  a substring is named, it cannot
%   be referenced by its number. The single chracters `$` and `\` can be
%   escaped  by  doubling  (e.g.,  `re_replace(".","$$","abc",Replaced)`
%   results in  `Replaced="$bc"`). (Because  `\` is an  escape character
%   inside strings, you need to write "\\\\" to get a single backslash.)
%
%   @arg Options See re_match/3 for the set of options.
%
%   The options  that are  derived from flags  take precedence  over the
%   options in the  Options list. In the case of  conflicting flags, the
%   first one  is used  (e.g., `as` results  in `capture_type(string)`).
%   If  a  `capture_type` is  meaningless  (`range`  or `term`),  it  is
%   ignored.

re_replace(Pattern, With, String, NewString) :-
    re_replace(Pattern, With, String, NewString, []).

re_replace(Pattern, With, String, NewString, Options) :-
    replace_range_regex(Pattern, Compiled, All, Type, Options),
    compile_replacement(With, RCompiled),
    State = state(String, 0, Type),
    (   All == all
    ->  re_foldl(replace(State, RCompiled), Compiled, String, Parts, [Last], [])
    ;   (   re_matchsub(Compiled, String, Match, [])
        ->  replace(State, RCompiled, Match, Parts, [Last])
        ;   Repl = false
        )
    ),
    (   Repl == false
    ->  parts_to_output(Type, [String], NewString)
    ;   arg(2, State, LastSkipStart),
        sub_string(String, LastSkipStart, _, 0, Last),
        parts_to_output(Type, Parts, NewString)
    ).

regex_capture_type_flag_chars(Flags, Chars, Options) :-
    atom_chars(Flags, Chars0),
    % For replace or split, the capture_type must be range, so if a
    % different result is desired, it is specified in the flags. The
    % following code converts an Options capture_type to a flag
    % character and appends it to the Flags.
    (   memberchk(capture_type(T), Options),
        type_flag(TFlag, T)
    ->  % No need to do delete(Options,capture_type(_),Options2)
        % because Flags take precedence and first occurence in Options
        % takes precedence.
        append(Chars0, [TFlag], Chars)
    ;   Chars = Chars0
    ).

%! replace_range_regex(+Pattern, -Compiled, -All, -Type, +Options) is det.
replace_range_regex(Pattern/Flags, Compiled, All, Type, Options) =>
    replace_range_regex(Pattern, Flags, Compiled, All, Type, Options).
replace_range_regex(Pattern, Compiled, All, Type, Options) =>
    replace_range_regex(Pattern, '', Compiled, All, Type, Options).

replace_range_regex(Pattern, Flags, Compiled, All, Type, Options) =>
    regex_capture_type_flag_chars(Flags, Chars, Options),
    replace_flags(Chars, Chars1, All, Type),
    atom_chars(RFlags, [r|Chars1]),
    re_flags_options(RFlags, ROptions),
    append(ROptions, Options, Options2),
    re_compiled(Pattern, Compiled, Options2).

replace_flags([], [], All, Type) :-
    default(All, first),
    default(Type, string).
replace_flags([H|T0], T, All, Type) :-
    (   all_flag(H, All)
    ->  true
    ;   type_flag(H, Type)
    ),
    !,
    replace_flags(T0, T, All, Type).
replace_flags([H|T0], [H|T], All, Type) :-
    replace_flags(T0, T, All, Type).

all_flag(g, all).

type_flag(a, atom).
type_flag(s, string).

%! default(?Val, +Default) is det.
%  If Val isn't instantiated, instantiate it to Default.
%  If Val is already instantiated, succeed.
%  Equivalent to:
%     default( Val,  Default), var(Val) => Val = Default.
%     default(_Val, _Default) => true.
default(Val, Val) :- !.
default(_, _).

replace(State, With, Dict, [Skipped|Parts], T) :-
    State = state(String, _, _Type),
    copy_term(With, r(PartsR, Skel)),
    maplist(dict_pair_lookup(Dict), Skel),
    range_strings(PartsR, String, Parts, T),
    skipped(State, Dict.0, Skipped).

% dict_pair_lookup(d{a:1}, a-K) results in K=1.
dict_pair_lookup(Dict, Key-Dict.Key).

range_strings([], _, T, T).
range_strings([Start-Len|T0], String, [S|T1], T) :-
    !,
    sub_string(String, Start, Len, _, S),
    range_strings(T0, String, T1, T).
range_strings([S|T0], String, [S|T1], T) :-
    range_strings(T0, String, T1, T).

parts_to_output(string, Parts, String) :-
    atomics_to_string(Parts, String).
parts_to_output(atom, Parts, String) :-
    atomic_list_concat(Parts, String).

%!  compile_replacement(+With, -Compiled)
%
%   Compile the replacement specification  into a specification that can
%   be processed quickly. The compiled expressions are cached and may be
%   reclaimed using  re_flush/0 (which also removes  compiled Regex from
%   re_compile/3).
%
%   This "compilation" has nothing to  do with PCRE pattern compilation;
%   it's used by re_replace/5 to proces the With argument.

:- table compile_replacement/2 as shared.

compile_replacement(With, r(Parts, Extract)) :-
    string_codes(With, Codes),
    phrase(replacement_parts(Parts, Pairs), Codes),
    % Pairs is LookupKey-Slot pairs, where a LookupKey might be
    % duplicated (Slot is a shared variable within Parts).
    Extract = Pairs.

replacement_parts(Parts, Extract) -->
    string_escape(HCodes),
    (   ("\\" ; "$"),
        capture_name(Name)
    ->  !,
        { add_part(HCodes, Parts, T0),
          T0 = [Repl|T1],
          Extract = [Name-Repl|Extract1]
        },
        replacement_parts(T1, Extract1)
    ;   eos
    ->  !,
        { add_part(HCodes, Parts, []),
          Extract = []
        }
    ).

add_part([], Parts, Parts) :-
    !.
add_part(Codes, [H|T], T) :-
    string_codes(H, Codes).

%! string_escape(-Codes)// is nondet.
% Similar to dcg_basics:string(Codes) but also escapes "$" and "/"
string_escape([]) -->
    [].
string_escape([0'$|T]) -->
    "$$", !,
    string_escape(T).
string_escape([0'\\|T]) -->
    "\\\\", !,
    string_escape(T).
string_escape([H|T]) -->
    [H],
    string_escape(T).

capture_name(Name) -->
    "{",
    (   digit(D0)
    ->  digits(DL),
        "}",
        { number_codes(Name, [D0|DL]) }
    ;   letter(A0),
        alnums(AL),
        "}",
        { atom_codes(Name, [A0|AL]) }
    ).
capture_name(Name) -->
    digit(D0),
    !,
    digits(DL),
    { number_codes(Name, [D0|DL]) }.
capture_name(Name) -->
    letter(A0),
    !,
    alnums(AL),
    { atom_codes(Name, [A0|AL]) }.

letter(L) -->
    [L],
    { between(0'a,0'z,L)
    ; between(0'A,0'Z,L)
    ; L == 0'_
    }, !.

alnums([H|T]) -->
    alnum(H),
    !,
    alnums(T).
alnums([]) -->
    "".

alnum(L) -->
    [L],
    { between(0'a,0'z,L)
    ; between(0'A,0'Z,L)
    ; between(0'0,0'9,L)
    ; L == 0'_
    }, !.

%!  re_compile(+Pattern, -Regex, +Options) is det.
%
%   Compiles Pattern  to a  Regex _blob_ of  type `regex`  (see blob/2).
%   Defined  Options are  given below.   Please consult  the [PCRE2  API
%   documentation](https://www.pcre.org/current/doc/html/pcre2api.html)
%   for details.  If an option is  repeated, the first value is used and
%   subsequent values  are ignored.   Unrecognized options  are ignored.
%   Unless otherwise specified, boolean options default to `false`. Some
%   options may not exist on your system, depending on the PCRE2 version
%   and  how it  was  built  - these  unsupported  options are  silently
%   ignored.
%
%   The various matching predicates can take  either a Regex _blob_ or a
%   string  pattern; if  they  are  given a  string  pattern, they  call
%   re_compile/3 and cache the result; so, there is little reason to use
%   re_compile/3 directly.
%
%     * anchored(Bool)
%     If `true`, match only at the first position
%     * auto_capture(Bool)
%     Enable use of numbered capturing parentheses.
%     (default `true`)
%     * bsr(Mode)
%     If `anycrlf`, \R only matches CR, LF or CRLF;  if `unicode`,
%     \R matches all Unicode line endings.
%     * bsr2(Mode) - synonym for bsr(Mode).
%     * caseless(Bool)
%     If `true`, do caseless matching.
%     * compat(With)
%     Error   -   PCRE1   had  =|compat(javascript)|=   for   JavaScript
%     compatibility, but PCRE2 has removed that.
%     * dollar_endonly(Bool)
%     If `true`, $ not to match newline at end
%     * dotall(Bool)
%     If `true`, . matches anything including NL
%     * dupnames(Bool)
%     If `true`, allow duplicate names for subpatterns
%     * extended(Bool)
%     If `true`, ignore white space and # comments
%     * firstline(Bool)
%     If `true`, force matching to be before newline
%     * greedy(Bool)
%     If  `true`,  operators such  as  `+`  and  `*` are  greedy  unless
%     followed by `?`; if `false`, the  operators are not greedy and `?`
%     has the opposite meaning. It can also beset by a `(?U)` within the
%     pattern  -   see  the  [PCRE2  pattern   internal  option  setting
%     documentation](https://www.pcre.org/current/doc/html/pcre2pattern.html#SEC13)
%     for details and note that the PCRE2 option is `UNGREEDY`, which is
%     the inverse of this packages `greedy` options.  (default `true`)
%     * compat(With)
%     Raises an  errr - PCRE1 had  =|compat(javascript)|= for JavaScript
%     compatibility, but PCRE2 has removed that option . Consider using
%     the `alt_bsux` and `extra_alt_bsux` options.
%     * multiline(Bool)
%     If `true`, ^ and $ match newlines within data
%     * newline(Mode)
%     If  `any`, recognize  any Unicode  newline sequence;  if `anycrlf`
%     (default), recognize  CR, LF,  and CRLF  as newline  sequences; if
%     `cr`, recognize CR;  if `lf`, recognize LF;  `crlf` recognize CRLF
%     as  newline; if  `nul`,  recognize the  NULL  character (0x00)  as
%     newline.
%     * newline2(Mode) - synonym for newline(Mode).
%     * ucp(Bool)
%     If `true`, use Unicode properties for \d, \w, etc.
%     * utf_check(Bool) - see [PCRE2 API documentation](https://www.pcre.org/current/doc/html/pcre2api.html)
%     You should not need this because SWI-Prolog ensures that the UTF8 strings are valid,
%     * endanchored(boolean) - see [PCRE2 API documentation](https://www.pcre.org/current/doc/html/pcre2api.html)
%     * allow_empty_class(boolean) - see [PCRE2 API documentation](https://www.pcre.org/current/doc/html/pcre2api.html)
%     * alt_bsux(boolean) - see [PCRE2 API documentation](https://www.pcre.org/current/doc/html/pcre2api.html)
%     * auto_callout(boolean) - see [PCRE2 API documentation](https://www.pcre.org/current/doc/html/pcre2api.html)
%     * match_unset_backref(boolean) - see [PCRE2 API documentation](https://www.pcre.org/current/doc/html/pcre2api.html)
%     * never_ucp(boolean) - see [PCRE2 API documentation](https://www.pcre.org/current/doc/html/pcre2api.html)
%     * never_utf(boolean) - see [PCRE2 API documentation](https://www.pcre.org/current/doc/html/pcre2api.html)
%     * auto_possess(boolean) - see [PCRE2 API documentation](https://www.pcre.org/current/doc/html/pcre2api.html)
%     (default `true`)
%     * dotstar_anchor(boolean) - see [PCRE2 API documentation](https://www.pcre.org/current/doc/html/pcre2api.html)
%     (default `true`)
%     * start_optimize(boolean) - see [PCRE2 API documentation](https://www.pcre.org/current/doc/html/pcre2api.html)
%     (default `true`)
%     * utf(boolean) - see [PCRE2 API documentation](https://www.pcre.org/current/doc/html/pcre2api.html)
%     * never_backslash_c(boolean) - see [PCRE2 API documentation](https://www.pcre.org/current/doc/html/pcre2api.html)
%     * alt_circumflex(boolean) - see [PCRE2 API documentation](https://www.pcre.org/current/doc/html/pcre2api.html)
%     * alt_verbnames(boolean) - see [PCRE2 API documentation](https://www.pcre.org/current/doc/html/pcre2api.html)
%     * use_offset_limit(boolean) - see [PCRE2 API documentation](https://www.pcre.org/current/doc/html/pcre2api.html)
%     * extended_more(boolean) - see [PCRE2 API documentation](https://www.pcre.org/current/doc/html/pcre2api.html)
%     * literal(boolean) - see [PCRE2 API documentation](https://www.pcre.org/current/doc/html/pcre2api.html)
%     * match_invalid_utf(boolean) - see [PCRE2 API documentation](https://www.pcre.org/current/doc/html/pcre2api.html)
%     * jit_complete(boolean) - see [PCRE2 API documentation](https://www.pcre.org/current/doc/html/pcre2api.html)
%     * jit_partial_soft(boolean) - see [PCRE2 API documentation](https://www.pcre.org/current/doc/html/pcre2api.html)
%     * jit_partial_hard(boolean) - see [PCRE2 API documentation](https://www.pcre.org/current/doc/html/pcre2api.html)
%     * jit_invalid_utf(boolean) - see [PCRE2 API documentation](https://www.pcre.org/current/doc/html/pcre2api.html)
%     * jit(boolean) - see [PCRE2 API documentation](https://www.pcre.org/current/doc/html/pcre2api.html)
%     (default `true`)
%     * copy_matched_subject(boolean) - see [PCRE2 API documentation](https://www.pcre.org/current/doc/html/pcre2api.html)
%
%   In addition to the options above that directly map to PCRE flags the
%   following options are processed:
%
%     * optimise(Bool) or optimize(Bool)
%     Turns on the JIT compiler for additional optimization that greatly
%     that speeds  up the matching  performance of many  patterns. (Note
%     that he meaning has changed slightly from the PCRE1 implementation
%     - PCRE2  always optimises  where possible;  this is  an additional
%     optimisation.)
%     * capture_type(+Type)
%     How to return the matched part  of the input and possibly captured
%     groups in there.  Possible values are:
%       - string
%       Return the captured string as a string (default).
%       - atom
%       Return the captured string as an atom.
%       - range
%       Return the captured string as a pair `Start-Length`.  Note that
%       we use `Start-Length` rather than the more conventional
%       `Start-End` to allow for immediate use with sub_atom/5 and
%       sub_string/5.
%       - term
%       Parse the  captured string  as a Prolog  term.  This  is notably
%       practical if you capture a number.
%
%    The  `capture_type` specifies  the default  for this  pattern.  The
%    interface supports  a different type  for each _named_  group using
%    the syntax  `(?<name_T>...)`, where `T`  is one of  ``S`` (string),
%    ``A`` (atom), ``I`` (integer), ``F`` (float), ``N`` (number), ``T``
%    (term)  and ``R``  (range).  In the  current implementation  ``I``,
%    ``F`` and  ``N`` are synonyms  for ``T``.  Future versions  may act
%    different if the parsed value is not of the requested numeric type.
%
%    Note that re_compile/3 does not support the Pattern/Flags form that
%    is supported by re_match/3, re_replace/4, etc.; the Pattern must be
%    text and all compile options specified in Options.

%!  re_compiled(+Spec, --Regex, +Options) is det.
%
%   Create  a  compiled regex  from  a  specification.  Cached  compiled
%   regular expressions  can be  reclaimed using re_flush/0  (which also
%   removes   "compiled"   With    arguments   from   re_replace/4   and
%   re_replace/5).

:- table re_compiled_/4 as shared.

re_compiled(RegexIn, Regex, Options) :-
    (   blob(RegexIn, regex)
    ->  Regex = RegexIn
    ;   RegexIn = Text/Flags
    ->  re_compiled_(Text, Flags, Regex, Options)
    ;   re_compiled_(RegexIn, '', Regex, Options)
    ).

re_compiled_(Text, Flags, Regex, Options) =>
    must_be(text, Text),
    must_be(atom, Flags),
    re_flags_options(Flags, Options0),
    append(Options0, Options, Options2),
    re_compile(Text, Regex, Options2).

re_flags_options(Flags, Options) :-
    atom_chars(Flags, Chars),
    maplist(re_flag_option, Chars, Options).

re_flag_option(Flag, Option) :-
    re_flag_option_(Flag, Option),
    !.
re_flag_option(Flag, _) :-
    existence_error(re_flag, Flag).

re_flag_option_(i, caseless(true)).
re_flag_option_(m, multiline(true)).
re_flag_option_(x, extended(true)).
re_flag_option_(s, dotall(true)).
re_flag_option_(a, capture_type(atom)).
re_flag_option_(r, capture_type(range)).
re_flag_option_(t, capture_type(term)).

%!  re_flush
%
%   Clean pattern and replacement caches.
%
%   @tbd Flush automatically if the cache becomes too large.

re_flush :-
    abolish_module_tables(pcre).

%!  re_config(+Term)
%
%   Extract configuration information from the  pcre library. Term is of
%   the   form    ``Name(Value)``.    Name    is   derived    from   the
%   ``PCRE_CONFIG_*``  constant  after   removing  ``PCRE_CONFIG_``  and
%   mapping the name to  lower case, e.g.  `utf8`, `unicode_properties`,
%   etc.  Value is a Prolog boolean, integer, or atom. For boolean (1 or
%   0) values, `true` or `false` is returned.
%
%   re_config/1 will  backtrack through  all the  possible configuration
%   values  if its  argument  is a  variable. If  an  unknown option  is
%   specified, re_config/1 fails.
%
%   Non-compatible  changes  between  PCRE1 and  PCRE2  because  numeric
%   values changed: `bsr` and `newline` have been replaced by `bsr2` and
%   `newline2`:
%     * `bsr2` - previously `bsr` returned 0 or 1; now returns `unicode`
%       or `anycrlf`
%     * `newline2`  -  previously  `newline` returned  an  integer,  now
%       returns `cr`, `lf`, `crlf`, `any`, `anycrlf`, `nul`
%
%  Term values are as follows. Some values might not exist, depending on
%  the version of PCRE2 and the options it was built with.
%
%   * bsr2
%     The character  sequences that the `\R` escape sequence  matches by
%     default. Replaces `bsr` option from PCRE1, which is not compatible.
%   * compiled_widths
%     An integer whose  lower bits indicate which code  unit widths were
%     selected when PCRE2 was built.  The 1-bit indicates 8-bit support,
%     and  the  2-bit and  4-bit  indicate  16-bit and  32-bit  support,
%     respectively. The 1  bit should always be set  because the wrapper
%     code requires 8 bit support.
%   * depthlimit
%   * heaplimit
%   * jit
%     `true` if just-in-time compiling is available.
%   * jittarget
%     A string containing the name of the architecture for which the JIT
%     compiler is configured. e.g., 'x86 64bit (little endian + unaligned)'.
%   * linksize
%   * matchlimit
%   * never_backslash_c
%   * newline2
%     An atom whose value specifies  the default character sequence that
%     is  recognized as  meaning "newline"  (`cr`, `lf`,  `crlf`, `any`,
%     `anycrlf`, `nul`).  Replaces `newline` option from PCRE1, which is
%     not compatible.
%   * parenslimit
%   * stackrecurse
%   * unicode
%     Always `true`
%   * unicode_version
%     The unicode version as an atom, e.g. '12.1.0'.
%   * utf8 - synonym for `unicode`
%   * parens_limit
%   * version
%   The  version information  as an  atom, containing  the PCRE  version
%   number and release date, e.g. '10.34 2019-11-21'.
%
%   For backwards compatibility with  PCRE1, the following are accepted,
%   but are deprecated:
%     * `utf8` - synonym for `unicode`
%     * `link_size` - synonym for `linksize`
%     * `match_limit` - synonym for `matchlimit`
%     * `parens_limit` - synonym for `parenslimit`
%     * `unicode_properties` - always true
%   The following  have been removed  because they don't exist  in PCRE2
%   and don't seem to have any meaningful use in PCRE1:
%     * `posix_malloc_threshold`
%     * `match_limit_recursion`

%   @see `man pcre2api` for details

re_config(Term), var(Term) =>
    re_config_choice(Term),
    % This code depends on re_config_/1 failing if it's given an invalid
    % Term (e.g., re_config_(jittarget(_)) fails if jit(false)). If
    % re_config_/1 is changed to throw an error, then the following call
    % needs to be inside catch/3.
    re_config_(Term).
re_config(Term) =>
    re_config_(Term).
