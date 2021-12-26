/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2017, VU University Amsterdam
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
            re_matchsub/4,        % +Regex, +String, -Subs, +Options
            re_foldl/6,           % :Goal, +Regex, +String, ?V0, ?V, +Options
            re_split/3,		  % +Pattern, +String, -Split:list
            re_split/4,		  % +Pattern, +String, -Split:list, +Options
            re_replace/4,	  % +Pattern, +With, +String, -NewString

            re_compile/3,         % +Pattern, -Regex, +Options
            re_flush/0,
            re_config/1           % ?Config
          ]).
:- autoload(library(apply),[maplist/3]).
:- autoload(library(error),[must_be/2,existence_error/2]).
:- autoload(library(dcg/basics),[string/3,eos/2,digit/3,digits/3]).

:- use_foreign_library(foreign(pcre4pl)).

:- meta_predicate
    re_foldl(3, +, +, ?, ?, +).

/** <module> Perl compatible regular expression matching for SWI-Prolog

This module provides an interface   to  the [PCRE](http://www.pcre.org/)
(Perl Compatible Regular Expression)  library.   This  Prolog  interface
provides an almost comprehensive wrapper around PCRE.

Regular  expressions  are  created  from  a   pattern  and  options  and
represented as a SWI-Prolog _blob_.  This   implies  they are subject to
(atom) garbage collection. Compiled regular   expressions  can safely be
used in multiple threads. Most  predicates   accept  both  an explicitly
compiled regular expression, a pattern or   a term Pattern/Flags. In the
latter two cases a regular expression _blob_  is created and stored in a
cache. The cache can be cleared using re_flush/0.

@see `man pcre` for details.
*/

:- predicate_options(re_match/3, 3,
                     [ anchored(boolean),
                       bol(boolean),
                       bsr(oneof([anycrlf,unicode])),
                       empty(boolean),
                       empty_atstart(boolean),
                       eol(boolean),
                       newline(oneof([any,anycrlf,cr,lf,crlf])),
                       start(integer)
                     ]).
:- predicate_options(re_compile/3, 3,
                     [ anchored(boolean),
                       bsr(oneof([anycrlf,unicode])),
                       caseless(boolean),
                       dollar_endonly(boolean),
                       dotall(boolean),
                       dupnames(boolean),
                       extended(boolean),
                       extra(boolean),
                       firstline(boolean),
                       compat(oneof([javascript])),
                       multiline(boolean),
                       newline(oneof([any,anycrlf,cr,lf,crlf])),
                       ucp(boolean),
                       ungreedy(boolean)
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
%   Options:
%
%     * anchored(Bool)
%     If =true=, match only at the first position
%     * bol(Bool)
%     Subject string is the beginning of a line (default =false=)
%     * bsr(Mode)
%     If =anycrlf=, \R only matches CR, LF or CRLF.  If =unicode=,
%     \R matches all Unicode line endings.
%     Subject string is the end of a line (default =false=)
%     * empty(Bool)
%     An empty string is a valid match (default =true=)
%     * empty_atstart(Bool)
%     An empty string at the start of the subject is a valid match
%     (default =true=)
%     * eol(Bool)
%     Subject string is the end of a line (default =false=)
%     * newline(Mode)
%     If =any=, recognize any Unicode newline sequence,
%     if =anycrlf=, recognize CR, LF, and CRLF as newline
%     sequences, if =cr=, recognize CR, if =lf=, recognize
%     LF and finally if =crlf= recognize CRLF as newline.
%     * start(+From)
%     Start at the given character index
%
%   @arg Regex is the output  of  re_compile/3,   a  pattern  or  a term
%   Pattern/Flags, where Pattern is an atom or string. The defined flags
%   and there related option for re_compile/3 are below.
%
%     - *x*: extended(true)
%     - *i*: caseless(true)
%     - *m*: multiline(true)
%     - *s*: dotall(true)
%     - *a*: capture_type(atom)
%     - *r*: capture_type(range)
%     - *t*: capture_type(term)

re_match(Regex, String) :-
    re_match(Regex, String, []).
re_match(Regex, String, Options) :-
    re_compiled(Regex, Compiled),
    re_match_(Compiled, String, Options).

%!  re_matchsub(+Regex, +String, -Sub:dict, +Options) is semidet.
%
%   Match String against Regex. On  success,   Sub  is a dict containing
%   integer keys for the numbered capture group   and  atom keys for the
%   named capture groups. The associated  value   is  determined  by the
%   capture_type(Type) option passed to re_compile/3,   may be specified
%   using flags if Regex  is  of  the   form  Pattern/Flags  and  may be
%   specified at the  level  of  individual   captures  using  a  naming
%   convention for the caption name. See re_compile/3 for details.
%
%   The example below  exploits  the  typed   groups  to  parse  a  date
%   specification:
%
%     ```
%     ?- re_matchsub("(?<date> (?<year_I>(?:\\d\\d)?\\d\\d) -
%                     (?<month_I>\\d\\d) - (?<day_I>\\d\\d) )"/e,
%                    "2017-04-20", Sub, []).
%     Sub = re_match{0:"2017-04-20", date:"2017-04-20",
%                    day:20, month:4, year:2017}.
%
%     ```
%
%   @arg Options Only _execution_ options are processed.  See re_match/3
%   for the set of options.  _Compilation_ options must be passed as
%   `/flags` to Regex.
%   @arg Regex  See re_match/2 for a description of this argument.

re_matchsub(Regex, String, Subs, Options) :-
    re_compiled(Regex, Compiled),
    re_matchsub_(Compiled, String, Pairs, Options),
    dict_pairs(Subs, re_match, Pairs).

%!  re_foldl(:Goal, +Regex, +String, ?V0, ?V, +Options) is semidet.
%
%   _Fold_ all matches of Regex on String.  Each match is represented by
%   a dict as specified for re_matchsub/4. V0  and V are related using a
%   sequence of invocations of Goal as illustrated below.
%
%	```
%       call(Goal, Dict1, V0, V1),
%       call(Goal, Dict2, V1, V2),
%       ...
%       call(Goal, Dictn, Vn, V).
%       ```
%
%   This predicate is used to implement re_split/4 and re_replace/4. For
%   example, we can count all matches of   a  Regex on String using this
%   code:
%
%     ```
%     re_match_count(Regex, String, Count) :-
%         re_foldl(increment, Regex, String, 0, Count, []).
%
%     increment(_Match, V0, V1) :-
%	  V1 is V0+1.
%     ```
%
%   After which we can query
%
%     ```
%     ?- re_match_count("a", "aap", X).
%     X = 2.
%     ```

re_foldl(Goal, Regex, String, V0, V, Options) :-
    re_compiled(Regex, Compiled),
    re_foldl_(Compiled, String, Goal, V0, V, Options).

:- public re_call_folder/4.

re_call_folder(Goal, Pairs, V0, V1) :-
    dict_pairs(Dict, re_match, Pairs),
    call(Goal, Dict, V0, V1).


%!  re_split(+Pattern, +String, -Split:list) is det.
%!  re_split(+Pattern, +String, -Split:list, +Options) is det.
%
%   Split String using the regular expression   Pattern. Split is a list
%   of strings holding alternating matches of  Pattern and skipped parts
%   of the String, starting with a skipped   part.  The Split lists ends
%   with a string of the content  of   String  after  the last match. If
%   Pattern does not appear in String, Split is a list holding a copy of
%   String. This implies the number  of   elements  in Split is _always_
%   odd.  For example:
%
%     ```
%     ?- re_split("a+", "abaac", Split, []).
%     Split = ["","a","b","aa","c"].
%     ?- re_split(":\\s*"/n, "Age: 33", Split, []).
%     Split = ['Age', ': ', 33].
%     ```
%
%   @arg Pattern is the pattern  text,   optionally  follows  by /Flags.
%   Similar to re_matchsub/4, the final output type can be controlled by
%   a flag =a= (atom), =s= (string, default) or =n= (number if possible,
%   atom otherwise).

re_split(Pattern, String, Split) :-
    re_split(Pattern, String, Split, []).
re_split(Pattern, String, Split, Options) :-
    range_regex(Pattern, Compiled, Type),
    State = state(String, 0, Type),
    re_foldl(split(State), Compiled, String, Split, [Last], Options),
    arg(2, State, LastSkipStart),
    typed_sub(Type, String, LastSkipStart, _, 0, Last).

range_regex(Pattern/Flags, Compiled, Type) :- !,
    atom_chars(Flags, Chars),
    replace_flags(Chars, Chars1, Type),
    atom_chars(RFlags, [r|Chars1]),
    re_compiled(Pattern/RFlags, Compiled).
range_regex(Pattern, Compiled, string) :-
    re_compiled(Pattern/r, Compiled).

replace_flags([], [], Type) :-
    default(Type, string).
replace_flags([H|T0], T, Type) :-
    split_type(H, Type),
    !,
    replace_flags(T0, T, Type).
replace_flags([H|T0], [H|T], Type) :-
    replace_flags(T0, T, Type).

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

%!  re_replace(+Pattern, +With, +String, -NewString)
%
%   Replace matches of the regular  expression   Pattern  in String with
%   With. With may reference captured substrings using \N or $Name. Both
%   N and Name may be written as {N} and {Name} to avoid ambiguities.
%
%   @arg Pattern is the pattern  text,   optionally  follows  by /Flags.
%   Flags may include `g`,  replacing  all   occurences  of  Pattern. In
%   addition, similar to re_matchsub/4, the  final   output  type can be
%   controlled by a flag =a= (atom) or =s= (string, default).

re_replace(Pattern, With, String, NewString) :-
    range_regex(Pattern, Compiled, All, Type),
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

range_regex(Pattern/Flags, Compiled, All, Type) :- !,
    atom_chars(Flags, Chars),
    replace_flags(Chars, Chars1, All, Type),
    atom_chars(RFlags, [r|Chars1]),
    re_compiled(Pattern/RFlags, Compiled).
range_regex(Pattern, Compiled, first, string) :-
    re_compiled(Pattern/r, Compiled).

replace_flags([], [], All, Type) :-
    default(All, first),
    default(Type, string).
replace_flags([H|T0], T, All, Type) :-
    (   all(H, All)
    ->  true
    ;   type(H, Type)
    ),
    !,
    replace_flags(T0, T, All, Type).
replace_flags([H|T0], [H|T], All, Type) :-
    replace_flags(T0, T, All, Type).

all(g, all).
type(a, atom).
type(s, string).

default(Val, Val) :- !.
default(_, _).

replace(State, With, Dict, [Skipped|Parts], T) :-
    State = state(String, _, _Type),
    copy_term(With, r(PartsR, Skel)),
    Skel :< Dict,
    range_strings(PartsR, String, Parts, T),
    skipped(State, Dict.0, Skipped).

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
%   Compile the replacement specification into  a specification that can
%   be processed quickly. The compiled expressions are cached and may be
%   reclaimed using re_flush/0.

:- dynamic replacement_cache/2.
:- volatile replacement_cache/2.

compile_replacement(With, Compiled) :-
    replacement_cache(With, Compiled),
    !.
compile_replacement(With, Compiled) :-
    compile_replacement_nocache(With, Compiled),
    assertz(replacement_cache(With, Compiled)).

compile_replacement_nocache(With, r(Parts, Extract)) :-
    string_codes(With, Codes),
    phrase(replacement_parts(Parts, Pairs), Codes),
    dict_pairs(Extract, _, Pairs).

replacement_parts(Parts, Extract) -->
    string(HCodes),
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
%   Compiles Pattern to a Regex _blob_ of type =regex= (see blob/2).
%   Defined Options are  defined  below.   Please  consult  the PCRE
%   documentation for details.
%
%     * anchored(Bool)
%     Force pattern anchoring
%     * bsr(Mode)
%     If =anycrlf=, \R only matches CR, LF or CRLF.  If =unicode=,
%     \R matches all Unicode line endings.
%     * caseless(Bool)
%     If =true=, do caseless matching.
%     * dollar_endonly(Bool)
%     If =true=, $ not to match newline at end
%     * dotall(Bool)
%     If =true=, . matches anything including NL
%     * dupnames(Bool)
%     If =true=, allow duplicate names for subpatterns
%     * extended(Bool)
%     If =true=, ignore white space and # comments
%     * extra(Bool)
%     If =true=, PCRE extra features (not much use currently)
%     * firstline(Bool)
%     If =true=, force matching to be before newline
%     * compat(With)
%     If =javascript=, JavaScript compatibility
%     * multiline(Bool)
%     If =true=, ^ and $ match newlines within data
%     * newline(Mode)
%     If =any=, recognize any Unicode newline sequence,
%     if =anycrlf= (default), recognize CR, LF, and CRLF as newline
%     sequences, if =cr=, recognize CR, if =lf=, recognize
%     LF and finally if =crlf= recognize CRLF as newline.
%     * ucp(Bool)
%     If =true=, use Unicode properties for \d, \w, etc.
%     * ungreedy(Bool)
%     If =true=, invert greediness of quantifiers
%
%   In addition to the options above that directly map to pcre flags the
%   following options are processed:
%
%     * optimize(Bool)
%     If `true`, _study_ the regular expression.
%     * capture_type(+Type)
%     How to return the matched part of the input and possibly captured
%     groups in there.  Possible values are:
%       - string
%       Return the captured string as a string (default).
%       - atom
%       Return the captured string as an atom.
%       - range
%       Return the captured string as a pair `Start-Length`.  Note
%       the we use ``Start-Length` rather than the more conventional
%       `Start-End` to allow for immediate use with sub_atom/5 and
%       sub_string/5.
%       - term
%       Parse the captured string as a Prolog term.  This is notably
%       practical if you capture a number.
%
%    The `capture_type` specifies the  default   for  this  pattern. The
%    interface supports a different type for   each  _named_ group using
%    the syntax =|(?<name_T>...)|=, where =T= is   one  of =S= (string),
%    =A= (atom), =I= (integer), =F= (float),   =N=  (number), =T= (term)
%    and =R= (range). In the current implementation =I=, =F= and =N= are
%    synonyms for =T=. Future versions may   act different if the parsed
%    value is not of the requested numeric type.

%!  re_compiled(+Spec, --Regex) is det.
%
%   Create a compiled regex from a specification.  Cached compiled
%   regular expressions can be reclaimed using re_flush/0.

:- dynamic re_pool/3.
:- volatile re_pool/3.

re_compiled(Regex, Regex) :-
    blob(Regex, regex),
    !.
re_compiled(Text/Flags, Regex) :-
    must_be(text, Text),
    must_be(atom, Flags),
    re_pool(Text, Flags, Regex),
    !.
re_compiled(Text/Flags, Regex) :-
    !,
    re_flags_options(Flags, Options),
    re_compile(Text, Regex, Options),
    assertz(re_pool(Text, Flags, Regex)).
re_compiled(Text, Regex) :-
    must_be(text, Text),
    re_pool(Text, '', Regex),
    !.
re_compiled(Text, Regex) :-
    re_compiled(Text/'', Regex).

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
    retractall(replacement_cache(_,_)),
    retractall(re_pool(_,_,_)).

%!  re_config(+Term)
%
%   Extract configuration information from the pcre  library. Term is of
%   the form Name(Value). Name  is   derived  from the =|PCRE_CONFIG_*|=
%   constant after removing =PCRE_CONFIG_= and mapping the name to lower
%   case, e.g. `utf8`, `unicode_properties`,  etc.   Value  is  either a
%   Prolog boolean, integer or atom.
%
%   Finally, the functionality of pcre_version()  is available using the
%   configuration name `version`.
%
%   @see `man pcreapi` for details
