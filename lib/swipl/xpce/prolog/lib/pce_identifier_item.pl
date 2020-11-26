/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  2003-2013, University of Amsterdam
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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The class identifier_item is  a  subclass   of  text_item  for  entering
identifiers. Its main task is not to allow for empty identifiers, handle
different case and white-space regimes.

Case regimes supported are:

        sensitive*              Identifiers are casesensitive
        upper                   Identifiers are mapped to upper-case
        lower                   Identifiers are mapped to lower-case

white-space regimes are:

        accept                  Don't change
        stripped                Delete leading and trailing white space
        canonicalise*           As stripped and make all internal white
                                space exactly one space-character
        <a character>           As canonicalise, but pass spaces as this
                                character.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


:- module(pce_identifier_item, []).
:- use_module(library(pce)).

:- pce_begin_class(identifier_item, text_item,
                   "Item for non-empty, canonicalised word").

variable(case,  {sensitive,upper,lower} := sensitive,
         both, "Case mapping").
variable(blank, '{accept,stripped,canonicalise}|char' := canonicalise,
         both, "How to handle blank space").

selection(II, Selection:name) :<-
    "Get selection and canonicalise"::
    get(II, get_super, selection, Name0),
    get(II, case, CaseMap),
    canonicalise_case(CaseMap, Name0, Name1),
    get(II, blank, BlankMap),
    canonicalise_blanks(BlankMap, Name1, Selection),
    (   Selection == ''
    ->  get(II?name, label_name, Label),
        send(II, error, item_not_filled, Label),
        fail
    ;   true
    ).

canonicalise_case(upper, Name, Upper) :-
    !,
    get(Name, upcase, Upper).
canonicalise_case(lower, Name, Upper) :-
    !,
    get(Name, downcase, Upper).
canonicalise_case(_, Name, Name).

canonicalise_blanks(canonicalise, Name0, Name) :-
    !,
    get(Name0, strip, canonicalise, Name).
canonicalise_blanks(stripped, Name0, Name) :-
    !,
    get(Name0, strip, both, Name).
canonicalise_blanks(Mapped, Name0, Name) :-
    integer(Mapped),
    !,
    get(Name0, strip, canonicalise, Name1),
    new(S, string('%s', Name1)),
    send(S, translate, ' ', Mapped),
    get(S, value, Name),
    free(S).
canonicalise_blanks(_, Name, Name).

%typed(II, Key:event_id) :->
%       "Properly handle completion"::
%       (   Key == 32                   % Space: completion
%       ->  true
%       ;   send(II, send_super, typed, Key)
%       ).

:- pce_end_class(identifier_item).


:- initialization
   new(_, error(item_not_filled, '%I%s: No value', error, report)).
