/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2015, VU University Amsterdam
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

:- module(malloc_info,
          [
          ]).
:- autoload(library(apply),[maplist/3,partition/4]).
:- autoload(library(lists),[selectchk/3]).
:- autoload(library(sgml),[load_xml/3]).

:- use_foreign_library(foreign(mallocinfo)).

/** <module> Memory allocation details

This library is provided if the clib   package  is compiled on a _glibc_
based system, typically Linux. It provides  access to the glibc ptmalloc
informational  functions  for  diagnosing  memory  usage.  This  library
exports

  * mallinfo/1
  * malloc_info/1
*/

:- if(current_predicate('$mallinfo'/1)).
:- export(mallinfo/1).

%!  mallinfo(-Info:dict) is det.
%
%   Return the content  of  the   =|struct  mallinfo|=  returned  by
%   =|mallinfo()|= as a dict. See =|man mallinfo|= for an
%   explanation of the fields.
%
%   @bug    The =|struct mallinfo|= contains =int= fields and is thus
%           incapable of expressing the memory sizes of 64-bit
%           machines.  The fields are interpreted as _unsigned_ and
%           thus represent the true value modulo 2**32 (4Gb).

mallinfo(Info) :-
    '$mallinfo'(List),
    dict_create(Info, malinfo, List).
:- endif.

:- if(current_predicate('$malloc_info'/1)).
:- export(malloc_info/1).

%!  malloc_info(-Info:dict) is det.
%
%   Interface to =|malloc_info()|=, which provides   an XML document
%   describing the status of the   GNU  glibc malloc implementation.
%   The XML document is parsed and  translated   into  a dict with a
%   similar structure. The  malloc_info()  XML   is  supposed  to be
%   self-explanatory.
%
%   @see [Understanding glibc malloc](https://sploitfun.wordpress.com/2015/02/10/understanding-glibc-malloc/)

malloc_info(Info) :-
    '$malloc_info'(XML),
    setup_call_cleanup(
        open_string(XML, In),
        load_xml(In, DOM, [space(remove)]),
        close(In)),
    malloc_dom_prolog(DOM, Info).

malloc_dom_prolog([element(malloc, _, DOM)], Info) :-
    maplist(malloc_prolog, DOM, List),
    partition(is_dict, List, Heaps, Rest),
    dict_create(Info, malloc, [heaps:Heaps|Rest]).

malloc_prolog(element(heap, [nr=NRA], DOM), Heap) :-
    !,
    atom_number(NRA, NR),
    maplist(heap_prolog, DOM, HeapProperties),
    dict_create(Heap, heap, [nr-NR|HeapProperties]).
malloc_prolog(Element, Pair) :-
    misc_field(Element, Pair).

heap_prolog(element(sizes, _, DOM), sizes-Sizes) :-
    !,
    maplist(chunk_size, DOM, Sizes).
heap_prolog(Element, Pair) :-
    misc_field(Element, Pair).

misc_field(element(Name, Attrs0, []), Key-Value) :-
    selectchk(type=Type, Attrs0, Attrs1),
    atomic_list_concat([Name, '_', Type], Key),
    maplist(attr_value, Attrs1, Attrs),
    (   Attrs = [_=Value]
    ->  true
    ;   dict_create(Value, Name, Attrs)
    ).

chunk_size(element(size, Attrs0, []), Dict) :-
    !,
    maplist(attr_value, Attrs0, Attrs),
    dict_create(Dict, size, Attrs).
chunk_size(element(unsorted, Attrs0, []), Dict) :-
    maplist(attr_value, Attrs0, Attrs),
    dict_create(Dict, unsorted, Attrs).

attr_value(Name=In, Name=Out) :-
    atom_number(In, Out),
    !.
attr_value(Name=In, Name=Out) :-
    atom_string(In, Out),
    !.

:- endif.
