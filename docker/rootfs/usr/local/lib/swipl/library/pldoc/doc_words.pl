/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2018, VU University Amsterdam
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

:- module(doc_words,
          [ doc_related_word/3,         % +Term, -Related, -Distance
            prolog_identifier_part/1   % ?Part
          ]).
:- use_module(library(lists)).
:- use_module(library(atom)).

:- use_module(man_index).

/** <module> Reason about Prolog jargon

This  module  is  shared  between  PlDoc   and  apropos/1  and  collects
information about the Prolog jargon to   enhance recall and precision of
the search. One could call it an ontology, but I guess that is a bit too
much honour.
*/

:- multifile
    prolog:doc_object_summary/4.

%!  doc_related_word(+Term, -Related, -Distance) is nondet.
%
%   True when Related is a  word  that   is  related  to  Term. Map some
%   commonly known concepts to their Prolog   related  term. Where do we
%   find a comprehensive list of these?

doc_related_word(In, Out, Distance) :-
    related(In, Out, 1, Distance, [In]).

related(T, T, D, D, _).
related(In, Out, D0, D, Visited) :-
    (   synonym(In, Out0, D1)
    ;   synonym(Out0, In, D1)
    ),
    \+ memberchk(Out0, Visited),
    D2 is D0*D1,
    D2 > 0.2,
    related(Out0, Out, D2, D, [Out0|Visited]).

synonym(abs,       absolute,    0.7).
synonym(add,       append,      0.3).
synonym(add,       assert,      0.3).
synonym(append,    concatenate, 0.5).
synonym(arg,       argument,	0.8).
synonym(argument,  parameter,   0.7).
synonym(at,        on,		0.3).
synonym(cancel,    stop,	0.3).
synonym(ceil,      ceiling,     0.7).
synonym(char,      character,   0.8).
synonym(clone,     duplicate,   0.3).
synonym(close,     destroy,     0.3).
synonym(comma,	   conjunction, 0.3).
synonym(concat,    concatenate, 0.8).
synonym(console,   terminal,    0.7).
synonym(consult,   compile,     0.7).
synonym(cos,       cosine,      0.9).
synonym(create,    clone,       0.3).
synonym(create,    fork,        0.3).
synonym(create,    make,        0.3).
synonym(create,    new,         0.3).
synonym(del,       delete,	0.7).
synonym(delete,    remove,      0.3).
synonym(delete,    unregister,  0.3).
synonym(destroy,   delete,      0.3).
synonym(destroy,   unregister,  0.3).
synonym(dir,       directory,	0.7).
synonym(div,       divide,	0.5).
synonym(elem,      element,     0.7).
synonym(element,   member,      0.3).
synonym(eq,        equal,       0.5).
synonym(equal,     equivalent,  0.9).
synonym(error,     catch,       0.5).
synonym(error,     exception,   0.8).
synonym(error,     throw,       0.5).
synonym(eval,      evaluate,    0.7).
synonym(exec,	   execute,     0.7).
synonym(exit,      halt,        0.9).
synonym(fast,      quick,       0.7).
synonym(file,      srcdest,     0.9).
synonym(file,      stream,      0.3).
synonym(find,      search,      0.7).
synonym(float,     double,      0.7).
synonym(folder,    directory,	0.7).
synonym(function,  procedure,   0.3).
synonym(http,      https,       0.3).
synonym(http,      url,         0.3).
synonym(inf,       infinite,    0.7).
synonym(int,	   integer,     0.8).
synonym(larger,    greater,     0.5).
synonym(larger,    higher,      0.5).
synonym(load,      compile,     0.3).
synonym(lock,      mutex,	0.5).
synonym(log,       logarithmic, 0.9).
synonym(max,       maximum,     0.8).
synonym(min,       minimum,     0.5).
synonym(min,       minus,       0.5).
synonym(mod,	   module,      0.3).
synonym(mod,	   modulo,      0.3).
synonym(mul,       multiply,	0.5).
synonym(param,     parameter,	0.8).
synonym(perm,      permutation, 0.5).
synonym(pred,      predicate,   0.7).
synonym(predicate, procedure,   0.8).
synonym(quit,      halt,        0.9).
synonym(rand,      random,      0.5).
synonym(real,      float,       0.3).
synonym(rem,       remainder,   0.3).
synonym(remove,    abolish,     0.5).
synonym(remove,    retract,     0.5).
synonym(remove,    unload,      0.3).
synonym(rm,        remove,	0.7).
synonym(run,       call,        0.3).
synonym(same,      equivalent,  0.8).
synonym(screen,    console,     0.5).
synonym(semincolon,disjunction, 0.3).
synonym(sin,       sine,        0.9).
synonym(size,      memory,      0.3).
synonym(smaller,   less,        0.5).
synonym(smaller,   lower,       0.5).
synonym(sqrt,      root,        0.5).
synonym(sqrt,      square,      0.5).
synonym(ssl,       tls,         0.9).
synonym(tan,       tangent,     0.9).
synonym(task,	   process,     0.3).
synonym(task,	   thread,      0.3).
synonym(temp,      temporary,   0.7).
synonym(tmp,       temporary,   0.7).
synonym(tty,	   terminal,    0.7).
synonym(unzip,     decompress,  0.3).
synonym(zip,	   compress,    0.3).

:- dynamic
    prolog_id_part_cached/0,
    cached_prolog_id_part/1.

%!  prolog_identifier_part(?Part) is nondet.
%
%   True if Part appears  as  part  of   a  Prolog  identifier  from the
%   documentation. This extracts most of the Prolog jargon.

prolog_identifier_part(Part) :-
    prolog_id_part_cached,
    !,
    cached_prolog_id_part(Part).
prolog_identifier_part(Part) :-
    with_mutex(doc_search,
               make_prolog_id_part_cache),
    cached_prolog_id_part(Part).

make_prolog_id_part_cache :-
    prolog_id_part_cached,
    !.
make_prolog_id_part_cache :-
    findall(Part, prolog_id_part_no_cache(Part), Parts),
    sort(Parts, Unique),
    forall(member(U, Unique),
           assertz(cached_prolog_id_part(U))),
    asserta(prolog_id_part_cached).

prolog_id_part_no_cache(Part) :-
    prolog:doc_object_summary(Obj, _, _, _),
    doc_object_identifier(Obj, Identifier),
    identifier_parts(Identifier, Parts),
    member(Part, Parts).

