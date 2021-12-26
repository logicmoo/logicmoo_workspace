/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2013-2015, VU University Amsterdam
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

:- module(rdf_ntriples,
          [ rdf_read_ntriples/3,        % +Input, -Triples, +Options
            rdf_read_nquads/3,          % +Input, -Quads, +Options
            rdf_process_ntriples/3,     % +Input, :CallBack, +Options

            read_ntriple/2,             % +Stream, -Triple
            read_nquad/2,               % +Stream, -Quad
            read_ntuple/2               % +Stream, -TripleOrQuad
          ]).
:- use_module(library(semweb/rdf_db),
              [rdf_transaction/2,rdf_set_graph/2,rdf_assert/4]).
:- use_module(library(record),[(record)/1, op(_,_,record)]).

:- autoload(library(error),[domain_error/2]).
:- autoload(library(memfile),
	    [atom_to_memory_file/2,open_memory_file/4]).
:- autoload(library(option),[option/3,option/2]).
:- autoload(library(uri),
	    [uri_file_name/2,uri_is_global/1,uri_normalized/2]).
:- autoload(library(http/http_open),[http_open/3]).

:- use_foreign_library(foreign(ntriples)).

/** <module> Process files in the RDF N-Triples format

The library(semweb/rdf_ntriples) provides a  fast   reader  for  the RDF
N-Triples and N-Quads format. N-Triples is   a simple format, originally
used to support the W3C RDF  test   suites.  The current format has been
extended   and   is   a   subset    of     the    Turtle   format   (see
library(semweb/turtle)).

The API of this library is   almost identical to library(semweb/turtle).
This module provides a plugin  into   rdf_load/2,  making this predicate
support the format =ntriples= and =nquads=.

@see http://www.w3.org/TR/n-triples/
@tbd Sync with RDF 1.1. specification.
*/

:- predicate_options(rdf_read_ntriples/3, 3,
                     [ anon_prefix(any), % atom or node(_)
                       base_uri(atom),
                       error_count(-integer),
                       on_error(oneof([warning,error]))
                     ]).
:- predicate_options(rdf_read_nquads/3, 3,
                     [ anon_prefix(any), % atom or node(_)
                       base_uri(atom),
                       error_count(-integer),
                       on_error(oneof([warning,error])),
                       graph(atom)
                     ]).
:- predicate_options(rdf_process_ntriples/3, 3,
                     [ graph(atom),
                       pass_to(rdf_read_ntriples/3, 3)
                     ]).

:- meta_predicate
    rdf_process_ntriples(+,2,+).


%!  read_ntriple(+Stream, -Triple) is det.
%
%   Read the next triple from Stream as Triple. Stream must have UTF-8
%   encoding.
%
%   @param  Triple is a term triple(Subject,Predicate,Object).
%           Arguments follow the normal conventions of the RDF
%           libraries.  NodeID elements are mapped to node(Id).
%           If end-of-file is reached, Triple is unified with
%           =end_of_file=.
%   @error  syntax_error(Message) on syntax errors

%!  read_nquad(+Stream, -Quad) is det.
%
%   Read the next quad from Stream as Quad.  Stream must have UTF-8
%   encoding.
%
%   @param  Quad is a term quad(Subject,Predicate,Object,Graph).
%           Arguments follow the normal conventions of the RDF
%           libraries.  NodeID elements are mapped to node(Id).
%           If end-of-file is reached, Quad is unified with
%           =end_of_file=.
%   @error  syntax_error(Message) on syntax errors

%!  read_ntuple(+Stream, -Tuple) is det.
%
%   Read the next triple or quad from  Stream as Tuple. Tuple is one
%   of the terms below.  See   read_ntriple/2  and  read_nquad/2 for
%   details.
%
%     - triple(Subject,Predicate,Object)
%     - quad(Subject,Predicate,Object,Graph).

:- record nt_state(anon_prefix,
               graph,
               on_error:oneof([warning,error])=warning,
               format:oneof([ntriples,nquads]),
               error_count=0).


%!  rdf_read_ntriples(+Input, -Triples, +Options) is det.
%!  rdf_read_nquads(+Input, -Quads, +Options) is det.
%
%   True when Triples/Quads is a list   of triples/quads from Input.
%   Options:
%
%     * anon_prefix(+AtomOrNode)
%     Prefix nodeIDs with this atom.  If AtomOrNode is the term
%     node(_), bnodes are returned as node(Id).
%     * base_uri(+Atom)
%     Defines the default anon_prefix as _:<baseuri>_
%     * on_error(Action)
%     One of =warning= (default) or =error=
%     * error_count(-Count)
%     If =on_error= is =warning=, unify Count with th number of
%     errors.
%     * graph(+Graph)
%     For rdf_read_nquads/3, this defines the graph associated
%     to _triples_ loaded from the input.  For rdf_read_ntriples/3
%     this opion is ignored.
%
%   @arg Triples is a list of rdf(Subject, Predicate, Object)
%   @arg Quads is a list of rdf(Subject, Predicate, Object, Graph)

rdf_read_ntriples(Input, Triples, Options) :-
    rdf_read_ntuples(Input, Triples, [format(ntriples)|Options]).

rdf_read_nquads(Input, Triples, Options) :-
    rdf_read_ntuples(Input, Triples, [format(nquads)|Options]).


rdf_read_ntuples(Input, Triples, Options) :-
    setup_call_cleanup(
        open_input(Input, Stream, Close),
        (   init_state(Input, Options, State0),
            read_ntuples(Stream, Triples, State0, State)
        ),
        Close),
    option(error_count(Count), Options, _),
    nt_state_error_count(State, Count).

%!  rdf_process_ntriples(+Input, :CallBack, +Options)
%
%   Call-back interface, compatible with the   other triple readers.
%   In  addition  to  the  options  from  rdf_read_ntriples/3,  this
%   processes the option graph(Graph).
%
%   @param  CallBack is called as call(CallBack, Triples, Graph),
%           where Triples is a list holding a single rdf(S,P,O)
%           triple.  Graph is passed from the =graph= option and
%           unbound if this option is omitted.

rdf_process_ntriples(Input, CallBack, Options) :-
    setup_call_cleanup(
        open_input(Input, Stream, Close),
        (   init_state(Input, Options, State0),
            process_ntriple(Stream, CallBack, State0, State)
        ),
        Close),
    option(error_count(Count), Options, _),
    nt_state_error_count(State, Count).


%!  read_ntuples(+Stream, -Triples, +State0, -State)

read_ntuples(Stream, Triples, State0, State) :-
    read_ntuple(Stream, Triple0, State0, State1),
    (   Triple0 == end_of_file
    ->  Triples = [],
        State = State1
    ;   map_nodes(Triple0, Triple, State1, State2),
        Triples = [Triple|More],
        read_ntuples(Stream, More, State2, State)
    ).

%!  process_ntriple(+Stream, :CallBack, +State0, -State)

process_ntriple(Stream, CallBack, State0, State) :-
    read_ntuple(Stream, Triple0, State0, State1),
    (   Triple0 == end_of_file
    ->  State = State1
    ;   map_nodes(Triple0, Triple, State1, State2),
        nt_state_graph(State2, Graph),
        call(CallBack, [Triple], Graph),
        process_ntriple(Stream, CallBack, State2, State)
    ).

%!  read_ntuple(+Stream, -Tuple, +State0, -State) is det.
%
%   True when Tuple is the next triple on Stream. May increment
%   the error count on State.

read_ntuple(Stream, Triple, State0, State) :-
    nt_state_on_error(State0, error),
    !,
    read_ntuple(Stream, Triple, State0),
    State = State0.
read_ntuple(Stream, Triple, State0, State) :-
    catch(read_ntuple(Stream, Triple, State0), E, true),
    (   var(E)
    ->  State = State0
    ;   print_message(warning, E),
        nt_state_error_count(State0, EC0),
        EC is EC0+1,
        set_error_count_of_nt_state(EC, State0, State1),
        read_ntuple(Stream, Triple, State1, State)
    ).

read_ntuple(Stream, Triple, State0) :-
    nt_state_format(State0, Format),
    format_read_ntuple(Format, Stream, Triple, State0).

format_read_ntuple(ntriples, Stream, Triple, _) :-
    !,
    read_ntriple(Stream, Triple).
format_read_ntuple(nquads, Stream, Quad, State) :-
    !,
    read_ntuple(Stream, Tuple),
    to_quad(Tuple, Quad, State).

to_quad(Quad, Quad, _) :-
    functor(Quad, quad, 4),
    !.
to_quad(triple(S,P,O), quad(S,P,O,Graph), State) :-
    nt_state_graph(State, Graph).
to_quad(end_of_file, end_of_file, _).


map_nodes(triple(S0,P0,O0), rdf(S,P,O), State0, State) :-
    map_node(S0, S, State0, State1),
    map_node(P0, P, State1, State2),
    map_node(O0, O, State2, State).
map_nodes(quad(S0,P0,O0,G0), rdf(S,P,O,G), State0, State) :-
    map_node(S0, S, State0, State1),
    map_node(P0, P, State1, State2),
    map_node(O0, O, State2, State3),
    map_node(G0, G, State3, State).

map_node(node(NodeId), BNode, State, State) :-
    nt_state_anon_prefix(State, Prefix),
    atom(Prefix),
    !,
    atom_concat(Prefix, NodeId, BNode).
map_node(Node, Node, State, State).


%!  open_input(+Input, -Stream, -Close) is det.
%
%   Open input for reading ntriples. The  default encoding is UTF-8.
%   If the input has a different encoding,   Input  must be a stream
%   with the correct encoding and the stream type must be =text=.

open_input(stream(Stream), Stream, Close) :-
    !,
    (   stream_property(Stream, type(binary))
    ->  set_stream(Stream, encoding(utf8)),
        Close = set_stream(Stream, type(binary))
    ;   stream_property(Stream, encoding(Old)),
        (   n3_encoding(Old)
        ->  true
        ;   domain_error(ntriples_encoding, Old)
        ),
        Close = true
    ).
open_input(Stream, Stream, Close) :-
    is_stream(Stream),
    !,
    open_input(stream(Stream), Stream, Close).
open_input(atom(Atom), Stream, close(Stream)) :-
    !,
    atom_to_memory_file(Atom, MF),
    open_memory_file(MF, read, Stream, [free_on_close(true)]).
open_input(URL, Stream, close(Stream)) :-
    (   sub_atom(URL, 0, _, _, 'http://')
    ;   sub_atom(URL, 0, _, _, 'https://')
    ),
    !,
    http_open(URL, Stream, []),
    set_stream(Stream, encoding(utf8)).
open_input(URL, Stream, close(Stream)) :-
    uri_file_name(URL, Path),
    !,
    open(Path, read, Stream, [encoding(utf8)]).
open_input(File, Stream, close(Stream)) :-
    absolute_file_name(File, Path,
                       [ access(read),
                         extensions(['', nt, ntriples])
                       ]),
    open(Path, read, Stream, [encoding(utf8)]).

n3_encoding(octet).
n3_encoding(ascii).
n3_encoding(iso_latin_1).
n3_encoding(utf8).
n3_encoding(text).

%!  init_state(+Input, +Options, -State) is det.

init_state(In, Options, State) :-
    (   option(base_uri(BaseURI), Options)
    ->  true
    ;   In = stream(_)
    ->  BaseURI = []
    ;   is_stream(In)
    ->  BaseURI = []
    ;   In = atom(_)
    ->  BaseURI = []
    ;   uri_is_global(In),
        \+ is_absolute_file_name(In)        % Avoid C:Path in Windows
    ->  uri_normalized(In, BaseURI)
    ;   uri_file_name(BaseURI, In)
    ),
    (   option(anon_prefix(Prefix), Options)
    ->  true
    ;   BaseURI == []
    ->  Prefix = '_:genid'
    ;   atom_concat('_:', BaseURI, Prefix)
    ),
    option(on_error(OnError), Options, warning),
    % If the format is not set explicitly we assume N-Triples.
    % The format option _must_ be set before make_nt_state/2.
    option(format(Format), Options, ntriples),
    rdf_db:graph(Options, Graph),
    (   var(Graph)
    ->  Graph = user
    ;   true
    ),
    make_nt_state([ anon_prefix(Prefix),
                    on_error(OnError),
                    format(Format),
                    graph(Graph)
                  ], State).


                 /*******************************
                 *          RDF-DB HOOK         *
                 *******************************/

:- multifile
    rdf_db:rdf_load_stream/3,
    rdf_db:rdf_file_type/2.

%!  rdf_db:rdf_load_stream(+Format, +Stream, :Options) is semidet.
%
%   Plugin rule that supports loading   the  =ntriples= and =nquads=
%   formats.

rdf_db:rdf_load_stream(ntriples, Stream, _Module:Options) :-
    rdf_db:graph(Options, Graph),
    rdf_transaction((  rdf_process_ntriples(Stream, assert_tuples, Options),
                       rdf_set_graph(Graph, modified(false))
                    ),
                    parse(Graph)).
rdf_db:rdf_load_stream(nquads, Stream, _Module:Options) :-
    rdf_db:graph(Options, Graph),
    (   var(Graph)
    ->  Graph = user
    ;   true
    ),
    rdf_transaction((  rdf_process_ntriples(Stream, assert_tuples, Options),
                       rdf_set_graph(Graph, modified(false))
                    ),
                    parse(Graph)).

assert_tuples([], _).
assert_tuples([H|T], Graph) :-
    assert_tuple(H, Graph),
    assert_tuples(T, Graph).

assert_tuple(rdf(S,P,O), Graph) :-
    rdf_assert(S,P,O,Graph).
assert_tuple(rdf(S,P,O,Graph), _) :-
    rdf_assert(S,P,O,Graph).


%!  rdf_db:rdf_file_type(+Extension, -Format)
%
%   Bind the ntriples reader to  files   with  the  extensions =nt=,
%   =ntriples= and =nquads=.

rdf_db:rdf_file_type(nt,       ntriples).
rdf_db:rdf_file_type(ntriples, ntriples).
rdf_db:rdf_file_type(nq,       nquads).
rdf_db:rdf_file_type(nquads,   nquads).
