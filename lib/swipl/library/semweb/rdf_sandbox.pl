/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2016, VU University Amsterdam
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

:- module(rdf_sandbox, []).

/** <module> Declare RDF API sandbox-safe

This   module   provides   clauses   for     the   multifile   predicate
sandbox:safe_primitive/1  defined  in  library(sandbox)  that  make  all
predicates of the RDF API that have   no permanent side effects safe. To
have an affect, this  module  must  be   *loaded  after  the  modules it
declares safe*. Thus, when using the   sequence  below, `rdf11` is safe,
while `sparql_client` is not (unless   sparql_client  was already loaded
before this sequence).

  ```
  :- use_module(library(semweb/rdf11)).         % safe
  :- use_module(library(semweb/rdf_sandbox)).
  :- use_module(library(semweb/sparql_client)). % Not safe
  ```

Normally, sandbox declarations live in  the   module  in  which the safe
predicates are defined, so we are sure  we are making declarations about
this specific version of a predicate. For   RDF we decoupled the sandbox
declarations  from  the  implementation  because    although   rdf/3  is
technically safe to use (calling it has   no side effects), it may _make
information accessible_ that is not supposed to be.

Loading this library makes all   side-effect-free  predicates considered
safe. If some of your RDF needs to  remain hidden, you should *not* load
this file and instead use your own   version that project your data. The
example below defines a wrapper around   rdf/4 that provides safe access
to certain graphs.

``` :- module(rdf_api,
          [ rdf/4
          ]).
:- use_module(library(semweb/rdf11)).

:- rdf_meta rdf(r,r,o,r).

rdf(S,P,O,G) :-
        public_graph(G), !,
        rdf11:rdf(S,P,O,G).
rdf(S,P,O,G) :-
        permission_error(access, graph, G).

:- multifile sandbox:safe_primitive/1.

sandbox:safe_primitive(rdf_api:rdf(_,_,_,_)).
*/

:- multifile
    sandbox:safe_primitive/1,
    sandbox:safe_meta_predicate/1.


                 /*******************************
                 *             RDF_DB           *
                 *******************************/

:- if(current_predicate(rdf_db:rdf/3)).
sandbox:safe_primitive(rdf_db:rdf(_,_,_)).
sandbox:safe_primitive(rdf_db:rdf(_,_,_,_)).
sandbox:safe_primitive(rdf_db:rdf_has(_,_,_)).
sandbox:safe_primitive(rdf_db:rdf_has(_,_,_,_)).
sandbox:safe_primitive(rdf_db:rdf_reachable(_,_,_)).
sandbox:safe_primitive(rdf_db:rdf_reachable(_,_,_,_,_)).
sandbox:safe_primitive(rdf_db:rdf_resource(_)).
sandbox:safe_primitive(rdf_db:rdf_subject(_)).
sandbox:safe_primitive(rdf_db:rdf_predicate_property(_,_)).
sandbox:safe_primitive(rdf_db:rdf_current_predicate(_)).
sandbox:safe_primitive(rdf_db:rdf_current_literal(_)).
sandbox:safe_primitive(rdf_db:rdf_graph(_)).
sandbox:safe_primitive(rdf_db:rdf_generation(_)).
sandbox:safe_primitive(rdf_db:rdf_estimate_complexity(_,_,_,_)).
sandbox:safe_primitive(rdf_db:rdf_statistics(_)).
sandbox:safe_primitive(rdf_db:lang_matches(_,_)).
sandbox:safe_primitive(rdf_db:lang_equal(_,_)).
sandbox:safe_primitive(rdf_db:rdf_version(_)).
sandbox:safe_primitive(rdf_db:rdf_md5(_,_)).
sandbox:safe_primitive(rdf_db:rdf_graph_modified_(_,_,_)).
sandbox:safe_primitive(rdf_db:rdf_graph_source_(_,_,_)).
sandbox:safe_primitive(rdf_db:rdf_graph_(_,_)).
sandbox:safe_primitive(rdf_db:rdf_find_literal_map(_,_,_)).
sandbox:safe_primitive(rdf_db:rdf_keys_in_literal_map(_,_,_)).
sandbox:safe_primitive(rdf_db:rdf_statistics_literal_map(_,_)).

sandbox:safe_meta_predicate(rdf_prefixes:rdf_current_prefix/2).
sandbox:safe_meta_predicate(rdf_prefixes:rdf_global_id/2).
:- endif.


                 /*******************************
                 *            RDF11             *
                 *******************************/

:- if(current_predicate(rdf11:in_xml_literal/3)).
sandbox:safe_primitive(rdf11:in_xml_literal(_,_,_)).
sandbox:safe_primitive(rdf11:pre_object(_,_,_,_)).
sandbox:safe_primitive(rdf11:post_object(_,_)).
sandbox:safe_primitive(rdf11:rdf_where(_)).
:- endif.


                 /*******************************
                 *         RDF-LITINDEX         *
                 *******************************/

:- if(current_predicate(rdf_litindex:rdf_find_literals/2)).
sandbox:safe_primitive(rdf_litindex:rdf_find_literals(_,_)).
sandbox:safe_primitive(rdf_litindex:rdf_tokenize_literal(_,_)).
sandbox:safe_primitive(rdf_litindex:rdf_literal_index(_,_)).
:- endif.

                 /*******************************
                 *         SPARQL-CLIENT        *
                 *******************************/

:- if(current_predicate(sparql_client:sparql_query/3)).
sandbox:safe_primitive(sparql_client:sparql_query(_,_,_)).
:- endif.
