/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2020, University of Amsterdam
                              VU University Amsterdam
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

:- module(rdf_library,
          [ rdf_attach_library/1,       % +Dir
            rdf_load_library/1,         % +Ontology
            rdf_load_library/2,         % +Ontology, +Options
            rdf_list_library/0,
            rdf_list_library/1,         % +Ontology
            rdf_list_library/2,         % +Ontology, +Options
            rdf_library_source/2,       % +Ontology, -SourceURL
            rdf_library_index/2,        % ?Id, ?Facet
            rdf_current_manifest/1      % -Manifest
          ]).
:- use_module(library(semweb/rdf_prefixes),
              [ (rdf_meta)/1, op(_,_,rdf_meta)
              ]).
:- use_module(library(semweb/rdf_db),
              [ rdf_register_ns/2, rdf_equal/2, rdf_register_ns/3, rdf_load/2
              ]).

:- autoload(library(apply),[exclude/3,maplist/2]).
:- autoload(library(date),[parse_time/2]).
:- autoload(library(debug),[debug/3]).
:- autoload(library(dif),[dif/2]).
:- autoload(library(error),[must_be/2,existence_error/2]).
:- autoload(library(lists),[member/2,list_to_set/2]).
:- autoload(library(option),[option/2,option/3]).
:- autoload(library(pairs),
	    [pairs_values/2,map_list_to_pairs/3,group_pairs_by_key/2]).
:- autoload(library(rdf),[load_rdf/2]).
:- autoload(library(solution_sequences),[distinct/2]).
:- autoload(library(thread),[concurrent/3]).
:- autoload(library(uri),
	    [ uri_file_name/2,
	      uri_components/2,
	      uri_data/3,
	      uri_is_global/1,
	      uri_normalized/2
	    ]).
:- autoload(library(http/http_open),[http_open/3]).
:- autoload(library(semweb/turtle),[rdf_load_turtle/3]).

:- predicate_options(rdf_list_library/2, 2,
                     [ indent(atom),
                       show_graph(boolean),
                       show_source(boolean),
                       show_virtual(boolean)
                     ]).
:- predicate_options(rdf_load_library/2, 2,
                     [ concurrent(positive_integer),
                       import(boolean),
                       load(boolean),
                       base_uri(atom),
                       claimed_source(atom),
                       not_found(oneof([error,warning,silent]))
                     ]).

/** <module> RDF Library Manager

This module manages an ontology library. Such   a  library consists of a
directory with manifest files named =|Manifest.rdf|= or =|Manifest.ttl|=
(Turtle). The manifest files define ontologies  appearing in the library
as well as namespace mnemonics and dependencies.

The typical usage scenario is

==
?- rdf_attach_library('/some/directory').
?- rdf_load_library(my_ontology).
==

@tbd    Add caching info
@tbd    Allow using Manifests on HTTP servers
@author Jan Wielemaker
*/

:- rdf_register_ns(lib,  'http://www.swi-prolog.org/rdf/library/').
:- rdf_register_ns(void, 'http://rdfs.org/ns/void#').
:- rdf_register_ns(vann, 'http://purl.org/vocab/vann/').

:- dynamic
    manifest/2,                     % Path, Time
    library_db/3.                   % Name, URL, Facets

%       Force compile-time namespace expansion

:- rdf_meta
    edge(+, r,r,o).

                 /*******************************
                 *            LOADING           *
                 *******************************/

%!  rdf_load_library(+Id) is det.
%!  rdf_load_library(+Id, +Options) is det.
%
%   Load ontologies from the  library.  A   library  must  first  be
%   attached using rdf_attach_library/1.  Defined Options are:
%
%           * import(Bool)
%           If =true= (default), also load ontologies that are
%           explicitely imported.
%
%           * base_uri(URI)
%           BaseURI used for loading RDF.  Local definitions in
%           ontologies overrule this option.
%
%           * claimed_source(URL)
%           URL from which we claim to have loaded the data.
%
%           * not_found(+Level)
%           The system does a pre-check for the existence of
%           all references RDF databases.  If Level is =error=
%           it reports missing databases as an error and fails.
%           If =warning= it prints them, but continues.  If
%           =silent=, no checks are preformed.  Default is =error=.
%
%           * concurrent(Threads)
%           Perform the load concurrently using N threads.  If not
%           specified, the number is determined by
%           guess_concurrency/2.
%
%           * load(+Bool)
%           If =false=, to all the preparation, but do not execute
%           the actual loading.  See also rdf_list_library/2.

rdf_load_library(Id) :-
    rdf_load_library(Id, []).

rdf_load_library(Id, Options) :-
    cleaned_load_commands(Id, Cmds, Options),
    (   option(concurrent(Threads), Options)
    ->  true
    ;   guess_concurrency(Cmds, Threads)
    ),
    length(Cmds, NSources),
    print_message(informational, rdf(loading(NSources, Threads))),
    (   option(load(true), Options, true)
    ->  concurrent(Threads, Cmds, [])
    ;   true
    ).

%!  rdf_library_source(+Id, -Source) is nondet.
%
%   True of Source is the URL that is  part of the given library Id.
%   This predicate finds all indirect   dependencies.  It does _not_
%   check whether the source exists or is valid.
%
%   @see uri_file_name/2 for converting file:// URLs to a filename.

rdf_library_source(Id, Source) :-
    cleaned_load_commands(Id, Cmds,
                          [ import(true),
                            not_found(silent)
                          ]),
    member(rdf_load(Source, _), Cmds).


cleaned_load_commands(Id, Cmds, Options) :-
    load_commands(Id, Options, Pairs),
    pairs_values(Pairs, Commands),
    list_to_set(Commands, Cmds2),
    delete_virtual(Cmds2, Cmds3),
    find_conflicts(Cmds3),
    check_existence(Cmds3, Cmds, Options).

delete_virtual([], []).
delete_virtual([virtual(_)|T0], T) :-
    !,
    delete_virtual(T0, T).
delete_virtual([H|T0], [H|T]) :-
    delete_virtual(T0, T).


%!  find_conflicts(+LoadCommands) is semidet.
%
%   Find possibly conflicting options for loading the same source

find_conflicts(Commands) :-
    no_source_with_different_options(Commands),
    no_sources_in_same_graph(Commands).

%!  no_source_with_different_options(+Commands) is semidet.
%
%   True if there are not multiple calls to load the same graph, but
%   with  different  load-options.  Prints  a    warning  and  fails
%   otherwise.

no_source_with_different_options(Commands) :-
    sort(Commands, Cmds),
    conflicts(Cmds, Conflicts),
    report_conflicts(Conflicts),
    Conflicts == [].

conflicts([], []).
conflicts([C1, C2|T0], [C1-C2|T]) :-
    conflict(C1, C2),
    !,
    conflicts([C2|T0], T).
conflicts([_|T0], T) :-
    conflicts(T0, T).

conflict(rdf_load(Src, Options1), rdf_load(Src, Options2)) :-
    sort(Options1, S1),
    sort(Options2, S2),
    S1 \== S2.

report_conflicts([]).
report_conflicts([C1-C2|T]) :-
    print_message(warning, rdf(load_conflict(C1,C2))),
    report_conflicts(T).

%!  no_sources_in_same_graph(+Commands) is semidet.
%
%   True if there are not two load   commands  referring to the same
%   graph.

no_sources_in_same_graph(Commands) :-
    map_list_to_pairs(command_graph, Commands, Keyed),
    keysort(Keyed, KeySorted),
    group_pairs_by_key(KeySorted, SourcesByGraph),
    (   member(Graph-Sources, SourcesByGraph),
        Sources = [_,_|_]
    ->  forall(( member(Graph-Sources, SourcesByGraph),
                 Sources = [_,_|_]
               ),
               print_message(error,
                             rdf(multiple_source_for_graph(Graph, Sources)))),
        fail
    ;   true
    ).

command_graph(rdf_load(_, Options), Graph) :-
    option(graph(Graph), Options),
    !.
command_graph(rdf_load(URL, _), URL) :- !.
command_graph(_, _).                    % Other command.  Each variable it its own key


%!  check_existence(+CommandsIn, -Commands, +Options) is det.
%
%   Report existence errors. Fail if at   least  one source does not
%   exist. and the not_found level is not =silent=.
%
%   @error existence_error(urls, ListOfUrls)

check_existence(CommandsIn, Commands, Options) :-
    option(not_found(Level), Options, error),
    must_be(oneof([error,warning,silent]), Level),
    (   Level == silent
    ->  Commands = CommandsIn
    ;   missing_urls(CommandsIn, Commands, Missing),
        (   Missing == []
        ->  true
        ;   Level == warning
        ->  report_missing(Missing, Level)
        ;   existence_error(urls, Missing)
        )
    ).


missing_urls([], [], []).
missing_urls([H|T0], Cmds, Missing) :-
    H = rdf_load(URL, _),
    (   catch(exists_url(URL, _Ext), error(existence_error(_,_), _), fail)
    ->  Cmds = [H|T],
        missing_urls(T0, T, Missing)
    ;   Missing = [URL|T],
        missing_urls(T0, Cmds, T)
    ).

report_missing([], _).
report_missing([H|T], Level) :-
    print_message(Level, error(existence_error(url, H), _)),
    report_missing(T, Level).

%!  guess_concurrency(+Commands, -Threads) is det.
%
%   How much concurrency to use? Set to   the  number of CPUs if all
%   input comes from  files  or  5   if  network  based  loading  is
%   demanded.

guess_concurrency(Commands, Threads) :-
    count_uris(Commands, FileURLs, OtherURLs),
    (   FileURLs > 0
    ->  (   current_prolog_flag(cpu_count, CPUs)
        ->  true
        ;   CPUs = 1
        ),
        FileThreads is min(FileURLs, CPUs)
    ;   FileThreads = 0
    ),
    (   OtherURLs > 0
    ->  OtherThreads is min(5, OtherURLs)
    ;   OtherThreads = 0
    ),
    Threads is FileThreads + OtherThreads.

count_uris([], 0, 0).
count_uris([rdf_load(URL, _)|T], F, NF) :-
    count_uris(T, F0, NF0),
    (   web_url(URL)
    ->  NF is NF0 + 1,
        F = F0
    ;   F is F0 + 1,
        NF = NF0
    ).


%!  load_commands(+Id, +Options, -Pairs:list(Level-Command)) is det.
%
%   Commands are the RDF commands to execute for rdf_load_library/2.
%   Splitting  in  command  collection  and   execution  allows  for
%   concurrent execution as well  as   forward  checking of possible
%   problems.
%
%   @tbd    Fix poor style; avoid assert/retract.

:- thread_local
    command/2.

load_commands(Id, Options, Commands) :-
    retractall(command(_,_)),
    rdf_update_library_index,
    dry_load(Id, 1, Options),
    findall(Level-Cmd, retract(command(Level, Cmd)), Commands).

dry_load(Id, Level, Options) :-
    (   library(Id, File, Facets)
    ->  merge_base_uri(Facets, Options, Options1),
        merge_source(Facets, Options1, Options2),
        merge_blanks(Facets, Options2, Options3),
        merge_format(Facets, Options3, Options4),
        (   \+ memberchk(virtual, Facets)
        ->  load_options(Options4, File, RdfOptions),
            assert(command(Level, rdf_load(File, RdfOptions)))
        ;   assert(command(Level, virtual(File)))
        ),
        (   option(import(true), Options, true)
        ->  Level1 is Level + 1,
            forall(member(imports(Type, Import), Facets),
                   import(Import, Level1, [type(Type)|Options4]))
        ;   true
        )
    ;   existence_error(ontology, Id)
    ).

merge_base_uri(Facets, Options0, Options) :-
    (   option(base_uri(Base), Facets)
    ->  exclude(name_option(base_uri), Options0, Options1),
        Options = [base_uri(Base)|Options1]
    ;   Options = Options0
    ).

merge_source(Facets, Options0, Options) :-
    (   option(claimed_source(Base), Facets)
    ->  exclude(name_option(claimed_source), Options0, Options1),
        Options = [claimed_source(Base)|Options1]
    ;   Options = Options0
    ).

merge_blanks(Facets, Options0, Options) :-
    (   option(blank_nodes(Share), Facets)
    ->  exclude(name_option(blank_nodes), Options0, Options1),
        Options = [blank_nodes(Share)|Options1]
    ;   Options = Options0
    ).

merge_format(Facets, Options0, Options) :-
    (   option(format(Format), Facets)
    ->  exclude(name_option(format), Options0, Options1),
        Options = [format(Format)|Options1]
    ;   Options = Options0
    ).

name_option(Name, Term) :-
    functor(Term, Name, 1).

load_options(Options, File, RDFOptions) :-
    findall(O, load_option(Options, File, O), RDFOptions).

load_option(Options, File, graph(Source)) :-
    option(claimed_source(Source0), Options),
    (   sub_atom(Source0, _, _, 0, /)
    ->  file_base_name(File, Base),
        atom_concat(Source0, Base, Source)
    ;   atom_concat(Source, #, Source0)
    ->  true
    ).
load_option(Options, File, base_uri(BaseURI)) :-
    option(base_uri(Base0), Options),
    sub_atom(/, _, _, 0, Base0),
    atom_concat(Base0, File, BaseURI).
load_option(Options, _File, blank_nodes(Share)) :-
    option(blank_nodes(Share), Options).
load_option(Options, _File, format(Format)) :-
    option(format(Format), Options).

%!  import(+URL, +Level, +Options) is det.

import(Path, Level, Options) :-
    option(type(data_dump), Options),
    !,
    load_options(Options, Path, RdfOptions),
    assert(command(Level, rdf_load(Path, RdfOptions))).
import(Path, Level, Options) :-
    (   (   library(Id, Path, _)
        ->  true
        ;   manifest_for_path(Path, Manifest),
            catch(exists_url(Manifest, _Ext), _, fail)
        ->  process_manifest(Manifest),
            library(Id, Path, _)
        )
    ->  dry_load(Id, Level, Options)
    ;   load_options(Options, Path, RdfOptions),
        assert(command(Level, rdf_load(Path, RdfOptions)))
    ).

manifest_for_path(URL, Manifest) :-
    file_directory_name(URL, Parent),
    manifest_file(Base),
    rdf_extension(Ext),
    atomic_list_concat([Parent, /, Base, '.', Ext], Manifest).

%!  rdf_list_library(+Id) is det.
%!  rdf_list_library(+Id, +Options) is det.
%
%   Print library dependency tree to the terminal.  Options include
%   options for rdf_load_library/2 and
%
%           * show_source(+Boolean)
%           If =true= (default), show location we are loading
%
%           * show_graph(+Boolean)
%           If =true= (default =false=), show name of graph
%
%           * show_virtual(+Boolean)
%           If =false= (default =true=), do not show virtual
%           repositories.
%
%           * indent(Atom)
%           Atom repeated for indentation levels

rdf_list_library(Id) :-
    rdf_list_library(Id, []).
rdf_list_library(Id, Options) :-
    load_commands(Id, Options, Commands),
    maplist(print_load(Options), Commands).

print_load(Options, _Level-virtual(_)) :-
    option(show_virtual(false), Options),
    !.
print_load(Options, Level-Command) :-
    option(indent(Indent), Options, '. '),
    forall(between(2, Level, _), format(Indent)),
    print_command(Command, Options),
    format('~N').

print_command(virtual(URL), _Options) :-
    format('<~w>', [URL]).
print_command(rdf_load(URL), Options) :-
    print_command(rdf_load(URL, []), Options).
print_command(rdf_load(URL, RDFOptions), Options) :-
    (   option(show_source(true), Options, true)
    ->  format('~w', [URL]),
        (   option(blank_nodes(noshare), RDFOptions)
        ->  format(' <not shared>')
        ;   true
        ),
        (   exists_url(URL, Ext)
        ->  (   Ext == ''
            ->  true
            ;   format('[.~w]', [Ext])
            )
        ;   format(' [NOT FOUND]')
        )
    ;   true
    ),
    (   option(show_graph(true), Options, false),
        option(graph(Base), RDFOptions)
    ->  format('~N\tSource: ~w', [Base])
    ;   true
    ).

exists_url(URL, Ext) :-
    uri_file_name(URL, Path),
    !,
    add_storage_extension(Path, Ext, PathEx),
    access_file(PathEx, read),
    !.
exists_url(URL, Ext) :-
    uri_components(URL, Components),
    uri_data(scheme, Components, Scheme),
    atom(Scheme),
    url_scheme(Scheme),
    add_storage_extension(URL, Ext, URLEx),
    catch(http_open(URLEx, Stream, [ method(head) ]), _, fail),
    !,
    close(Stream).

:- multifile
    rdf_db:rdf_storage_encoding/2.

add_storage_extension(File, '', File).
add_storage_extension(File, Ext, FileEx) :-
    rdf_db:rdf_storage_encoding(Ext, _Format),
    \+ file_name_extension(_, Ext, File),
    file_name_extension(File, Ext, FileEx).

url_scheme(http).
url_scheme(https).


%!  rdf_list_library
%
%   Prints known RDF library identifiers to current output.

rdf_list_library :-
    rdf_update_library_index,
    (   rdf_library_index(Id, title(TitleLiteral)),
        plain_string(TitleLiteral, Title),
        format('~w ~t~20|~w', [Id, Title]),
        (   rdf_library_index(Id, version(Version))
        ->  format(' (version ~w)', [Version])
        ;   true
        ),
        nl,
        fail
    ;   true
    ).

plain_string(String, String) :-
    atomic(String),
    !.
plain_string(lang(en, String), String) :- !.
plain_string(lang(_, String), String) :- !.
plain_string(type(_, String), String) :- !.

%!  rdf_library_index(?Id, ?Facet) is nondet.
%
%   Query the content of the library.  Defined facets are:
%
%           * source(URL)
%           Location from which to load the ontology
%
%           * title(Atom)
%           Title used for the ontology
%
%           * comment(Atom)
%           Additional comments for the ontology
%
%           * version(Atom)
%           Version information on the ontology
%
%           * imports(Type, URL)
%           URLs needed by this ontology. May succeed multiple
%           times.  Type is one of =ontology=, =schema= or =instances=.
%
%           * base_uri(BaseURI)
%           Base URI to use when loading documents. If BaseURI
%           ends in =|/|=, the actual filename is attached.
%
%           * claimed_source(Source)
%           URL from which we claim to have loaded the RDF. If
%           Source ends in =|/|=, the actual filename is
%           attached.
%
%           * blank_nodes(Share)
%           Defines how equivalent blank nodes are handled, where
%           Share is one of =share= or =noshare=.  Default is to
%           share.
%
%           * format(Format)
%           Format of the resource.  Can be used to overrule
%           if the format as derived from the HTTP content type
%           is wrong.
%
%           * provides_ns(URL)
%           Ontology provides definitions in the namespace URL.
%           The formal definition of this is troublesome, but in
%           practice it means the ontology has triples whose
%           subjects are in the given namespace.
%
%           * uses_ns(URL)
%           The ontology depends on the given namespace.  Normally
%           means it contains triples that have predicates or
%           objects in the given namespace.
%
%           * manifest(URL)
%           URL of the manifest in which this ontology is defined.
%
%           * virtual
%           Entry is virtual (cannot be loaded)

rdf_library_index(Id, Facet) :-
    library(Id, Path, Facets),
    (   Facet = source(Path)
    ;   member(Facet, Facets)
    ).


                 /*******************************
                 *      MANIFEST PROCESSING     *
                 *******************************/

%!  rdf_attach_library(+Source)
%
%   Attach manifest from Source.  Source is one of
%
%           * URL
%           Load single manifest from this URL
%           * File
%           Load single manifest from this file
%           * Directory
%           Scan all subdirectories and load all =|Manifest.ttl|= or
%           =|Manifest.rdf|= found.  If Directory is a path-alias
%           (e.g., ontology(.)), _all_ referenced directories are
%           scanned for manifest files.
%
%   Encountered namespaces are registered   using rdf_register_ns/2.
%   Encountered ontologies are added to the index. If a manifest was
%   already loaded it will be reloaded  if the modification time has
%   changed.

rdf_attach_library(URL) :-
    atom(URL),
    uri_is_global(URL),
    \+ is_absolute_file_name(URL),   % avoid interpreting C: as a schema
    !,
    process_manifest(URL).
rdf_attach_library(File) :-
    absolute_file_name(File, Path,
                       [ extensions([rdf,ttl]),
                         access(read),
                         file_errors(fail)
                       ]),
    !,
    process_manifest(Path).
rdf_attach_library(Dir) :-
    forall(absolute_file_name(Dir, Path,
                              [ file_type(directory),
                                access(read),
                                solutions(all)
                              ]),
           attach_dir(Path, [])).


%!  rdf_update_library_index
%
%   Reload all Manifest files.

rdf_update_library_index :-
    forall(manifest(Location, _Time),
           process_manifest(Location)).

attach_dir(Path, Visited) :-
    memberchk(Path, Visited),
    !.
attach_dir(Path, Visited) :-
    atom_concat(Path, '/*', Pattern),
    expand_file_name(Pattern, Members),
    (   manifest_file(MBase),
        rdf_extension(Ext),
        atomic_list_concat([Path, /, MBase, '.', Ext], Manifest),
        exists_file(Manifest)
    ->  process_manifest(Manifest)
    ;   print_message(silent, rdf(no_manifest(Path)))
    ),
    (   member(Dir, Members),
        exists_directory(Dir),
        file_base_name(Dir, Base),
        \+ hidden_base(Base),
        attach_dir(Dir, [Path|Visited]),
        fail ; true
    ).

hidden_base('CVS').
hidden_base('cvs').                     % Windows

%!  process_manifest(+Location) is det.
%
%   Process a manifest file, registering  encountered namespaces and
%   creating clauses for library/3. No op if manifest was loaded and
%   not changed. Removes old data if the manifest was changed.
%
%   @param  Location is either a path name or a URL.

process_manifest(Source) :-
    (   web_url(Source)
    ->  uri_normalized(Source, Manifest)
    ;   uri_file_name(Source, Manifest0)
    ->  absolute_file_name(Manifest0, ManifestFile),
        uri_file_name(Manifest, ManifestFile)
    ;   absolute_file_name(Source, ManifestFile),
        uri_file_name(Manifest, ManifestFile)
    ),                              % Manifest is a canonical URI
    source_time(Manifest, MT),
    (   manifest(Manifest, Time),
        (   MT =< Time
        ->  !
        ;   retractall(manifest(Manifest, Time)),
            library_db(Id, URL, Facets),
            memberchk(manifest(Manifest), Facets),
            retractall(library_db(Id, URL, Facets)),
            fail
        )
    ;   read_triples(Manifest, Triples),
        process_triples(Manifest, Triples),
        print_message(informational, rdf(manifest(loaded, Manifest))),
        assert(manifest(Manifest, MT))
    ).

process_triples(Manifest, Triples) :-
    findall(ns(Mnemonic, NameSpace),
            extract_namespace(Triples, Mnemonic, NameSpace),
            NameSpaces),
    findall(Ontology,
            extract_ontology(Triples, Ontology),
            Ontologies),
    maplist(define_namespace, NameSpaces),
    maplist(assert_ontology(Manifest), Ontologies).

%!  extract_namespace(+Triples, -Mnemonic, -NameSpace)
%
%   True if Mnemonic is an abbreviation of NameSpace.

extract_namespace(Triples, Mnemonic, Namespace) :-
    edge(Triples, Decl, lib:mnemonic, literal(Mnemonic)),
    edge(Triples, Decl, lib:namespace, Namespace).
extract_namespace(Triples, Mnemonic, Namespace) :-
    edge(Triples, Decl, vann:preferredNamespacePrefix, literal(Mnemonic)),
    edge(Triples, Decl, vann:preferredNamespaceUri, literal(Namespace)).

%!  extract_ontology(+Triples, -Ontology) is nondet.
%
%   Extract definition of an ontology

extract_ontology(Triples, library(Name, URL, Options)) :-
    distinct(URL, ontology(Triples, URL)),
    file_base_name(URL, BaseName),
    file_name_extension(Name, _, BaseName),
    findall(Facet, facet(Triples, URL, Facet), Options0),
    sort(Options0, Options1),
    keep_specialized_facets(Options1, Options).

ontology(Triples, URL) :-
    edge(Triples, URL, rdf:type, Type),
    ontology_type(Type).

keep_specialized_facets(All, Special) :-
    exclude(more_general(All), All, Special).

more_general(All, Facet) :-
    generalized(Facet, Special),
    memberchk(Special, All).

generalized(imports(ontology, Path), imports(Other, Path)) :-
    dif(Other, ontology).

ontology_type(X) :-
    (   rdf_equal(X, lib:'Ontology')
    ;   rdf_equal(X, lib:'Schema')
    ;   rdf_equal(X, lib:'Instances')
    ;   rdf_equal(X, void:'Dataset')
    ;   rdf_equal(X, void:'Linkset')
    ).

%!  facet(+Triples, +File, -Facet) is nondet.
%
%   Enumerate facets about File from   Triples. Facets are described
%   with rdf_library_index/2.

facet(Triples, File, title(Title)) :-
    edge(Triples, File, dcterms:title, literal(Title)).
facet(Triples, File, version(Version)) :-
    edge(Triples, File, owl:versionInfo, literal(Version)).
facet(Triples, File, comment(Comment)) :-
    edge(Triples, File, rdfs:comment, literal(Comment)).
facet(Triples, File, base_uri(BaseURI)) :-
    edge(Triples, File, lib:baseURI, BaseURI).
facet(Triples, File, claimed_source(Source)) :-
    edge(Triples, File, lib:source, Source).
facet(Triples, File, format(Format)) :-
    edge(Triples, File, lib:format, literal(Format)).
facet(Triples, File, blank_nodes(Mode)) :-
    edge(Triples, File, lib:blankNodes, literal(Mode)),
    must_be(oneof([share,noshare]), Mode).
facet(Triples, File, imports(ontology, Path)) :-
    edge(Triples, File, owl:imports, Path).
facet(Triples, File, imports(schema, Path)) :-
    edge(Triples, File, lib:schema, Path).
facet(Triples, File, imports(instances, Path)) :-
    edge(Triples, File, lib:instances, Path).
facet(Triples, File, imports(subset, Path)) :-
    edge(Triples, File, void:subset, Path).
facet(Triples, File, imports(data_dump, Path)) :-
    edge(Triples, File, void:dataDump, Path).
facet(Triples, File, provides_ns(NS)) :-
    edge(Triples, File, lib:providesNamespace, NSDecl),
    edge(Triples, NSDecl, lib:namespace, NS).
facet(Triples, File, uses_ns(NS)) :-
    edge(Triples, File, lib:usesNamespace, NSDecl),
    edge(Triples, NSDecl, lib:namespace, NS).
facet(Triples, File, virtual) :-
    (   edge(Triples, File, rdf:type, lib:'Virtual')
    ;   edge(Triples, File, rdf:type, void:'Dataset')
    ;   edge(Triples, File, rdf:type, void:'Linkset')
    ) -> true.

%!  edge(+Triples, ?S, ?P, ?O) is nondet.
%
%   Like rdf_has/3 over a list of Triples.

edge(Triples, S, P, O) :-
    nonvar(P),
    !,
    sub_p(SubP, P),
    member(rdf(S,SubP,O), Triples).
edge(Triples, S, P, O) :-
    member(rdf(S,SubP,O), Triples),
    sub_p(SubP, P).

sub_p(P, P).
sub_p(Sub, P) :-
    (   nonvar(Sub)
    ->  sub_property_of(Sub, Sub1),
        sub_p(Sub1, P)
    ;   sub_property_of(Sub1, P),
        sub_p(Sub, Sub1)
    ).

:- rdf_meta
    sub_property_of(r,r).

sub_property_of(void:subset,         owl:imports).
sub_property_of(dcterms:description, rdfs:comment).
sub_property_of(void:dataDump,       owl:imports).
sub_property_of(dc:title,            dcterms:title).

%!  source_time(+Source, -Modified) is semidet.
%
%   Modified is the last modification time of Source.
%
%   @error  existence_error(Type, Source).

source_time(URL, Modified) :-
    web_url(URL),
    !,
    http_open(URL, Stream,
              [ header(last_modified, Date),
                method(head)
              ]),
    close(Stream),
    Date \== '',
    parse_time(Date, Modified).
source_time(URL, Modified) :-
    uri_file_name(URL, File),
    !,
    time_file(File, Modified).
source_time(File, Modified) :-
    time_file(File, Modified).

web_url(URL) :-
    sub_atom(URL, 0, _, _, 'http://').


%!  read_triples(+URL, -Triples) is det.
%
%   Read RDF/XML or Turtle file into a list of triples.

read_triples(FileURL, Triples) :-
    uri_file_name(FileURL, File),
    !,
    (   file_name_extension(_, rdf, File)
    ->  load_rdf(File, Triples)
    ;   rdf_load_turtle(File, Triples, [])
    ).
read_triples(HTTPURL, Triples) :-
    file_name_extension(_, Ext, HTTPURL),
    setup_call_cleanup(
        http_open(HTTPURL, In, []),
        stream_triples(In, Ext, Triples),
        close(In)).

stream_triples(Stream, rdf, Triples) :-
    load_rdf(stream(Stream), Triples).
stream_triples(Stream, ttl, Triples) :-
    rdf_load_turtle(stream(Stream), Triples, []).


manifest_file('void').                  % make order optional?
manifest_file('Manifest').
manifest_file('manifest').

rdf_extension(ttl).
rdf_extension(rdf).


%!  assert_ontology(+Manifest, +Term:library(Name, File, Facets)) is det.
%
%   Add ontology to our library.
%
%   @tbd    Proper behaviour of re-definition?

assert_ontology(Manifest, Term) :-
    Term = library(Name, URL, Facets),
    (   library(Name, _URL2, Facets2)
    ->  memberchk(manifest(Manifest2), Facets2),
        print_message(warning, rdf(redefined(Manifest, Name, Manifest2)))
    ;   true
    ),
    assert(library_db(Name, URL,
                   [ manifest(Manifest)
                   | Facets
                   ])).


%!  library(?Id, ?URL, ?Facets)
%
%   Access DB for library information.

library(Id, URL, Facets) :-
    nonvar(URL),
    normalize_url(URL, CanonicalURL),
    library_db(Id, CanonicalURL, Facets).
library(Id, URL, Facets) :-
    library_db(Id, URL, Facets).

%!  normalize_url(+URL, -Normalized)
%
%   Like uri_normalized/2, but we  also   need  (platform dependent)
%   filename canonization.

normalize_url(URL, CanonicalURL) :-
    uri_file_name(URL, File),
    !,
    absolute_file_name(File, CanFile),
    uri_file_name(CanonicalURL, CanFile).
normalize_url(URL, CanonicalURL) :-
    uri_normalized(URL, CanonicalURL).

%!  define_namespace(NS:ns(Mnemonic, Namespace)) is det.
%
%   Add namespace declaration for Mnemonic.

define_namespace(ns(Mnemonic, Namespace)) :-
    debug(rdf_library, 'Adding NS ~w = ~q', [Mnemonic, Namespace]),
    rdf_register_ns(Mnemonic, Namespace,
                    [
                        ]).

%!  rdf_current_manifest(-URL) is nondet.
%
%   True if URL is the URL of a currently loaded manifest file.

rdf_current_manifest(URL) :-
    manifest(URL, _Time).



                 /*******************************
                 *            MESSAGES          *
                 *******************************/

:- multifile
    prolog:message/3.

prolog:message(rdf(no_manifest(Path))) -->
    [ 'Directory ~w has no Manifest.{ttl,rdf} file'-[Path] ].
prolog:message(rdf(redefined(Manifest, Name, Manifest2))) -->
    [ '~w: Ontology ~w already defined in ~w'-
      [Manifest, Name, Manifest2]
    ].
prolog:message(rdf(manifest(loaded, Manifest))) -->
    [ 'Loaded RDF manifest ~w'-[Manifest]
    ].
prolog:message(rdf(load_conflict(C1, C2))) -->
    [ 'Conflicting loads: ~p <-> ~p'-[C1, C2] ].
prolog:message(rdf(multiple_source_for_graph(Graph, Sources))) -->
    [ 'Multiple sources for graph ~p:'-[Graph] ],
    sources(Sources).
prolog:message(rdf(loading(Files, Threads))) -->
    [ 'Loading ~D files using ~D threads ...'-[Files, Threads] ].

sources([]) --> [].
sources([rdf_load(From, _Options)|T]) -->
    [ nl, '\t~p'-[From] ],
    sources(T).
