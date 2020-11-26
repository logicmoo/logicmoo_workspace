/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2006-2012, University of Amsterdam
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

:- module(pldoc_files,
          [ doc_save/2,                 % +File, +Options
            doc_pack/1                  % +Pack (re-export from doc_pack)
          ]).
:- use_module(library(pldoc), []).
:- use_module(pldoc(doc_html)).
:- use_module(pldoc(doc_index)).
:- use_module(pldoc(doc_pack)).
:- use_module(library(option)).
:- use_module(library(lists)).
:- use_module(library(filesex)).

/** <module> Create stand-alone documentation files

Create stand-alone documentation from a  bundle of source-files. Typical
use of the PlDoc package is to run   it as a web-server from the project
in progress, providing search and guaranteed consistency with the loaded
version. Creating stand-alone files as  provided   by  this  file can be
useful for printing or distribution.

@tbd    Generate a predicate index?
*/

:- predicate_options(doc_save/2, 2,
                     [ format(oneof([html])),
                       doc_root(atom),
                       man_server(atom),
                       index_file(atom),
                       if(oneof([loaded,true])),
                       recursive(boolean),
                       css(oneof([copy,inline])),
                       title(atom)
                     ]).


%!  doc_save(+FileOrDir, +Options)
%
%   Save documentation for FileOrDir to file(s).  Options include
%
%           * format(+Format)
%           Currently only supports =html=.
%
%           * doc_root(+Dir)
%           Save output to the given directory.  Default is to save
%           the documentation for a file in the same directory as
%           the file and for a directory in a subdirectory =doc=.
%
%           * title(+Title)
%           Title is an atom that provides the HTML title of the
%           main (index) page.  Only meaningful when generating
%           documentation for a directory.
%
%           * man_server(+RootURL)
%           Root of a manual server used for references to built-in
%           predicates. Default is
%           =|http://www.swi-prolog.org/pldoc/|=
%
%           * index_file(+Base)
%           Filename for directory indices.  Default is =index=.
%
%           * if(Condition)
%           What to do with files in a directory.  =loaded= (default)
%           only documents files loaded into the Prolog image.  =true=
%           documents all files.
%
%           * recursive(+Bool)
%           If =true=, recurse into subdirectories.
%
%           * css(+Mode)
%           If =copy=, copy the CSS file to created directories.
%           Using =inline=, include the CSS file into the created
%           files.  Currently, only the default =copy= is supported.
%
%   The typical use-case is to document the Prolog files that belong
%   to a project in the  current  directory.   To  do  this load the
%   Prolog  files  and  run  the   goal    below.   This  creates  a
%   sub-directory  =doc=  with  an  index  file  =|index.html|=.  It
%   replicates the directory structure  of   the  source  directory,
%   creating an HTML file for each Prolog file and an index file for
%   each sub-directory. A  copy  of  the   required  CSS  and  image
%   resources is copied to the =doc= directory.
%
%     ==
%     ?- doc_save(., [recursive(true)]).
%     ==

doc_save(Spec, Options) :-
    doc_target(Spec, Target, Options),
    target_directory(Target, Dir),
    phrase(file_map(Target), FileMap),
    merge_options([ html_resources(pldoc_files),
                    source_link(false),
                    resource_directory(Dir)
                  ], Options, Options1),
    Options2 = [files(FileMap)|Options1],
    setup_call_cleanup(
        nb_setval(pldoc_options, Options2),
        generate(Target, Options2),
        nb_delete(pldoc_options)),
    copy_resources(Dir, Options2).


%!  generate(+Spec, +Options) is det.
%
%   Generate  documentation  for  the    specification   created  by
%   doc_target/2.

generate([], _).
generate([H|T], Options) :-
    \+ \+ generate(H, Options),
    generate(T, Options).
generate(file(PlFile, DocFile), Options) :-
    b_setval(pldoc_output, DocFile),
    setup_call_cleanup(
        open(DocFile, write, Out, [encoding(utf8)]),
        with_output_to(Out, doc_for_file(PlFile, Options)),
        close(Out)).
generate(directory(Dir, IndexFile, Members, DirOptions), Options) :-
    append(DirOptions, Options, AllOptions),
    b_setval(pldoc_output, IndexFile),
    setup_call_cleanup(
        open(IndexFile, write, Out, [encoding(utf8)]),
        with_output_to(
            Out,
            doc_for_dir(Dir,
                        [ members(Members)
                        | AllOptions
                        ])),
        close(Out)),
    generate(Members, Options).


%!  doc_target(+Spec, -Target, +Options) is semidet.
%
%   Generate a structure describing what to document in what files.
%   This structure is a term:
%
%           * file(PlFile, DocFile)
%           Document PlFile in DocFile
%
%           * directory(Dir, IndexFile, Members, Options)
%           Document Dir in IndexFile.  Members is a list of
%           documentation structures.

doc_target(FileOrDir, file(File, DocFile), Options) :-
    absolute_file_name(FileOrDir, File,
                       [ file_type(prolog),
                         file_errors(fail),
                         access(read)
                       ]),
    !,
    (   option(source_root(_), Options)
    ->  Options1 = Options
    ;   file_directory_name(File, FileDir),
        Options1 = [source_root(FileDir)|Options]
    ),
    document_file(File, DocFile, Options1).
doc_target(FileOrDir, directory(Dir, Index, Members, DirOptions), Options0) :-
    absolute_file_name(FileOrDir, Dir,
                       [ file_type(directory),
                         file_errors(fail),
                         access(read)
                       ]),
    !,
    (   option(source_root(_), Options0)            % recursive
    ->  Options = Options0
    ;   Options1 = [source_root(Dir)|Options0],     % top
        exclude(main_option, Options1, Options2),
        set_doc_root(Dir, Options2, Options)
    ),
    DirOptions = Options,
    document_file(Dir, Index, Options),
    findall(Member,
            (   prolog_file_in_dir(Dir, File, Options),
                doc_target(File, Member, Options)
            ),
            Members).

%!  main_option(?Option)
%
%   Options that apply only to the main directory.

main_option(title(_)).
main_option(readme(_)).
main_option(todo(_)).

target_directory(directory(_, Index, _, _), Dir) :-
    file_directory_name(Index, Dir).
target_directory(file(_, DocFile), Dir) :-
    file_directory_name(DocFile, Dir).

set_doc_root(_Dir, Options0, Options) :-
    option(doc_root(_), Options0),
    !,
    Options = Options0.
set_doc_root(Dir, Options0, Options) :-
    directory_file_path(Dir, doc, DocRoot),
    Options = [doc_root(DocRoot)|Options0].

%!  file_map(+DocStruct, -List)
%
%   Create a list of file(PlFile, DocFile) for files that need to
%   be documented.

file_map([]) -->
    [].
file_map([H|T]) -->
    file_map(H),
    file_map(T).
file_map(file(Src, Doc)) -->
    [ file(Src, Doc) ].
file_map(directory(_Dir, _Doc, Members, _Options)) -->
    file_map(Members).



%!  document_file(+File, -DocFile, +Options) is semidet.
%
%   DocFile is the file into which to write the documentation for
%   File.  File must be a canonical Prolog source-file.

document_file(File, DocFile, Options) :-
    (   option(if(loaded), Options, loaded)
    ->  (   source_file(File)
        ->  true
        ;   exists_directory(File),
            source_file(SrcFile),
            sub_atom(SrcFile, 0, _, _, File)
        ->  true
        )
    ;   true
    ),
    option(format(Format), Options, html),
    doc_extension(Format, Ext),
    (   exists_directory(File)
    ->  option(index_file(Index), Options, index),
        atomic_list_concat([File, /, Index, '.', Ext], DocFile0)
    ;   file_name_extension(Base, _, File),
        file_name_extension(Base, Ext, DocFile0)
    ),
    (   option(doc_root(Dir0), Options),
        ensure_slash(Dir0, Dir)
    ->  (   option(source_root(SrcTop), Options)
        ->  true
        ;   working_directory(SrcTop, SrcTop)
        ),
        directory_file_path(SrcTop, Local, DocFile0),
        directory_file_path(Dir, Local, DocFile),
        file_directory_name(DocFile, DocDir),
        ensure_dir(DocDir, Options)
    ;   DocFile = DocFile0
    ).


%!  doc_extension(+Format, -Extension) is det.

doc_extension(html, html).
doc_extension(latex, tex).


%!  ensure_slash(+DirName, -WithSlash) is det.
%
%   Ensure WithSlash ends with a /.

ensure_slash(DirName, WithSlash) :-
    (   sub_atom(DirName, _, _, 0, /)
    ->  WithSlash = DirName
    ;   atom_concat(DirName, /, WithSlash)
    ).


%!  ensure_dir(+Directory, +Options) is det.
%
%   Create Directory as mkdir -p.  May generate file errors.

ensure_dir(Directory, _Options) :-
    exists_directory(Directory),
    !.
ensure_dir(Directory, Options) :-
    file_directory_name(Directory, Parent),
    Parent \== Directory,
    ensure_dir(Parent, Options),
    make_directory(Directory).


%!  prolog_file_in_dir(+Dir, -File, +Options) is nondet.
%
%   File is a file in Dir that must be documented.  Options:
%
%           * recursive(+Bool)
%           If =true=, also generate subdirectories

prolog_file_in_dir(Dir, File, Options) :-
    (   option(if(loaded), Options, loaded)
    ->  source_file(File),
        file_directory_name(File, Dir)
    ;   user:prolog_file_type(Ext, prolog),
        \+ user:prolog_file_type(Ext, qlf),
        atomic_list_concat([Dir, '/*.', Ext], Pattern),
        expand_file_name(Pattern, Files),
        member(File, Files)
    ),
    file_base_name(File, Base),
    \+ blocked(Base).
prolog_file_in_dir(Dir, SubDir, Options) :-
    option(recursive(true), Options, false),
    option(doc_root(DocRoot), Options),
    atom_concat(Dir, '/*', Pattern),
    expand_file_name(Pattern, Matches),
    member(SubDir, Matches),
    SubDir \== DocRoot,
    exists_directory(SubDir).

%!  blocked(+File) is semidet.
%
%   True if File is blocked from documentation.

blocked('.plrc').
blocked('INDEX.pl').


                 /*******************************
                 *           RESOURCES          *
                 *******************************/

%!  copy_resources(+Dir, +Options)

copy_resources(Dir, Options) :-
    option(format(Format), Options, html),
    forall(doc_resource(Format, Res),
           ( absolute_file_name(pldoc(Res), File, [access(read)]),
             copy_file(File, Dir))).

doc_resource(html, 'pldoc.css').
doc_resource(html, 'h1-bg.png').
doc_resource(html, 'h2-bg.png').
doc_resource(html, 'multi-bg.png').
doc_resource(html, 'priv-bg.png').
doc_resource(html, 'pub-bg.png').
