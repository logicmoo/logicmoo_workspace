/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2006-2020, University of Amsterdam
                              VU University Amsterdam
                              CWI, Amsterdam
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

:- module(pldoc_html,
          [ doc_for_file/2,             % +FileSpec, +Options
            doc_write_html/3,           % +Stream, +Title, +Term
            doc_for_wiki_file/2,        % +FileSpec, +Options
                                        % Support doc_index
            doc_page_dom/3,             % +Title, +Body, -DOM
            print_html_head/1,          % +Stream
            predref//1,                 % +PI //
            predref//2,                 % +PI, Options //
            module_info/3,              % +File, +Options0, -Options
            doc_hide_private/3,         % +Doc0, -Doc, +Options
            edit_button//2,             % +File, +Options, //
            source_button//2,           % +File, +Options, //
            zoom_button//2,             % +File, +Options, //
            pred_edit_button//2,        % +PredInd, +Options, //
            object_edit_button//2,      % +Obj, +Options, //
            object_source_button//2,    % +Obj, +Options, //
            doc_resources//1,           % +Options
            ensure_doc_objects/1,       % +File
                                        % Support other backends
            doc_file_objects/5,         % +FSpec, -File, -Objs, -FileOpts, +Opts
            existing_linked_file/2,     % +FileSpec, -Path
            unquote_filespec/2,         % +FileSpec, -Unquoted
            doc_tag_title/2,            % +Tag, -Title
            mode_anchor_name/2,         % +Mode, -Anchor
            pred_anchor_name/3,         % +Head, -PI, -Anchor
            private/2,                  % +Obj, +Options
            (multifile)/2,              % +Obj, +Options
            is_pi/1,                    % @Term
            is_op_type/2,               % +Atom, ?Type
                                        % Output routines
            file//1,                    % +File, //
            file//2,                    % +File, +Options, //
            include//3,                 % +File, +Type, +Options //
            tags//1,                    % +Tags, //
            term//3,                    % +Text, +Term, +Bindings, //
            file_header//2,             % +File, +Options, //
            objects//2,                 % +Objects, +Options, //
            object_ref//2,              % +Object, +Options, //
            object_name//2,             % +Object, +Object
            object_href/2,              % +Object, -URL
            object_tree//3,             % +Tree, +Current, +Options
            object_page//2,             % +Object, +Options, //
            object_page_header//2,      % +File, +Options, //
            object_synopsis//2,         % +Object, +Options, //
            object_page_footer//2       % +Object, +Options, //
          ]).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(uri)).
:- use_module(library(readutil)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_path)).
:- use_module(library(http/html_head)).
:- use_module(library(http/term_html)).
:- use_module(library(http/jquery)).
:- use_module(library(debug)).
:- use_module(library(apply)).
:- use_module(library(pairs)).
:- use_module(library(filesex)).
:- use_module(doc_process).
:- use_module(doc_man).
:- use_module(doc_modes).
:- use_module(doc_wiki).
:- use_module(doc_search).
:- use_module(doc_index).
:- use_module(doc_util).
:- use_module(library(solution_sequences)).
:- use_module(library(error)).
:- use_module(library(occurs)).
:- use_module(library(prolog_source)).
:- use_module(library(prolog_xref)).

:- include(hooks).


/** <module> PlDoc HTML backend

This  module  translates  the  Herbrand   term  from  the  documentation
extracting module doc_wiki.pl into HTML+CSS.

@tbd    Split put generation from computation as computation is reusable
        in other backends.
*/

:- public
    args//1,                        % Called from \Term output created
    pred_dt//3,                     % by the wiki renderer
    section//2,
    tag//2.


:- predicate_options(doc_for_wiki_file/2, 2,
                     [ edit(boolean)
                     ]).
:- predicate_options(doc_hide_private/3, 3,
                     [module(atom), public(list), public_only(boolean)]).
:- predicate_options(edit_button//2, 2,
                     [ edit(boolean)
                     ]).
:- predicate_options(file//2, 2,
                     [ label(any),
                       absolute_path(atom),
                       href(atom),
                       map_extension(list),
                       files(list),
                       edit_handler(atom)
                     ]).
:- predicate_options(file_header//2, 2,
                     [ edit(boolean),
                       files(list),
                       public_only(boolean)
                     ]).
:- predicate_options(include//3, 3,
                     [ absolute_path(atom),
                       class(atom),
                       files(list),
                       href(atom),
                       label(any),
                       map_extension(list)
                     ]).
:- predicate_options(object_edit_button//2, 2,
                     [ edit(boolean),
                       pass_to(pred_edit_button//2, 2)
                     ]).
:- predicate_options(object_page//2, 2,
                     [ for(any),
                       header(boolean),
                       links(boolean),
                       no_manual(boolean),
                       try_manual(boolean),
                       search_in(oneof([all,app,man])),
                       search_match(oneof([name,summary])),
                       search_options(boolean)
                     ]).
:- predicate_options(object_ref//2, 2,
                     [ files(list),
                       qualify(boolean),
                       style(oneof([number,title,number_title])),
                       secref_style(oneof([number,title,number_title]))
                     ]).
:- predicate_options(object_synopsis//2, 2,
                     [ href(atom)
                     ]).
:- predicate_options(pred_dt//3, 3,
                     [ edit(boolean)
                     ]).
:- predicate_options(pred_edit_button//2, 2,
                     [ edit(boolean)
                     ]).
:- predicate_options(predref//2, 2,
                     [ files(list),
                       prefer(oneof([manual,app])),
                       pass_to(object_ref/4, 2)
                     ]).
:- predicate_options(private/2, 2,
                     [ module(atom),
                       public(list)
                     ]).
:- predicate_options(source_button//2, 2,
                     [ files(list)
                     ]).


                 /*******************************
                 *           RESOURCES          *
                 *******************************/

:- html_resource(pldoc_css,
                 [ virtual(true),
                   requires([ pldoc_resource('pldoc.css')
                            ])
                 ]).
:- html_resource(pldoc_resource('pldoc.js'),
                 [ requires([ jquery
                            ])
                 ]).
:- html_resource(pldoc_js,
                 [ virtual(true),
                   requires([ pldoc_resource('pldoc.js')
                            ])
                 ]).
:- html_resource(pldoc,
                 [ virtual(true),
                   requires([ pldoc_css,
                              pldoc_js
                            ])
                 ]).


                 /*******************************
                 *       FILE PROCESSING        *
                 *******************************/

%!  doc_for_file(+File, +Options) is det
%
%   HTTP  handler  that  writes  documentation  for  File  as  HTML.
%   Options:
%
%           * public_only(+Bool)
%           If =true= (default), only emit documentation for
%           exported predicates.
%
%           * edit(Bool)
%           If =true=, provide edit buttons. Default, these buttons
%           are suppressed.
%
%           * title(+Title)
%           Specify the page title.  Default is the base name of the
%           file.
%
%   @param File     Prolog file specification or xref source id.

doc_for_file(FileSpec, Options) :-
    doc_file_objects(FileSpec, File, Objects, FileOptions, Options),
    doc_file_title(File, Title, FileOptions, Options),
    doc_write_page(
        pldoc(file(File, Title)),
        title(Title),
        \prolog_file(File, Objects, FileOptions, Options),
        Options).

doc_file_title(_, Title, _, Options) :-
    option(title(Title), Options),
    !.
doc_file_title(File, Title, FileOptions, _) :-
    memberchk(file(Title0, _Comment), FileOptions),
    !,
    file_base_name(File, Base),
    atomic_list_concat([Base, ' -- ', Title0], Title).
doc_file_title(File, Title, _, _) :-
    file_base_name(File, Title).

:- html_meta doc_write_page(+, html, html, +).

doc_write_page(Style, Head, Body, Options) :-
    option(files(_), Options),
    !,
    phrase(page(Style, Head, Body), HTML),
    print_html(HTML).
doc_write_page(Style, Head, Body, _) :-
    reply_html_page(Style, Head, Body).


prolog_file(File, Objects, FileOptions, Options) -->
    { b_setval(pldoc_file, File),   % TBD: delete?
      file_directory_name(File, Dir)
    },
    html([ \doc_resources(Options),
           \doc_links(Dir, FileOptions),
           \file_header(File, FileOptions)
         | \objects(Objects, FileOptions)
         ]),
    undocumented(File, Objects, FileOptions).

%!  doc_resources(+Options)// is det.
%
%   Include required resources (CSS, JS) into  the output. The first
%   clause supports doc_files.pl. A bit hacky ...

doc_resources(Options) -->
    { option(resource_directory(ResDir), Options),
      nb_current(pldoc_output, OutputFile),
      !,
      directory_file_path(ResDir, 'pldoc.css', Res),
      relative_file_name(Res, OutputFile, Ref)
    },
    html_requires(Ref).
doc_resources(Options) -->
    { option(html_resources(Resoures), Options, pldoc)
    },
    html_requires(Resoures).


%!  doc_file_objects(+FileSpec, -File, -Objects, -FileOptions, +Options) is det.
%
%   Extracts  relevant  information  for  FileSpec  from  the  PlDoc
%   database.  FileOptions contains:
%
%           * file(Title:string, Comment:string)
%           * module(Module:atom)
%           * public(Public:list(predicate_indicator)
%
%   Objects contains
%
%           * doc(PI:predicate_indicator, File:Line, Comment)
%
%   We distinguish three different states for FileSpec:
%
%     1. File was cross-referenced with collection enabled.  All
%        information is in the xref database.
%     2. File was loaded. If comments are not loaded,
%        cross-reference the file, while _storing_ the comments
%        as the compiler would do.
%     3. Neither of the above.  In this case we cross-reference the
%        file.
%
%   @param FileSpec File specification as used for load_files/2.
%   @param File     Prolog canonical filename

doc_file_objects(FileSpec, File, Objects, FileOptions, Options) :-
    xref_current_source(FileSpec),
    xref_option(FileSpec, comments(collect)),
    !,
    File = FileSpec,
    findall(Object, xref_doc_object(File, Object), Objects0),
    reply_file_objects(File, Objects0, Objects, FileOptions, Options).
doc_file_objects(FileSpec, File, Objects, FileOptions, Options) :-
    absolute_file_name(FileSpec, File,
                       [ file_type(prolog),
                         access(read)
                       ]),
    source_file(File),
    !,
    ensure_doc_objects(File),
    Pos = File:Line,
    findall(Line-doc(Obj,Pos,Comment),
            doc_comment(Obj, Pos, _, Comment), Pairs),
    sort(Pairs, Pairs1),            % remove duplicates
    keysort(Pairs1, ByLine),
    pairs_values(ByLine, Objs0),
    reply_file_objects(File, Objs0, Objects, FileOptions, Options).
doc_file_objects(FileSpec, File, Objects, FileOptions, Options) :-
    absolute_file_name(FileSpec, File,
                       [ file_type(prolog),
                         access(read)
                       ]),
    xref_source(File, [silent(true)]),
    findall(Object, xref_doc_object(File, Object), Objects0),
    reply_file_objects(File, Objects0, Objects, FileOptions, Options).


reply_file_objects(File, Objs0, Objects, FileOptions, Options) :-
    module_info(File, ModuleOptions, Options),
    file_info(Objs0, Objs1, FileOptions, ModuleOptions),
    doc_hide_private(Objs1, ObjectsSelf, ModuleOptions),
    include_reexported(ObjectsSelf, Objects1, File, FileOptions),
    remove_doc_duplicates(Objects1, Objects, []).

remove_doc_duplicates([], [], _).
remove_doc_duplicates([H|T0], [H|T], Seen) :-
    H = doc(_, _, Comment),
    \+ memberchk(Comment, Seen),
    !,
    remove_doc_duplicates(T0, T, [Comment|Seen]).
remove_doc_duplicates([_|T0], T, Seen) :-
    remove_doc_duplicates(T0, T, Seen).

include_reexported(SelfObjects, Objects, File, Options) :-
    option(include_reexported(true), Options),
    option(module(Module), Options),
    option(public(Exports), Options),
    select_undocumented(Exports, Module, SelfObjects, Undoc),
    re_exported_doc(Undoc, File, Module, REObjs, _),
    REObjs \== [],
    !,
    append(SelfObjects, REObjs, Objects).
include_reexported(Objects, Objects, _, _).


%!  xref_doc_object(File, DocObject) is nondet.

xref_doc_object(File, doc(M:module(Title),File:0,Comment)) :-
    xref_comment(File, Title, Comment),
    xref_module(File, M).
xref_doc_object(File, doc(M:Name/Arity,File:0,Comment)) :-
    xref_comment(File, Head, _Summary, Comment),
    xref_module(File, Module),
    strip_module(Module:Head, M, Plain),
    functor(Plain, Name, Arity).

%!  ensure_doc_objects(+File) is det.
%
%   Ensure we have documentation about File.  If we have no comments
%   for the file because it was loaded before comment collection was
%   enabled, run the cross-referencer on it  to collect the comments
%   and meta-information.
%
%   @param File is a canonical filename that is loaded.

:- dynamic
    no_comments/2.

ensure_doc_objects(File) :-
    source_file(File),
    !,
    (   doc_file_has_comments(File)
    ->  true
    ;   no_comments(File, TimeChecked),
        time_file(File, TimeChecked)
    ->  true
    ;   xref_source(File, [silent(true), comments(store)]),
        retractall(no_comments(File, _)),
        (   doc_file_has_comments(File)
        ->  true
        ;   time_file(File, TimeChecked),
            assertz(no_comments(File, TimeChecked))
        )
    ).
ensure_doc_objects(File) :-
    xref_source(File, [silent(true)]).

%!  module_info(+File, -ModuleOptions, +OtherOptions) is det.
%
%   Add options module(Name),  public(Exports)   to  OtherOptions if
%   File is a module file.

module_info(File, [module(Module), public(Exports)|Options], Options) :-
    module_property(Module, file(File)),
    !,
    module_property(Module, exports(Exports)).
module_info(File, [module(Module), public(Exports)|Options], Options) :-
    xref_module(File, Module),
    !,
    findall(PI, xref_exported_pi(File, PI), Exports).
module_info(_, Options, Options).

xref_exported_pi(Src, Name/Arity) :-
    xref_exported(Src, Head),
    functor(Head, Name, Arity).

%!  doc_hide_private(+Objs, +Public, +Options)
%
%   Remove the private objects from Objs according to Options.

doc_hide_private(Objs, Objs, Options) :-
    option(public_only(false), Options, true),
    !.
doc_hide_private(Objs0, Objs, Options) :-
    hide_private(Objs0, Objs, Options).

hide_private([], [], _).
hide_private([H|T0], T, Options) :-
    obj(H, Obj),
    private(Obj, Options),
    !,
    hide_private(T0, T, Options).
hide_private([H|T0], [H|T], Options) :-
    hide_private(T0, T, Options).

%!  obj(+Term, -Object) is det.
%
%   Extract the documented  object  from   its  environment.  It  is
%   assumed to be the first term. Note  that if multiple objects are
%   described by the same comment Term is a list.

obj(doc(Obj0, _Pos, _Summary), Obj) :-
    !,
    (   Obj0 = [Obj|_]
    ->  true
    ;   Obj = Obj0
    ).
obj(Obj0, Obj) :-
    (   Obj0 = [Obj|_]
    ->  true
    ;   Obj = Obj0
    ).


%!  private(+Obj, +Options) is semidet.
%
%   True if Obj is not  exported   from  Options. This means Options
%   defined a module and Obj is  not   member  of the exports of the
%   module.

:- multifile
    prolog:doc_is_public_object/1.

private(Object, _Options):-
    prolog:doc_is_public_object(Object), !, fail.
private(Module:PI, Options) :-
    multifile(Module:PI, Options), !, fail.
private(Module:PI, Options) :-
    option(module(Module), Options),
    option(public(Public), Options),
    !,
    \+ ( member(PI2, Public),
         eq_pi(PI, PI2)
       ).
private(Module:PI, _Options) :-
    module_property(Module, file(_)),      % A loaded module
    !,
    module_property(Module, exports(Exports)),
    \+ ( member(PI2, Exports),
         eq_pi(PI, PI2)
       ).
private(Module:PI, _Options) :-
    \+ (pi_to_head(PI, Head),
        xref_exported(Source, Head),
        xref_module(Source, Module)).

%!  prolog:doc_is_public_object(+Object) is semidet.
%
%   Hook that allows objects  to  be   displayed  with  the  default
%   public-only view.

%!  multifile(+Obj, +Options) is semidet.
%
%   True if Obj is a multifile predicate.

multifile(Obj, _Options) :-
    strip_module(user:Obj, Module, PI),
    pi_to_head(PI, Head),
    (   predicate_property(Module:Head, multifile)
    ;   xref_module(Source, Module),
        xref_defined(Source, Head, multifile(_))
    ),
    !.

pi_to_head(Var, _) :-
    var(Var), !, fail.
pi_to_head(Name/Arity, Term) :-
    functor(Term, Name, Arity).
pi_to_head(Name//DCGArity, Term) :-
    Arity is DCGArity+2,
    functor(Term, Name, Arity).

%!  file_info(+Comments, -RestComment, -FileOptions, +OtherOptions) is det.
%
%   Add options file(Title, Comment) to OtherOptions if available.

file_info(Comments, RestComments, [file(Title, Comment)|Opts], Opts) :-
    select(doc(_:module(Title),_,Comment), Comments, RestComments),
    !.
file_info(Comments, Comments, Opts, Opts).


%!  file_header(+File, +Options)// is det.
%
%   Create the file header.

file_header(File, Options) -->
    { memberchk(file(Title, Comment), Options),
      !,
      file_base_name(File, Base)
    },
    file_title([Base, ' -- ', Title], File, Options),
    { is_structured_comment(Comment, Prefixes),
      string_codes(Comment, Codes),
      indented_lines(Codes, Prefixes, Lines),
      section_comment_header(Lines, _Header, Lines1),
      wiki_lines_to_dom(Lines1, [], DOM)
    },
    html(DOM).
file_header(File, Options) -->
    { file_base_name(File, Base)
    },
    file_title([Base], File, Options).


%!  file_title(+Title:list, +File, +Options)// is det
%
%   Emit the file-header and manipulation buttons.

file_title(Title, File, Options) -->
    prolog:doc_file_title(Title, File, Options),
    !.
file_title(Title, File, Options) -->
    { file_base_name(File, Base)
    },
    html(h1(class(file),
            [ span(style('float:right'),
                   [ \reload_button(File, Base, Options),
                     \zoom_button(Base, Options),
                     \source_button(Base, Options),
                     \edit_button(File, Options)
                   ])
            | Title
            ])).


%!  reload_button(+File, +Base, +Options)// is det.
%
%   Create a button for  reloading  the   sources  and  updating the
%   documentation page. Note that the  button   is  not shown if the
%   file is not loaded because we do  not want to load files through
%   the documentation system.

reload_button(File, _Base, Options) -->
    { \+ source_file(File),
      \+ option(files(_), Options)
    },
    !,
    html(span(class(file_anot), '[not loaded]')).
reload_button(_File, Base, Options) -->
    { option(edit(true), Options),
      !,
      option(public_only(Public), Options, true)
    },
    html(a(href(Base+[reload(true), public_only(Public)]),
           img([ class(action),
                 alt('Reload'),
                 title('Make & Reload'),
                 src(location_by_id(pldoc_resource)+'reload.png')
               ]))).
reload_button(_, _, _) --> [].

%!  edit_button(+File, +Options)// is det.
%
%   Create an edit button  for  File.   If  the  button  is clicked,
%   JavaScript sends a message to the   server without modifying the
%   current page.  JavaScript code is in the file pldoc.js.

edit_button(File, Options) -->
    { option(edit(true), Options)
    },
    !,
    html(a([ onClick('HTTPrequest(\'' +
                     location_by_id(pldoc_edit) + [file(File)] +
                     '\')')
           ],
           img([ class(action),
                 alt(edit),
                 title('Edit file'),
                 src(location_by_id(pldoc_resource)+'edit.png')
             ]))).
edit_button(_, _) -->
    [].


%!  zoom_button(BaseName, +Options)// is det.
%
%   Add zoom in/out button to show/hide the private documentation.

zoom_button(_, Options) -->
    { option(files(_Map), Options) },
    !.    % generating files
zoom_button(Base, Options) -->
    {   (   option(public_only(true), Options, true)
        ->  Zoom = 'public.png',
            Alt = 'Public',
            Title = 'Click to include private',
            PublicOnly = false
        ;   Zoom = 'private.png',
            Alt = 'All predicates',
            Title = 'Click to show exports only',
            PublicOnly = true
        )
    },
    html(a(href(Base+[public_only(PublicOnly)]),
           img([ class(action),
                 alt(Alt),
                 title(Title),
                 src(location_by_id(pldoc_resource)+Zoom)
               ]))).


%!  source_button(+File, +Options)// is det.
%
%   Add show-source button.

source_button(_File, Options) -->
    { option(files(_Map), Options) },
    !.    % generating files
source_button(File, _Options) -->
    { (   is_absolute_file_name(File)
      ->  doc_file_href(File, HREF0)
      ;   HREF0 = File
      )
    },
    html(a(href(HREF0+[show(src)]),
           img([ class(action),
                 alt('Show source'),
                 title('Show source'),
                 src(location_by_id(pldoc_resource)+'source.png')
               ]))).


%!  objects(+Objects:list, +Options)// is det.
%
%   Emit the documentation body.  Options includes:
%
%     * navtree(+Boolean)
%     If =true=, provide a navitation tree.

objects(Objects, Options) -->
    { option(navtree(true), Options),
      !,
      objects_nav_tree(Objects, Tree)
    },
    html([ div(class(navtree),
               div(class(navwindow),
                   \nav_tree(Tree, Objects, Options))),
           div(class(navcontent),
               \objects_nt(Objects, Options))
         ]).
objects(Objects, Options) -->
    objects_nt(Objects, Options).

objects_nt(Objects, Options) -->
    objects(Objects, [body], Options).

objects([], Mode, _) -->
    pop_mode(body, Mode, _).
objects([Obj|T], Mode, Options) -->
    object(Obj, Mode, Mode1, Options),
    objects(T, Mode1, Options).

%!  object(+Spec, +ModeIn, -ModeOut, +Options)// is det.
%
%   Emit the documentation of a single object.
%
%   @param  Spec is one of doc(Obj,Pos,Comment), which is used
%           to list the objects documented in a file or a plain
%           Obj, used for documenting the object regardless of
%           its location.

object(doc(Obj,Pos,Comment), Mode0, Mode, Options) -->
    !,
    object(Obj, [Pos-Comment], Mode0, Mode, [scope(file)|Options]).
object(Obj, Mode0, Mode, Options) -->
    { findall(Pos-Comment,
              doc_comment(Obj, Pos, _Summary, Comment),
              Pairs)
    },
    !,
    { b_setval(pldoc_object, Obj) },
    object(Obj, Pairs, Mode0, Mode, Options).

object(Obj, Pairs, Mode0, Mode, Options) -->
    { is_pi(Obj),
      !,
      maplist(pred_dom(Obj, Options), Pairs, DOMS),
      append(DOMS, DOM)
    },
    need_mode(dl, Mode0, Mode),
    html(DOM).
object([Obj|_Same], Pairs, Mode0, Mode, Options) -->
    !,
    object(Obj, Pairs, Mode0, Mode, Options).
object(Obj, _Pairs, Mode, Mode, _Options) -->
    { debug(pldoc, 'Skipped ~p', [Obj]) },
    [].

pred_dom(Obj, Options, Pos-Comment, DOM) :-
    is_structured_comment(Comment, Prefixes),
    string_codes(Comment, Codes),
    indented_lines(Codes, Prefixes, Lines),
    strip_module(user:Obj, Module, _),
    process_modes(Lines, Module, Pos, Modes, Args, Lines1),
    (   private(Obj, Options)
    ->  Class = privdef             % private definition
    ;   multifile(Obj, Options)
    ->  (   option(scope(file), Options)
        ->  (   more_doc(Obj, Pos)
            ->  Class = multidef(object(Obj))
            ;   Class = multidef
            )
        ;   Class = multidef(file((Pos)))
        )
    ;   Class = pubdef              % public definition
    ),
    (   Obj = Module:_
    ->  POptions = [module(Module)|Options]
    ;   POptions = Options
    ),
    Pos = File:Line,
    DTOptions = [file(File),line(Line)|POptions],
    DOM = [\pred_dt(Modes, Class, DTOptions), dd(class=defbody, DOM1)],
    wiki_lines_to_dom(Lines1, Args, DOM0),
    strip_leading_par(DOM0, DOM1).

more_doc(Obj, File:_) :-
    doc_comment(Obj, File2:_, _, _),
    File2 \== File,
    !.

%!  need_mode(+Mode:atom, +Stack:list, -NewStack:list)// is det.
%
%   While predicates are part of a   description  list, sections are
%   not and we therefore  need  to   insert  <dl>...</dl>  into  the
%   output. We do so by demanding  an outer environment and push/pop
%   the required elements.

need_mode(Mode, Stack, Stack) -->
    { Stack = [Mode|_] },
    !,
    [].
need_mode(Mode, Stack, Rest) -->
    { memberchk(Mode, Stack)
    },
    !,
    pop_mode(Mode, Stack, Rest).
need_mode(Mode, Stack, [Mode|Stack]) -->
    !,
    html_begin(Mode).

pop_mode(Mode, Stack, Stack) -->
    { Stack = [Mode|_] },
    !,
    [].
pop_mode(Mode, [H|Rest0], Rest) -->
    html_end(H),
    pop_mode(Mode, Rest0, Rest).

%!  undocumented(+File, +Objects, +Options)// is det.
%
%   Describe undocumented predicates if the file is a module file.

undocumented(File, Objs, Options) -->
    { memberchk(module(Module), Options),
      memberchk(public(Exports), Options),
      select_undocumented(Exports, Module, Objs, Undoc),
      re_exported_doc(Undoc, File, Module, REObjs, ReallyUnDoc)
    },
    !,
    re_exported_doc(REObjs, Options),
    undocumented(ReallyUnDoc, Options).
undocumented(_, _, _) -->
    [].

re_exported_doc([], _) --> !.
re_exported_doc(Objs, Options) -->
    reexport_header(Objs, Options),
    objects(Objs, Options).

reexport_header(_, Options) -->
    { option(reexport_header(true), Options, true)
    },
    !,
    html([ h2(class(wiki), 'Re-exported predicates'),
           p([ 'The following predicates are re-exported from other ',
               'modules'
             ])
         ]).
reexport_header(_, _) -->
    [].

undocumented([], _) --> !.
undocumented(UnDoc, Options) -->
    html([ h2(class(undoc), 'Undocumented predicates'),
           p(['The following predicates are exported, but not ',
              'or incorrectly documented.'
             ]),
           dl(class(undoc),
              \undocumented_predicates(UnDoc, Options))
         ]).


undocumented_predicates([], _) -->
    [].
undocumented_predicates([H|T], Options) -->
    undocumented_pred(H, Options),
    undocumented_predicates(T, Options).

undocumented_pred(Name/Arity, Options) -->
    { functor(Head, Name, Arity) },
    html(dt(class=undoc, \pred_mode(Head, [], _, Options))).

select_undocumented([], _, _, []).
select_undocumented([PI|T0], M, Objs, [PI|T]) :-
    is_pi(PI),
    \+ in_doc(M:PI, Objs),
    !,
    select_undocumented(T0, M, Objs, T).
select_undocumented([_|T0], M, Objs, T) :-
    select_undocumented(T0, M, Objs, T).

in_doc(PI, Objs) :-
    member(doc(O,_,_), Objs),
    (   is_list(O)
    ->  member(O2, O),
        eq_pi(PI, O2)
    ;   eq_pi(PI, O)
    ).


%!  eq_pi(PI1, PI2) is semidet.
%
%   True if PI1 and PI2 refer to the same predicate.

eq_pi(PI, PI) :- !.
eq_pi(M:PI1, M:PI2) :-
    atom(M),
    !,
    eq_pi(PI1, PI2).
eq_pi(Name/A, Name//DCGA) :-
    A =:= DCGA+2,
    !.
eq_pi(Name//DCGA, Name/A) :-
    A =:= DCGA+2.

%!  is_pi(@Term) is semidet.
%
%   True if Term is a predicate indicator.

is_pi(Var) :-
    var(Var),
    !,
    fail.
is_pi(_:PI) :-
    !,
    is_pi(PI).
is_pi(_/_).
is_pi(_//_).


%!  re_exported_doc(+Undoc:list(pi), +File:atom, +Module:atom,
%!                  -ImportedDoc, -ReallyUnDoc:list(pi))

re_exported_doc([], _, _, [], []).
re_exported_doc([PI|T0], File, Module, [doc(Orig:PI,Pos,Comment)|ObjT], UnDoc) :-
    pi_to_head(PI, Head),
    (   predicate_property(Module:Head, imported_from(Orig))
    ->  true
    ;   xref_defined(File, Head, imported(File2)),
        ensure_doc_objects(File2),
        xref_module(File2, Orig)
    ),
    doc_comment(Orig:PI, Pos, _, Comment),
    !,
    re_exported_doc(T0, File, Module, ObjT, UnDoc).
re_exported_doc([PI|T0], File, Module, REObj, [PI|UnDoc]) :-
    re_exported_doc(T0, File, Module, REObj, UnDoc).


                 /*******************************
                 *      SINGLE OBJECT PAGE      *
                 *******************************/

%!  object_page(+Obj, +Options)// is semidet.
%
%   Generate an HTML page describing Obj.  The top presents the file
%   the object is documented in and a search-form.  Options:
%
%       * header(+Boolean)
%       Show the navigation and search header.

object_page(Obj, Options) -->
    prolog:doc_object_page(Obj, Options),
    !,
    object_page_footer(Obj, Options).
object_page(Obj, Options) -->
    { doc_comment(Obj, File:_Line, _Summary, _Comment)
    },
    !,
    (   { \+ ( doc_comment(Obj, File2:_, _, _),
               File2 \== File )
        }
    ->  html([ \html_requires(pldoc),
               \object_page_header(File, Options),
               \object_synopsis(Obj, []),
               \objects([Obj], Options)
             ])
    ;   html([ \html_requires(pldoc),
               \object_page_header(-, Options),
               \objects([Obj], [synopsis(true)|Options])
             ])
    ),
    object_page_footer(Obj, Options).
object_page(M:Name/Arity, Options) -->          % specified module, but public
    { functor(Head, Name, Arity),
      (   predicate_property(M:Head, exported)
      ->  module_property(M, class(library))
      ;   \+ predicate_property(M:Head, defined)
      )
    },
    prolog:doc_object_page(Name/Arity, Options),
    !,
    object_page_footer(Name/Arity, Options).

object_page_header(File, Options) -->
    prolog:doc_page_header(file(File), Options),
    !.
object_page_header(File, Options) -->
    { option(header(true), Options, true) },
    !,
    html(div(class(navhdr),
             [ div(class(jump), \file_link(File)),
               div(class(search), \search_form(Options)),
               br(clear(right))
             ])).
object_page_header(_, _) --> [].

file_link(-) -->
    !,
    places_menu(-).
file_link(File) -->
    { file_directory_name(File, Dir)
    },
    places_menu(Dir),
    html([ div(a(href(location_by_id(pldoc_doc)+File), File))
         ]).

%!  object_page_footer(+Obj, +Options)// is det.
%
%   Call the hook prolog:doc_object_page_footer//2. This hook will
%   be used to deal with annotations.

object_page_footer(Obj, Options) -->
    prolog:doc_object_page_footer(Obj, Options).
object_page_footer(_, _) --> [].


%!  object_synopsis(Obj, Options)// is det.
%
%   Provide additional information  about  Obj.   Note  that  due to
%   reexport facilities, predicates may be   available from multiple
%   modules.
%
%   @tbd Currently we provide a synopsis   for the one where the
%   definition resides. This is not   always  correct. Notably there
%   are cases where multiple implementation modules are bundled in a
%   larger interface that is the `preferred' module.

object_synopsis(Name/Arity, _) -->
    { functor(Head, Name, Arity),
      predicate_property(system:Head, built_in)
    },
    synopsis([span(class(builtin), 'built-in')]).
object_synopsis(Name/Arity, Options) -->
    !,
    object_synopsis(_:Name/Arity, Options).
object_synopsis(M:Name/Arity, Options) -->
    { functor(Head, Name, Arity),
      (   option(source(Spec), Options)
      ->  absolute_file_name(Spec, File,
                             [ access(read),
                               file_type(prolog),
                               file_errors(fail)
                             ])
      ;   predicate_property(M:Head, exported),
          \+ predicate_property(M:Head, imported_from(_)),
          module_property(M, file(File)),
          file_name_on_path(File, Spec)
      ),
      !,
      unquote_filespec(Spec, Unquoted),
      (   predicate_property(Head, autoload(FileBase)),
          file_name_extension(FileBase, _Ext, File)
      ->  Extra = [span(class(autoload), '(can be autoloaded)')]
      ;   Extra = []
      )
    },
    (   { option(href(HREF), Options) }
    ->  synopsis([code([':- use_module(',a(href(HREF), '~q'-[Unquoted]),').'])|Extra])
    ;   synopsis([code(':- use_module(~q).'-[Unquoted])|Extra])
    ).
object_synopsis(Name//Arity, Options) -->
    !,
    { DCGArity is Arity+2 },
    object_synopsis(Name/DCGArity, Options).
object_synopsis(Module:Name//Arity, Options) -->
    !,
    { DCGArity is Arity+2 },
    object_synopsis(Module:Name/DCGArity, Options).
object_synopsis(f(_/_), _) -->
    synopsis(span(class(function),
                  [ 'Arithmetic function (see ',
                    \object_ref(is/2, []),
                    ')'
                  ])).
object_synopsis(c(Func), _) -->
    { sub_atom(Func, 0, _, _, 'PL_')
    },
    !,
    synopsis([span(class(cfunc), 'C-language interface function')]).
object_synopsis(_, _) --> [].

synopsis(Text) -->
    html(div(class(synopsis),
             [ span(class('synopsis-hdr'), 'Availability:')
             | Text
             ])).

%!  unquote_filespec(+Spec, -Unquoted) is det.
%
%   Translate       e.g.       library('semweb/rdf_db')         into
%   library(semweb/rdf_db).

unquote_filespec(Spec, Unquoted) :-
    compound(Spec),
    Spec =.. [Alias,Path],
    atom(Path),
    atomic_list_concat(Parts, /, Path),
    maplist(need_no_quotes, Parts),
    !,
    parts_to_path(Parts, UnquotedPath),
    Unquoted =.. [Alias, UnquotedPath].
unquote_filespec(Spec, Spec).

need_no_quotes(Atom) :-
    format(atom(A), '~q', [Atom]),
    \+ sub_atom(A, 0, _, _, '\'').

parts_to_path([One], One) :- !.
parts_to_path(List, More/T) :-
    (   append(H, [T], List)
    ->  parts_to_path(H, More)
    ).


                 /*******************************
                 *             PRINT            *
                 *******************************/

%!  doc_write_html(+Out:stream, +Title:atomic, +DOM) is det.
%
%   Write HTML for the documentation page DOM using Title to Out.

doc_write_html(Out, Title, Doc) :-
    doc_page_dom(Title, Doc, DOM),
    phrase(html(DOM), Tokens),
    print_html_head(Out),
    print_html(Out, Tokens).

%!  doc_page_dom(+Title, +Body, -DOM) is det.
%
%   Create the complete HTML DOM from the   Title  and Body. It adds
%   links to the style-sheet and javaScript files.

doc_page_dom(Title, Body, DOM) :-
    DOM = html([ head([ title(Title),
                        link([ rel(stylesheet),
                               type('text/css'),
                               href(location_by_id(pldoc_resource)+'pldoc.css')
                             ]),
                        script([ src(location_by_id(pldoc_resource)+'pldoc.js'),
                                 type('text/javascript')
                               ], [])
                      ]),
                 body(Body)
               ]).

%!  print_html_head(+Out:stream) is det.
%
%   Print the =DOCTYPE= line.

print_html_head(Out) :-
    format(Out,
           '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN" \c
               "http://www.w3.org/TR/html4/strict.dtd">~n', []).

% Rendering rules
%
% These rules translate \-terms produced by wiki.pl

%!  tags(+Tags)// is det.
%
%   Emit the @tag tags of a description. Tags is produced by tags/3.
%
%   @see combine_tags/2.

tags(Tags) -->
    html(dl(class=tags, Tags)).

%!  tag(+Tag, +Values:list)// is det.
%
%   Called from \tag(Name, Values) terms produced by doc_wiki.pl.

tag(Tag, Values) -->
    {   doc_tag_title(Tag, Title),
        atom_concat('keyword-', Tag, Class)
    },
    html([ dt(class=Class, Title),
           \tag_values(Values, Class)
         ]).

tag_values([], _) -->
    [].
tag_values([H|T], Class) -->
    html(dd(class=Class, ['- '|H])),
    tag_values(T, Class).


%!  doc_tag_title(+Tag, -Title) is det.
%
%   Title is the name to use for Tag in the generated documentation.

doc_tag_title(Tag, Title) :-
    tag_title(Tag, Title),
    !.
doc_tag_title(Tag, Tag).

tag_title(compat, 'Compatibility').
tag_title(tbd,    'To be done').
tag_title(see,    'See also').
tag_title(error,  'Errors').

%!  args(+Params:list) is det.
%
%   Called from \args(List) created by   doc_wiki.pl.  Params is a
%   list of arg(Name, Descr).

args(Params) -->
    html([ dt(class=tag, 'Arguments:'),
           dd(table(class=arglist,
                    \arg_list(Params)))
         ]).

arg_list([]) -->
    [].
arg_list([H|T]) -->
    argument(H),
    arg_list(T).

argument(arg(Name,Descr)) -->
    html(tr([td(var(Name)), td(class=argdescr, ['- '|Descr])])).


                 /*******************************
                 *         NAVIGATION TREE      *
                 *******************************/

%!  objects_nav_tree(+Objects, -Tree) is det.
%
%   Provide a navigation tree showing the context of Object.  Tree
%   is of the form node(Object, Children).

objects_nav_tree(Objects, Tree) :-
    maplist(object_nav_tree, Objects, Trees),
    union_trees(Trees, Tree0),
    remove_unique_root(Tree0, Tree).

object_nav_tree(Obj, Tree) :-
    Node = node(directory(Dir), FileNodes),
    FileNode = node(file(File), Siblings),
    doc_comment(Obj, File:_Line, _Summary, _Comment),
    !,
    file_directory_name(File, Dir),
    sibling_file_nodes(Dir, FileNodes0),
    selectchk(node(file(File),[]), FileNodes0, FileNode, FileNodes),
    findall(Sibling, doc_comment(Sibling, File:_, _, _), Siblings0),
    delete(Siblings0, _:module(_), Siblings1),
    doc_hide_private(Siblings1, Siblings2, []),
    flatten(Siblings2, Siblings),   % a comment may describe objects
    embed_directories(Node, Tree).

sibling_file_nodes(Dir, Nodes) :-
    findall(node(file(File), []),
            (   source_file(File),
                file_directory_name(File, Dir)
            ),
            Nodes).

embed_directories(Node, Tree) :-
    Node = node(file(File), _),
    !,
    file_directory_name(File, Dir),
    Super = node(directory(Dir), [Node]),
    embed_directories(Super, Tree).
embed_directories(Node, Tree) :-
    Node = node(directory(Dir), _),
    file_directory_name(Dir, SuperDir),
    SuperDir \== Dir,
    !,
    Super = node(directory(SuperDir), [Node]),
    embed_directories(Super, Tree).
embed_directories(Tree, Tree).


union_trees([Tree], Tree) :- !.
union_trees([T1,T2|Trees], Tree) :-
    merge_trees(T1, T2, M1),
    union_trees([M1|Trees], Tree).

merge_trees(node(R, Ch1), node(R, Ch2), node(R, Ch)) :-
    merge_nodes(Ch1, Ch2, Ch).

merge_nodes([], Ch, Ch) :- !.
merge_nodes(Ch, [], Ch) :- !.
merge_nodes([node(Root, Ch1)|T1], N1, [T1|Nodes]) :-
    selectchk(node(Root, Ch2), N1, N2),
    !,
    merge_trees(node(Root, Ch1), node(Root, Ch2), T1),
    merge_nodes(T1, N2, Nodes).
merge_nodes([Node|T1], N1, [Node|Nodes]) :-
    merge_nodes(T1, N1, Nodes).

%!  remove_unique_root(+TreeIn, -Tree)
%
%   Remove the root part that does not branch

remove_unique_root(node(_, [node(R1, [R2])]), Tree) :-
    !,
    remove_unique_root(node(R1, [R2]), Tree).
remove_unique_root(Tree, Tree).

%!  nav_tree(+Tree, +Current, +Options)// is det.
%
%   Render the navigation tree

nav_tree(Tree, Current, Options) -->
    html(ul(class(nav),
            \object_tree(Tree, Current, Options))).

%!  object_tree(+Tree, +Current, +Options)// is det.
%
%   Render a tree of objects used for navigation.

object_tree(node(Id, []), Target, Options) -->
    !,
    { node_class(Id, Target, Class) },
    html(li(class(Class),
            \node(Id, Options))).
object_tree(node(Id, Children), Target, Options) -->
    !,
    { node_class(Id, Target, Class) },
    html(li(class(Class),
            [ \node(Id, Options),
              ul(class(nav),
                 \object_trees(Children, Target, Options))
            ])).
object_tree(Id, Target, Options) -->
    !,
    { node_class(Id, Target, Class) },
    html(li(class([obj|Class]), \node(Id, Options))).

object_trees([], _, _) --> [].
object_trees([H|T], Target, Options) -->
    object_tree(H, Target, Options),
    object_trees(T, Target, Options).

node_class(Ids, Current, Class) :-
    is_list(Ids),
    !,
    (   member(Id, Ids), memberchk(Id, Current)
    ->  Class = [nav,current]
    ;   Class = [nav]
    ).
node_class(Id, Current, Class) :-
    (   memberchk(Id, Current)
    ->  Class = [nav,current]
    ;   Class = [nav]
    ).

node(file(File), Options) -->
    !,
    object_ref(file(File), [style(title)|Options]).
node(Id, Options) -->
    object_ref(Id, Options).


                 /*******************************
                 *            SECTIONS          *
                 *******************************/

section(Type, Title) -->
    { string_codes(Title, Codes),
      wiki_codes_to_dom(Codes, [], Content0),
      strip_leading_par(Content0, Content),
      make_section(Type, Content, HTML)
    },
    html(HTML).

make_section(module,  Title, h1(class=module,  Title)).
make_section(section, Title, h1(class=section, Title)).


                 /*******************************
                 *       PRED MODE HEADER       *
                 *******************************/

%!  pred_dt(+Modes, +Class, Options)// is det.
%
%   Emit the predicate header.
%
%   @param Modes    List as returned by process_modes/5.

pred_dt(Modes, Class, Options) -->
    pred_dt(Modes, Class, [], _Done, Options).

pred_dt([], _, Done, Done, _) -->
    [].
pred_dt([H|T], Class, Done0, Done, Options) -->
    { functor(Class, CSSClass, _) },
    html(dt(class=CSSClass,
            [ \pred_mode(H, Done0, Done1, Options),
              \mode_anot(Class)
            ])),
    pred_dt(T, Class, Done1, Done, Options).

mode_anot(privdef) -->
    !,
    html(span([class(anot), style('float:right')],
              '[private]')).
mode_anot(multidef(object(Obj))) -->
    !,
    { object_href(Obj, HREF) },
    html(span([class(anot), style('float:right')],
              ['[', a(href(HREF), multifile), ']'
              ])).
mode_anot(multidef(file(File:_))) -->
    !,
    { file_name_on_path(File, Spec),
      unquote_filespec(Spec, Unquoted),
      doc_file_href(File, HREF)
    },
    html(span([class(anot), style('float:right')],
              ['[multifile, ', a(href(HREF), '~q'-[Unquoted]), ']'
              ])).
mode_anot(multidef) -->
    !,
    html(span([class(anot), style('float:right')],
              '[multifile]')).
mode_anot(_) -->
    [].

pred_mode(mode(Head,Vars), Done0, Done, Options) -->
    !,
    { bind_vars(Head, Vars) },
    pred_mode(Head, Done0, Done, Options).
pred_mode(Head is Det, Done0, Done, Options) -->
    !,
    anchored_pred_head(Head, Done0, Done, Options),
    pred_det(Det).
pred_mode(Head, Done0, Done, Options) -->
    anchored_pred_head(Head, Done0, Done, Options).

bind_vars(Term, Bindings) :-
    bind_vars(Bindings),
    anon_vars(Term).

bind_vars([]).
bind_vars([Name=Var|T]) :-
    Var = '$VAR'(Name),
    bind_vars(T).

%!  anon_vars(+Term) is det.
%
%   Bind remaining variables in Term to '$VAR'('_'), so they are
%   printed as '_'.

anon_vars(Var) :-
    var(Var),
    !,
    Var = '$VAR'('_').
anon_vars(Term) :-
    compound(Term),
    !,
    Term =.. [_|Args],
    maplist(anon_vars, Args).
anon_vars(_).


anchored_pred_head(Head, Done0, Done, Options) -->
    { pred_anchor_name(Head, PI, Name) },
    (   { memberchk(PI, Done0) }
    ->  { Done = Done0 },
        pred_head(Head)
    ;   html([ span(style('float:right'),
                    [ \pred_edit_or_source_button(Head, Options),
                      &(nbsp)
                    ]),
               a(name=Name, \pred_head(Head))
             ]),
        { Done = [PI|Done0] }
    ).


pred_edit_or_source_button(Head, Options) -->
    { option(edit(true), Options) },
    !,
    pred_edit_button(Head, Options).
pred_edit_or_source_button(Head, Options) -->
    { option(source_link(true), Options) },
    !,
    pred_source_button(Head, Options).
pred_edit_or_source_button(_, _) --> [].

%!  pred_edit_button(+PredIndicator, +Options)// is det.
%
%   Create a button for editing the given predicate.  Options
%   processed:
%
%       * module(M)
%       Resolve to module M
%       * file(F)
%       For multi-file predicates: link to version in file.
%       * line(L)
%       Line to edit (in file)

pred_edit_button(_, Options) -->
    { \+ option(edit(true), Options) },
    !.
pred_edit_button(PI0, Options0) -->
    { canonicalise_predref(PI0, PI, Options0, Options) },
    pred_edit_button2(PI, Options).

pred_edit_button2(Name/Arity, Options) -->
    { \+ ( memberchk(file(_), Options), % always edit if file and line
           memberchk(line(_), Options)  % are given.
         ),
      functor(Head, Name, Arity),
      option(module(M), Options, _),
      \+ ( current_module(M),
           source_file(M:Head, _File)
         )
    },
    !.
pred_edit_button2(Name/Arity, Options) -->
    { include(edit_param, Options, Extra),
      http_link_to_id(pldoc_edit,
                      [name(Name),arity(Arity)|Extra],
                      EditHREF)
    },
    html(a(onClick('HTTPrequest(\'' + EditHREF + '\')'),
           img([ class(action),
                 alt('Edit predicate'),
                 title('Edit predicate'),
                 src(location_by_id(pldoc_resource)+'editpred.png')
               ]))).
pred_edit_button2(_, _) -->
    !,
    [].

edit_param(module(_)).
edit_param(file(_)).
edit_param(line(_)).


%!  object_edit_button(+Object, +Options)// is det.
%
%   Create a button for editing Object.

object_edit_button(_, Options) -->
    { \+ option(edit(true), Options) },
    !.
object_edit_button(PI, Options) -->
    { is_pi(PI) },
    !,
    pred_edit_button(PI, Options).
object_edit_button(_, _) -->
    [].


%!  pred_source_button(+PredIndicator, +Options)// is det.
%
%   Create a button for viewing the source of a predicate.

pred_source_button(PI0, Options0) -->
    { canonicalise_predref(PI0, PI, Options0, Options),
      option(module(M), Options, _),
      pred_source_href(PI, M, HREF), !
    },
    html(a([ href(HREF)
           ],
           img([ class(action),
                 alt('Source'),
                 title('Show source'),
                 src(location_by_id(pldoc_resource)+'source.png')
               ]))).
pred_source_button(_, _) -->
    [].


%!  object_source_button(+Object, +Options)// is det.
%
%   Create a button for showing the source of Object.

object_source_button(PI, Options) -->
    { is_pi(PI),
      option(source_link(true), Options, true)
    },
    !,
    pred_source_button(PI, Options).
object_source_button(_, _) -->
    [].


%!  canonicalise_predref(+PredRef, -PI:Name/Arity, +Options0, -Options) is det.
%
%   Canonicalise a predicate reference. A   possible module qualifier is
%   added as module(M) to Options.

canonicalise_predref(M:PI0, PI, Options0, [module(M)|Options]) :-
    !,
    canonicalise_predref(PI0, PI, Options0, Options).
canonicalise_predref(//(Head), PI, Options0, Options) :-
    !,
    functor(Head, Name, Arity),
    PredArity is Arity + 2,
    canonicalise_predref(Name/PredArity, PI, Options0, Options).
canonicalise_predref(Name//Arity, PI, Options0, Options) :-
    integer(Arity), Arity >= 0,
    !,
    PredArity is Arity + 2,
    canonicalise_predref(Name/PredArity, PI, Options0, Options).
canonicalise_predref(PI, PI, Options, Options) :-
    PI = Name/Arity,
    atom(Name), integer(Arity), Arity >= 0,
    !.
canonicalise_predref(Head, PI, Options0, Options) :-
    functor(Head, Name, Arity),
    canonicalise_predref(Name/Arity, PI, Options0, Options).


%!  pred_head(+Term) is det.
%
%   Emit a predicate head. The functor is  typeset as a =span= using
%   class =pred= and the arguments and =var= using class =arglist=.

pred_head(Var) -->
    { var(Var),
      !,
      instantiation_error(Var)
    }.
pred_head(//(Head)) -->
    !,
    pred_head(Head),
    html(//).
pred_head(M:Head) -->
    html([span(class=module, M), :]),
    pred_head(Head).
pred_head(Head) -->
    { atom(Head) },
    !,
    html(b(class=pred, Head)).
pred_head(Head) -->                     % Infix operators
    { Head =.. [Functor,Left,Right],
      is_op_type(Functor, infix)
    },
    !,
    html([ var(class=arglist, \pred_arg(Left, 1)),
           ' ', b(class=pred, Functor), ' ',
           var(class=arglist, \pred_arg(Right, 2))
         ]).
pred_head(Head) -->                     % Prefix operators
    { Head =.. [Functor,Arg],
      is_op_type(Functor, prefix)
    },
    !,
    html([ b(class=pred, Functor), ' ',
           var(class=arglist, \pred_arg(Arg, 1))
         ]).
pred_head(Head) -->                     % Postfix operators
    { Head =.. [Functor,Arg],
      is_op_type(Functor, postfix)
    },
    !,
    html([ var(class=arglist, \pred_arg(Arg, 1)),
           ' ', b(class=pred, Functor)
         ]).
pred_head({Head}) -->
    !,
    html([ b(class=pred, '{'),
           var(class=arglist,
               \pred_args([Head], 1)),
           b(class=pred, '}')
         ]).
pred_head(Head) -->                     % Plain terms
    { Head =.. [Functor|Args] },
    html([ b(class=pred, Functor),
           var(class=arglist,
               [ '(', \pred_args(Args, 1), ')' ])
         ]).

%!  is_op_type(+Atom, ?Type)
%
%   True if Atom is an operator of   Type.  Type is one of =prefix=,
%   =infix= or =postfix=.

is_op_type(Functor, Type) :-
    current_op(_Pri, F, Functor),
    op_type(F, Type).

op_type(fx,  prefix).
op_type(fy,  prefix).
op_type(xf,  postfix).
op_type(yf,  postfix).
op_type(xfx, infix).
op_type(xfy, infix).
op_type(yfx, infix).
op_type(yfy, infix).


pred_args([], _) -->
    [].
pred_args([H|T], I) -->
    pred_arg(H, I),
    (   {T==[]}
    ->  []
    ;   html(', '),
        { I2 is I + 1 },
        pred_args(T, I2)
    ).

pred_arg(Var, I) -->
    { var(Var) },
    !,
    html(['Arg', I]).
pred_arg(...(Term), I) -->
    !,
    pred_arg(Term, I),
    html('...').
pred_arg(Term, I) -->
    { Term =.. [Ind,Arg],
      mode_indicator(Ind)
    },
    !,
    html([Ind, \pred_arg(Arg, I)]).
pred_arg(Arg:Type, _) -->
    !,
    html([\argname(Arg), :, \argtype(Type)]).
pred_arg(Arg, _) -->
    argname(Arg).

argname('$VAR'(Name)) -->
    !,
    html(Name).
argname(Name) -->
    !,
    html(Name).

argtype(Term) -->
    { format(string(S), '~W',
             [ Term,
               [ quoted(true),
                 numbervars(true)
               ]
             ]) },
    html(S).

pred_det(unknown) -->
    [].
pred_det(Det) -->
    html([' is ', b(class=det, Det)]).


%!  term(+Text, +Term, +Bindings)// is det.
%
%   Process the \term element as produced by doc_wiki.pl.
%
%   @tbd    Properly merge with pred_head//1

term(_, Atom, []) -->
    { atomic(Atom),
      !,
      format(string(S), '~W', [Atom,[quoted(true)]])
    },
    html(span(class=functor, S)).
term(_, Key:Type, [TypeName=Type]) -->
    { atomic(Key)
    },
    !,
    html([span(class='pl-key', Key), :, span(class('pl-var'), TypeName)]).
term(_, Term, Bindings) -->
    { is_mode(Term is det),         % HACK. Bit too strict?
      bind_vars(Bindings)
    },
    !,
    pred_head(Term).
term(_, Term, Bindings) -->
    term(Term,
         [ variable_names(Bindings),
           quoued(true)
         ]).


                 /*******************************
                 *             PREDREF          *
                 *******************************/

%!  predref(+PI)// is det.
%!  predref(+PI, +Options)// is det.
%
%   Create a reference to a predicate. The reference consists of the
%   relative path to the  file  using   the  predicate  indicator as
%   anchor.
%
%   Current file must  be  available   through  the  global variable
%   =pldoc_file=. If this variable not  set   it  creates  a link to
%   /doc/<file>#anchor.  Such links only work in the online browser.

predref(Term) -->
    { catch(nb_getval(pldoc_options, Options), _, Options = []) },
    predref(Term, Options).

predref(Obj, Options) -->
    { Obj = _:_,
      doc_comment(Obj, File:_Line, _, _),
      (   (   option(files(Map), Options)
          ->  memberchk(file(File,_), Map)
          ;   true
          )
      ->  object_href(Obj, HREF, Options)
      ;   manref(Obj, HREF, Options)
      )
    },
    !,
    html(a(href(HREF), \object_name(Obj, [qualify(true)|Options]))).
predref(M:Term, Options) -->
    !,
    predref(Term, M, Options).
predref(Term, Options) -->
    predref(Term, _, Options).

predref(Name/Arity, _, Options) -->             % Builtin; cannot be overruled
    { prolog:doc_object_summary(Name/Arity, manual, _, _),
      !,
      manref(Name/Arity, HREF, Options)
    },
    html(a([class=builtin, href=HREF], [Name, /, Arity])).
predref(Name/Arity, _, Options) -->             % From packages
    { option(prefer(manual), Options),
      prolog:doc_object_summary(Name/Arity, Category, _, _),
      !,
      manref(Name/Arity, HREF, Options)
    },
    html(a([class=Category, href=HREF], [Name, /, Arity])).
predref(Obj, Module, Options) -->               % Local
    { doc_comment(Module:Obj, File:_Line, _, _),
      (   option(files(Map), Options)
      ->  memberchk(file(File,_), Map)
      ;   true
      )
    },
    !,
    object_ref(Module:Obj, Options).
predref(Name/Arity, Module, Options) -->
    { \+ option(files(_), Options),
      pred_href(Name/Arity, Module, HREF)
    },
    !,
    html(a(href=HREF, [Name, /, Arity])).
predref(Name//Arity, Module, Options) -->
    { \+ option(files(_), Options),
      PredArity is Arity + 2,
      pred_href(Name/PredArity, Module, HREF)
    },
    !,
    html(a(href=HREF, [Name, //, Arity])).
predref(PI, _, Options) -->             % From packages
    { canonical_pi(PI, CPI, HTML),
      (   option(files(_), Options)
      ->  Category = extmanual
      ;   prolog:doc_object_summary(CPI, Category, _, _)
      ),
      manref(CPI, HREF, Options)
    },
    html(a([class=Category, href=HREF], HTML)).
predref(PI, _, _Options) -->
    { canonical_pi(PI, _CPI, HTML)
    },
    !,
    html(span(class=undef, HTML)).
predref(Callable, Module, Options) -->
    { callable(Callable),
      functor(Callable, Name, Arity)
    },
    predref(Name/Arity, Module, Options).

canonical_pi(Name/Arity, Name/Arity, [Name, /, Arity]) :-
    atom(Name), integer(Arity),
    !.
canonical_pi(Name//Arity, Name/Arity2, [Name, //, Arity]) :-
    atom(Name), integer(Arity),
    !,
    Arity2 is Arity+2.


%!  manref(+NameArity, -HREF, +Options) is det.
%
%   Create reference to a manual page.  When generating files, this
%   listens to the option man_server(+Server).

manref(PI, HREF, Options) :-
    predname(PI, PredName),
    (   option(files(_Map), Options)
    ->  option(man_server(Server), Options,
               'http://www.swi-prolog.org/pldoc'),
        uri_components(Server, Comp0),
        uri_data(path, Comp0, Path0),
        directory_file_path(Path0, man, Path),
        uri_data(path, Comp0, Path, Components),
        uri_query_components(Query, [predicate=PredName]),
        uri_data(search, Components, Query),
        uri_components(HREF, Components)
    ;   http_link_to_id(pldoc_man, [predicate=PredName], HREF)
    ).

predname(Name/Arity, PredName) :-
    !,
    format(atom(PredName), '~w/~d', [Name, Arity]).
predname(Module:Name/Arity, PredName) :-
    !,
    format(atom(PredName), '~w:~w/~d', [Module, Name, Arity]).


%!  pred_href(+NameArity, +Module, -HREF) is semidet.
%
%   Create reference.  Prefer:
%
%           1. Local definition
%           2. If from package and documented: package documentation
%           3. From any file
%
%   @bug    Should analyse import list to find where the predicate
%           comes from.

pred_href(Name/Arity, Module, HREF) :-
    format(string(FragmentId), '~w/~d', [Name, Arity]),
    uri_data(fragment, Components, FragmentId),
    functor(Head, Name, Arity),
    (   catch(relative_file(Module:Head, File), _, fail)
    ->  uri_data(path, Components, File),
        uri_components(HREF, Components)
    ;   in_file(Module:Head, File)
    ->  (   current_prolog_flag(home, SWI),
            sub_atom(File, 0, _, _, SWI),
            prolog:doc_object_summary(Name/Arity, packages, _, _)
        ->  http_link_to_id(pldoc_man, [predicate=FragmentId], HREF)
        ;   http_location_by_id(pldoc_doc, DocHandler),
            atom_concat(DocHandler, File, Path),
            uri_data(path, Components, Path),
            uri_components(HREF, Components)
        )
    ).

relative_file(Head, '') :-
    b_getval(pldoc_file, CurrentFile), CurrentFile \== [],
    in_file(Head, CurrentFile),
    !.
relative_file(Head, RelFile) :-
    b_getval(pldoc_file, CurrentFile), CurrentFile \== [],
    in_file(Head, DefFile),
    relative_file_name(DefFile, CurrentFile, RelFile).

%!  pred_source_href(+Pred:predicate_indicator, +Module, -HREF) is semidet.
%
%   HREF is a URL to show the predicate source in its file.

pred_source_href(Name/Arity, Module, HREF) :-
    format(string(FragmentId), '~w/~d', [Name, Arity]),
    uri_data(fragment, Components, FragmentId),
    uri_query_components(Query, [show=src]),
    uri_data(search, Components, Query),
    functor(Head, Name, Arity),
    (   catch(relative_file(Module:Head, File), _, fail)
    ->  uri_data(path, Components, File),
        uri_components(HREF, Components)
    ;   in_file(Module:Head, File0)
    ->  insert_alias(File0, File),
        http_location_by_id(pldoc_doc, DocHandler),
        atom_concat(DocHandler, File, Path),
        uri_data(path, Components, Path),
        uri_components(HREF, Components)
    ).


%!  object_ref(+Object, +Options)// is det.
%
%   Create a hyperlink to Object. Points to the /doc_for URL. Object
%   is as the first argument of doc_comment/4.   Note  this can be a
%   list of objects.

object_ref([], _) -->
    !,
    [].
object_ref([H|T], Options) -->
    !,
    object_ref(H, Options),
    (   {T == []}
    ->  html(', '),
        object_ref(T, Options)
    ;   []
    ).
object_ref(Obj, Options) -->
    { object_href(Obj, HREF, Options)
    },
    html(a(href(HREF), \object_name(Obj, Options))).

%!  object_href(+Object, -HREF) is det.
%!  object_href(+Object, -HREF, +Options) is det.
%
%   HREF is the URL to access Object.

object_href(Obj, HREF) :-
    object_href(Obj, HREF, []).

object_href(M:PI0, HREF, Options) :-
    option(files(Map), Options),
    (   module_property(M, file(File))
    ->  true
    ;   xref_module(File, M)
    ),
    memberchk(file(File, DocFile), Map),
    !,
    file_base_name(DocFile, LocalFile),     % TBD: proper directory index
    expand_pi(PI0, PI),
    term_to_string(PI, PIS),
    uri_data(path, Components, LocalFile),
    uri_data(fragment, Components, PIS),
    uri_components(HREF, Components).
object_href(file(File), HREF, _Options) :-
    doc_file_href(File, HREF),
    !.
object_href(directory(Dir), HREF, _Options) :-
    directory_file_path(Dir, 'index.html', Index),
    doc_file_href(Index, HREF),
    !.
object_href(Obj, HREF, _Options) :-
    prolog:doc_object_href(Obj, HREF),
    !.
object_href(Obj0, HREF, _Options) :-
    localise_object(Obj0, Obj),
    term_to_string(Obj, String),
    http_link_to_id(pldoc_object, [object=String], HREF).

expand_pi(Name//Arity0, Name/Arity) :-
    !,
    Arity is Arity0+2.
expand_pi(PI, PI).


%!  localise_object(+ObjIn, -ObjOut) is det.
%
%   Abstract  path-details  to  make  references  more  stable  over
%   versions.

localise_object(Obj0, Obj) :-
    prolog:doc_canonical_object(Obj0, Obj),
    !.
localise_object(Obj, Obj).


%!  term_to_string(+Term, -String) is det.
%
%   Convert Term, possibly  holding  variables,   into  a  canonical
%   string using A, B, ... for variables and _ for singletons.

term_to_string(Term, String) :-
    State = state(-),
    (   numbervars(Term, 0, _, [singletons(true)]),
        with_output_to(string(String),
                       write_term(Term,
                                  [ numbervars(true),
                                    quoted(true)
                                  ])),
        nb_setarg(1, State, String),
        fail
    ;   arg(1, State, String)
    ).

%!  object_name(+Obj, +Options)// is det.
%
%   HTML description of documented Obj. Obj is as the first argument
%   of doc_comment/4.  Options:
%
%     - style(+Style)
%     One of =inline= or =title=
%     - qualify(+Boolean)
%     Qualify predicates by their module
%     - secref_style(Style)
%     One of =number=, =title= or =number_title=

object_name(Obj, Options) -->
    { option(style(Style), Options, inline)
    },
    object_name(Style, Obj, Options).

object_name(title, Obj, Options) -->
    { merge_options(Options, [secref_style(title)], Options1) },
    prolog:doc_object_link(Obj, Options1),
    !.
object_name(inline, Obj, Options) -->
    prolog:doc_object_link(Obj, Options),
    !.
object_name(title, f(Name/Arity), _Options) -->
    !,
    html(['Function ', Name, /, Arity]).
object_name(inline, f(Name/Arity), _Options) -->
    !,
    html([Name, /, Arity]).
object_name(Style, PI, Options) -->
    { is_pi(PI) },
    !,
    pi(Style, PI, Options).
object_name(inline, Module:module(_Title), _) -->
    !,
    { module_property(Module, file(File)),
      file_base_name(File, Base)
    },
    !,
    html(Base).
object_name(title, Module:module(Title), _) -->
    { module_property(Module, file(File)),
      file_base_name(File, Base)
    },
    !,
    html([Base, ' -- ', Title]).
object_name(title, file(File), _) -->
    { module_property(Module, file(File)),
      doc_comment(Module:module(Title), _, _, _),
      !,
      file_base_name(File, Base)
    },
    html([Base, ' -- ', Title]).
object_name(_, file(File), _) -->
    { file_base_name(File, Base) },
    html(Base).
object_name(_, directory(Dir), _) -->
    { file_base_name(Dir, Base) },
    html(Base).
object_name(_, module(Title), _Options) -->
    { print_message(warning,
                    pldoc(module_comment_outside_module(Title)))
    }.

pi(title, PI, Options) -->
    pi_type(PI),
    pi(PI, Options).
pi(inline, PI, Options) -->
    pi(PI, Options).

pi(M:PI, Options) -->
    !,
    (   { option(qualify(true), Options) }
    ->  html([span(class(module), M), :])
    ;   []
    ),
    pi(PI, Options).
pi(Name/Arity, _) -->
    !,
    html([Name, /, Arity]).
pi(Name//Arity, _) -->
    html([Name, //, Arity]).

pi_type(_:PI) -->
    !,
    pi_type(PI).
pi_type(_/_) -->
    html(['Predicate ']).
pi_type(_//_) -->
    html(['Grammar rule ']).



%!  in_file(+Head, ?File) is nondet.
%
%   File is the name of a file containing the Predicate Head.
%   Head may be qualified with a module.
%
%   @tbd Prefer local, then imported, then `just anywhere'
%   @tbd Look for documented and/or public predicates.

in_file(Module:Head, File) :-
    !,
    distinct(File, in_file(Module, Head, File)).
in_file(Head, File) :-
    distinct(File, in_file(_, Head, File)).

in_file(Module, Head, File) :-
    var(Module),
    (   predicate_property(system:Head, foreign)
    ->  !,
        fail
    ;   predicate_property(system:Head, file(File)),
        \+ system_arithmetic_function(Head)
    ->  !
    ;   predicate_property(Head, autoload(File0))
    ->  !,
        file_name_extension(File0, pl, File)
    ;   exported_from(Module, Head, File),
        module_property(Module, class(library))
    ).
in_file(Module, Head, File) :-
    xref_defined(File, Head, How),
    xref_current_source(File),
    atom(File),                     % only plain files
    xref_module(File, Module),
    How \= imported(_From).
in_file(Module, Head, File) :-
    exported_from(Module, Head, File).
in_file(Module, Head, File) :-
    predicate_property(Module:Head, file(File)),
    \+ predicate_property(Module:Head, imported_from(_)).
in_file(Module, Head, File) :-
    current_module(Module),
    source_file(Module:Head, File).

exported_from(Module, Head, File) :-
    distinct(Primary,
             (   predicate_property(Module:Head, exported),
                 (   predicate_property(Module:Head, imported_from(Primary))
                 ->  true
                 ;   Primary = Module
                 ))),
    module_property(Primary, file(File)).

:- multifile
    arithmetic:evaluable/2.

system_arithmetic_function(Head) :-
    functor(Head, Name, Arity),
    FArith is Arity-1,
    FArith >= 0,
    functor(FHead, Name, FArith),
    arithmetic:evaluable(FHead, system).

%%     file(+FileName)// is det.
%%     file(+FileName, +Options)// is det.
%
%      Create a link to another filename if   the file exists. Called by
%      \file(File) terms in the DOM term generated by wiki.pl. Supported
%      options are:
%
%          * label(+Label)
%          Label to use for the link to the file.
%
%          * absolute_path(+Path)
%          Absolute location of the referenced file.
%
%          * href(+HREF)
%          Explicitely provided link; overrule link computation.
%
%          * map_extension(+Pairs)
%          Map the final extension if OldExt-NewExt is in Pairs.
%
%          * files(+Map)
%          List of file(Name, Link) that specifies that we must
%          user Link for the given physical file Name.
%
%          * edit_handler(+Id)
%          HTTP handler Id to call if the user clicks the edit button.
%
%       @tbd    Translation of files to HREFS is a mess.  How to relate
%               these elegantly?

file(File) -->
    file(File, []).

file(File, Options) -->
    { catch(nb_getval(pldoc_options, GenOptions), _, GenOptions = []),
      merge_options(Options, GenOptions, FinalOptions)
    },
    link_file(File, FinalOptions),
    !.
file(File, Options) -->
    { option(edit_handler(Handler), Options),
      http_current_request(Request),
      memberchk(path(Path), Request),
      absolute_file_name(File, Location,
                         [ relative_to(Path)
                         ]),
      http_link_to_id(Handler, [location(Location)], HREF),
      format(atom(Title), 'Click to create ~w', [File])
    },
    html(a([href(HREF), class(nofile), title(Title)], File)).
file(File, _) -->
    html(code(class(nofile), File)).

link_file(File, Options) -->
    { file_href(File, HREF, Options),
      option(label(Label), Options, File),
      option(class(Class), Options, file)
    },
    html(a([class(Class), href(HREF)], Label)).

%!  file_href(+FilePath, -HREF, +Options) is det.
%
%   Find URL for refering to FilePath based on Options.

file_href(_, HREF, Options) :-
    option(href(HREF), Options),
    !.
file_href(File, HREF, Options) :-
    file_href_real(File, HREF0, Options),
    map_extension(HREF0, HREF, Options).

%!  map_extension(+HREFIn, -HREFOut, Options) is det.
%
%   Replace extension using the option
%
%       * map_extension(+Pairs)

map_extension(HREF0, HREF, Options) :-
    option(map_extension(Map), Options),
    file_name_extension(Base, Old, HREF0),
    memberchk(Old-New, Map),
    !,
    file_name_extension(Base, New, HREF).
map_extension(HREF, HREF, _).


file_href_real(File, HREF, Options) :-
    (   option(absolute_path(Path), Options)
    ;   existing_linked_file(File, Path)
    ),
    !,
    (   option(files(Map), Options),
        memberchk(file(Path, LinkFile), Map)
    ->  true
    ;   LinkFile = Path
    ),
    file_href(LinkFile, HREF).
file_href_real(File, HREF, _) :-
    directory_alias(Alias),
    Term =.. [Alias,File],
    absolute_file_name(Term, _,
                       [ access(read),
                         file_errors(fail)
                       ]),
    !,
    http_absolute_location(Term, HREF, []).

directory_alias(icons).
directory_alias(css).


%!  file_href(+FilePath, -HREF) is det.
%
%   Create a relative URL from  the   current  location to the given
%   absolute file name. It resolves  the   filename  relative to the
%   file being processed  that  is   available  through  the  global
%   variable =pldoc_file=.

file_href(Path, HREF) :-                % a loaded Prolog file
    source_file(Path),
    !,
    doc_file_href(Path, HREF).
file_href(Path, HREF) :-
    (   nb_current(pldoc_output, CFile)
    ;   nb_current(pldoc_file, CFile)
    ),
    CFile \== [],
    !,
    relative_file_name(Path, CFile, HREF).
file_href(Path, Path).


%!  existing_linked_file(+File, -Path) is semidet.
%
%   True if File is a path to an existing file relative to the
%   current file.  Path is the absolute location of File.

existing_linked_file(File, Path) :-
    catch(b_getval(pldoc_file, CurrentFile), _, fail),
    CurrentFile \== [],
    absolute_file_name(File, Path,
                       [ relative_to(CurrentFile),
                         access(read),
                         file_errors(fail)
                       ]).


%!  include(+FileName, +Type, +Options)// is det.
%
%   Inline FileName. If this is an image file, show an inline image.
%   Else we create a link  like   file//1.  Called by \include(File,
%   Type)  terms  in  the  DOM  term  generated  by  wiki.pl  if  it
%   encounters [[file.ext]].

include(PI, predicate, _) -->
    !,
    (   html_tokens_for_predicates(PI, [])
    ->  []
    ;   html(['[[', \predref(PI), ']]'])
    ).
include(File, image, Options) -->
    { file_name_extension(_, svg, File),
      file_href(File, HREF, Options),
      !,
      include(image_attribute, Options, Attrs0),
      merge_options(Attrs0,
                    [ alt(File),
                      data(HREF),
                      type('image/svg+xml')
                    ], Attrs)
    },
    (   { option(caption(Caption), Options) }
    ->  html(div(class(figure),
                 [ div(class(image), object(Attrs, [])),
                   div(class(caption), Caption)
                 ]))
    ;   html(object(Attrs, []))
    ).
include(File, image, Options) -->
    { file_href(File, HREF, Options),
      !,
      include(image_attribute, Options, Attrs0),
      merge_options(Attrs0,
                    [ alt(File),
                      border(0),
                      src(HREF)
                    ], Attrs)
    },
    (   { option(caption(Caption), Options) }
    ->  html(div(class(figure),
                 [ div(class(image), img(Attrs)),
                   div(class(caption), Caption)
                 ]))
    ;   html(img(Attrs))
    ).
include(File, wiki, _Options) -->       % [[file.txt]] is included
    { access_file(File, read),
      !,
      read_file_to_codes(File, String, []),
      wiki_codes_to_dom(String, [], DOM)
    },
    html(DOM).
include(File, _Type, Options) -->
    link_file(File, Options),
    !.
include(File, _, _) -->
    html(code(class(nofile), ['[[',File,']]'])).

image_attribute(src(_)).
image_attribute(alt(_)).
image_attribute(title(_)).
image_attribute(align(_)).
image_attribute(width(_)).
image_attribute(height(_)).
image_attribute(border(_)).
image_attribute(class(_)).
image_attribute(style(_)).


%!  html_tokens_for_predicates(+PI, +Options)// is semidet.
%
%   Inline description for a predicate as produced by the text below
%   from wiki processing.
%
%   ==
%           * [[member/2]]
%           * [[append/3]]
%   ==

html_tokens_for_predicates([], _Options) -->
    [].
html_tokens_for_predicates([H|T], Options) -->
    !,
    html_tokens_for_predicates(H, Options),
    html_tokens_for_predicates(T, Options).
html_tokens_for_predicates(PI, Options) -->
    { PI = _:_/_,
      !,
      (   doc_comment(PI, Pos, _Summary, Comment)
      ->  true
      ;   Comment = ''
      )
    },
    object(PI, [Pos-Comment], [dl], _, Options).
html_tokens_for_predicates(Spec, Options) -->
    { findall(PI, documented_pi(Spec, PI), List),
      List \== [], !
    },
    html_tokens_for_predicates(List, Options).
html_tokens_for_predicates(Spec, Options) -->
    man_page(Spec,
             [ links(false),                % no header
               navtree(false),              % no navigation tree
               footer(false),               % no footer
               synopsis(false)              % no synopsis
             | Options
             ]).


documented_pi(Spec, PI) :-
    generalise_spec(Spec, PI),
    doc_comment(PI, _Pos, _Summary, _Comment).

generalise_spec(Name/Arity, _M:Name/Arity).
generalise_spec(Name//Arity, _M:Name//Arity).


                 /*******************************
                 *           WIKI FILES         *
                 *******************************/


%!  doc_for_wiki_file(+File, +Options) is det.
%
%   Write HTML for the File containing wiki data.

doc_for_wiki_file(FileSpec, Options) :-
    absolute_file_name(FileSpec, File,
                       [ access(read)
                       ]),
    read_file_to_codes(File, String, []),
    b_setval(pldoc_file, File),
    call_cleanup(reply_wiki_page(File, String, Options),
                 nb_delete(pldoc_file)).

reply_wiki_page(File, String, Options) :-
    wiki_codes_to_dom(String, [], DOM0),
    title(DOM0, File, Title),
    insert_edit_button(DOM0, File, DOM, Options),
    reply_html_page(pldoc(wiki),
                    title(Title),
                    [ \html_requires(pldoc)
                    | DOM
                    ]).

title(DOM, _, Title) :-
    sub_term(h1(_,Title), DOM),
    !.
title(_, File, Title) :-
    file_base_name(File, Title).

insert_edit_button(DOM, _, DOM, Options) :-
    option(edit(false), Options, false),
    !.
insert_edit_button([h1(Attrs,Title)|DOM], File,
                   [h1(Attrs,[ span(style('float:right'),
                                   \edit_button(File, [edit(true)]))
                             | Title
                             ])|DOM], _) :- !.
insert_edit_button(DOM, File,
                   [ h1(class(wiki),
                        [ span(style('float:right'),
                               \edit_button(File, [edit(true)]))
                        ])
                   | DOM
                   ], _).


                 /*******************************
                 *            ANCHORS           *
                 *******************************/

%!  mode_anchor_name(+Mode, -Anchor:atom) is det.
%
%   Get the anchor name for a mode.

mode_anchor_name(Var, _) :-
    var(Var),
    !,
    instantiation_error(Var).
mode_anchor_name(mode(Head, _), Anchor) :-
    !,
    mode_anchor_name(Head, Anchor).
mode_anchor_name(Head is _Det, Anchor) :-
    !,
    mode_anchor_name(Head, Anchor).
mode_anchor_name(Head, Anchor) :-
    pred_anchor_name(Head, _, Anchor).


%!  pred_anchor_name(+Head, -PI:atom/integer, -Anchor:atom) is det.
%
%   Create an HTML anchor name from Head.

pred_anchor_name(//(Head), Name/Arity, Anchor) :-
    !,
    functor(Head, Name, DCGArity),
    Arity is DCGArity+2,
    format(atom(Anchor), '~w/~d', [Name, Arity]).
pred_anchor_name(Head, Name/Arity, Anchor) :-
    functor(Head, Name, Arity),
    format(atom(Anchor), '~w/~d', [Name, Arity]).

:- multifile prolog:message//1.

prolog:message(pldoc(module_comment_outside_module(Title))) -->
    [ 'PlDoc comment <module> ~w does not appear in a module'-[Title] ].
