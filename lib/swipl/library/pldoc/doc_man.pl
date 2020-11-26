/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2006-2018, University of Amsterdam
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

:- module(pldoc_man,
          [ man_page//2,                % +Obj, +Options
            man_overview//1,            % +Options

            man_content_tree/2,         % +Dir, -Tree
            man_packages_tree/1         % -Tree
          ]).
:- use_module(library(xpath),[xpath/3, op(_,_,_)]).
:- use_module(library(http/html_write)).

:- autoload(doc_html,
	    [ object_tree/5, private/2, object_page_header/4, objects/4,
	      object_href/2, object_synopsis/4, object_page_footer/4,
	      object_ref/4, object_page/4,
              object_source_button//2
	    ]).
:- autoload(doc_process,[doc_comment/4]).
:- autoload(doc_search,[search_form/3]).
:- autoload(doc_util,[atom_to_object/2,atom_pi/2]).
:- autoload(man_index,[manual_object/5]).
:- autoload(library(apply),[maplist/2,maplist/3]).
:- autoload(library(debug),[assertion/1,debug/3]).
:- autoload(library(error),[permission_error/3,existence_error/2]).
:- autoload(library(filesex),
	    [directory_file_path/3,relative_file_name/3]).
:- autoload(library(lists),
	    [select/4,append/3,member/2,last/2,selectchk/3]).
:- autoload(library(option),[merge_options/3,option/2,option/3]).
:- autoload(library(pairs),[pairs_values/2,pairs_keys/2]).
:- autoload(library(prolog_xref),[xref_public_list/3]).
:- autoload(library(sgml),
	    [ load_html/3, dtd/2, new_sgml_parser/2, set_sgml_parser/2,
	      sgml_parse/2, free_sgml_parser/1
	    ]).
:- autoload(library(uri),[uri_encoded/3]).
:- autoload(library(www_browser),[expand_url_path/2]).
:- autoload(library(http/html_head),[html_requires/3]).
:- autoload(library(http/http_dispatch),
	    [ http_link_to_id/3, http_location_by_id/2,
	      http_handler/3, http_reply_file/3, http_redirect/3
	    ]).
:- autoload(library(http/http_path),[http_absolute_location/3]).
:- autoload(library(http/mimetype),[file_mime_type/2]).

:- include(hooks).

/** <module> Process SWI-Prolog HTML manuals

*/

:- predicate_options(man_page//2, 2,
                     [ for(atom),
                       links(boolean),
                       navtree(boolean),
                       synopsis(boolean),
                       footer(boolean),
                       link_source(boolean),
                       no_manual(oneof([fail,error])),
                       search_in(oneof([all, app, man])),
                       search_match(oneof([name, summary])),
                       search_options(boolean)
                     ]).


                 /*******************************
                 *           HIERARCHY          *
                 *******************************/

%!  man_nav_tree(+Obj, +Options) is semidet.
%
%   Create a navigation tree consisting of   a nested =ul= list that
%   reflects the location of Obj in the manual.

man_nav_tree(Obj, Options) -->
    { ensure_man_tree,
      man_nav_tree(Obj, Tree, Options),
      TreeOptions = [ secref_style(title)
                    | Options
                    ]
    },
    html(ul(class(nav),
            \object_tree(Tree, [Obj], TreeOptions))).


%!  man_nav_tree(+Obj, -Tree, +Options) is semidet.
%
%   True when Tree is the navigation tree  for Obj. By default, this
%   is the tree going from  the  leaf   to  the  root, unfolding the
%   neighbors of Obj.

man_nav_tree(Obj, Tree, _Options) :-
    man_child_of(Obj, Parent),
    !,
    findall(Neighbour, man_child_of(Neighbour, Parent), Neighbours0),
    (   findall(Child, man_child_of(Child, Obj), Children),
        Children \== []
    ->  select(Obj, Neighbours0, node(Obj, Children), Neighbours)
    ;   Neighbours = Neighbours0
    ),
    path_up(node(Parent, Neighbours), Tree).
man_nav_tree(Obj, node(Obj, Children), _Options) :-
    findall(Child, man_child_of(Child, Obj), Children).


path_up(Node, Tree) :-
    node_id(Node, Id),
    man_child_of(Id, Parent),
    !,
    (   Parent == root
    ->  findall(Neighbour, man_child_of(Neighbour, Parent), Neighbours0),
        select(Id, Neighbours0, Node, Neighbours),
        Tree = node(root, Neighbours)
    ;   path_up(node(Parent, [Node]), Tree)
    ).
path_up(Tree, Tree).


%!  man_child_of(?Child, ?Parent) is nondet.
%
%   Query the manual hierarchy.

man_child_of(Child, Parent) :-
    term_hash(Child, ChildHash),
    term_hash(Parent, ParentHash),
    man_child_of(ChildHash, Child, ParentHash, Parent).

:- dynamic
    man_child_of/4,
    man_tree_done/0.

%!  ensure_man_tree
%
%   Materialize the manual tree as a binary relation.

ensure_man_tree :-
    man_tree_done,
    !.
ensure_man_tree :-
    with_mutex(man_tree,
               make_man_tree).

make_man_tree :-
    man_tree_done,
    !.
make_man_tree :-
    man_content_tree(swi_man_manual('.'), ManTree),
    man_packages_tree(PkgTree),
    assert_tree(node(root, [ManTree, PkgTree])),
    assertz(man_tree_done).

assert_tree(node(Id, Children)) :-
    !,
    maplist(assert_parent(Id), Children),
    maplist(assert_tree, Children).
assert_tree(_).

assert_parent(Id, Child) :-
    node_id(Child, ChildId),
    term_hash(Id, ParentHash),
    term_hash(ChildId, ChildHash),
    assertz(man_child_of(ChildHash, ChildId, ParentHash, Id)).

node_id(node(Id, _), Id) :- !.
node_id(Id, Id).


%!  man_content_tree(+Dir, -Tree) is det.
%
%   Compute the content tree for a   multi-file HTML document. We do
%   this by processing =Contents.html= for  making the toplevel tree
%   that   links   to   the   individual    files.   Then   we   use
%   html_content_tree/2 to materialize the trees for the files.

man_content_tree(Spec, node(manual, Chapters)) :-
    absolute_file_name(Spec, Dir,
                       [ file_type(directory),
                         access(read)
                       ]),
    directory_file_path(Dir, 'Contents.html', ContentsFile),
    load_html(ContentsFile, DOM, [cdata(string)]),
    findall(Level-Path,
            ( xpath(DOM, //div(@class=Class), DIV),
              class_level(Class, Level),
              xpath(DIV, a(@class=sec,@href=File), _),
              \+ sub_atom(File, _, _, _, #),
              directory_file_path(Dir, File, Path)
            ),
            Pairs),
    index_chapters(Pairs, Chapters).

class_level('toc-h1', 1).
class_level('toc-h2', 2).
class_level('toc-h3', 3).
class_level('toc-h4', 4).

index_chapters([], []).
index_chapters([Level-File|T0], [node(Chapter, Children)|T]) :-
    html_content_tree(File, Node),
    Node = node(Chapter, Children0),
    append(Children0, Sections, Children),
    index_sections(T0, Level, Sections, T1),
    index_chapters(T1, T).

index_sections([], _, [], []) :- !.
index_sections([SLevel-File|T0], Level, [Node|T], Rest) :-
    SLevel > Level,
    !,
    html_content_tree(File, Node),
    index_sections(T0, Level, T, Rest).
index_sections(Rest, _, [], Rest).


%!  man_packages_tree(-Tree) is det.
%
%   Tree is the content tree of all packages

man_packages_tree(node(packages, Packages)) :-
    Section = section(0, _, _, _),
    findall(File,
            manual_object(Section, _Title, File, packages, _),
            Files),
    maplist(package_node, Files, Packages).

package_node(File, Tree) :-
    html_content_tree(File, Tree).

%!  html_content_tree(+ManualFile, -Tree) is det.
%
%   True when Tree represents the  hierarchical structure of objects
%   documented in the HTML file ManualFile. Tree  is a term where of
%   the form below. Object is a   documentation  object (typically a
%   section  or  predicate  indicator)  that    may   be  handed  to
%   object_link//1  and  similar  predicates  to  make  a  table  of
%   contents.
%
%       node(Object, ListOfTree).

html_content_tree(FileIn, Tree) :-
    absolute_file_name(FileIn, File),
    findall(Offset-Obj,
            manual_object(Obj, _Summary, File, _Class, Offset),
            Pairs),
    keysort(Pairs, Sorted),
    pairs_values(Sorted, Objects),
    make_tree(Objects, Trees),
    assertion(Trees = [_]),
    Trees = [Tree].

make_tree([], []).
make_tree([Obj|T0], [node(Obj, Children)|T]) :-
    children(T0, Obj, Children, T1),
    make_tree(T1, T).

children([], _, [], []) :- !.
children([Obj|T0], Root, [Node|T], Rest) :-
    section_level(Obj, ObjLevel),
    section_level(Root, Level),
    ObjLevel > Level,
    !,
    Node = node(Obj, Children),
    children(T0, Obj, Children, T1),
    children(T1, Root, T, Rest).
children([Obj|T0], Root, [Obj|T], Rest) :-
    \+ section_level(Obj, _),
    !,
    children(T0, Root, T, Rest).
children(Rest, _, [], Rest).

section_level(section(Level, _Nr, _Id, _File), Level).


                 /*******************************
                 *            RETRIEVE          *
                 *******************************/

%!  load_man_object(+Obj, -Parent, -Path, -DOM) is nondet.
%
%   load the desription of the  object   matching  Obj from the HTML
%   sources and return the DT/DD pair in DOM.
%
%   @tbd    Nondet?

load_man_object(Obj, ParentSection, Path, DOM) :-
    resolve_section(Obj, For),
    For = section(_,SN,_ID,Path),
    parent_section(For, ParentSection),
    findall(Nr-Pos, section_start(Path, Nr, Pos), Pairs),
    (   (   Pairs = [SN-_|_]
        ;   Pairs == []
        )
    ->  !,
        load_html(Path, DOM, [cdata(string)])           % Load whole file
    ;   append(_, [SN-Start|Rest], Pairs)
    ->  !,
        (   member(N-End, Rest),
            \+ sub_atom(N, 0, _, _, SN),
            Len is End - Start,
            Options = [content_length(Len)]
        ->  true
        ;   Options = []
        ),
        open(Path, read, In, [type(binary)]),
        seek(In, Start, bof, _),
        dtd(html, DTD),
        new_sgml_parser(Parser,
                        [ dtd(DTD)
                        ]),
        set_sgml_parser(Parser, file(Path)),
        set_sgml_parser(Parser, dialect(sgml)),
        set_sgml_parser(Parser, shorttag(false)),
        set_sgml_parser(Parser, defaults(false)),
        call_cleanup(sgml_parse(Parser,
                                [ document(DOM),
                                  source(In),
                                  syntax_errors(quiet),
                                  cdata(string)
                                | Options
                                ]),
                     ( free_sgml_parser(Parser),
                       close(In)
                     ))
    ).
load_man_object(For, Parent, Path, DOM) :-
    object_spec(For, Obj),
    manual_object(Obj, _, Path, _, Position),
    (   object_section(Path, Position, Parent)
    ->  true
    ;   Parent = Path
    ),
    open(Path, read, In, [type(binary)]),
    seek(In, Position, bof, _),
    dtd(html, DTD),
    new_sgml_parser(Parser,
                    [ dtd(DTD)
                    ]),
    set_sgml_parser(Parser, file(Path)),
    set_sgml_parser(Parser, dialect(sgml)),
    set_sgml_parser(Parser, shorttag(false)),
    set_sgml_parser(Parser, defaults(false)),
    call_cleanup(parse_dts_upto_dd(Parser, In, DOM),
                 ( free_sgml_parser(Parser),
                   close(In)
                 )).

parse_dts_upto_dd(Parser, In, Description) :-
    sgml_parse(Parser,
               [ document(DOM0),
                 cdata(string),
                 source(In),
                 parse(element),
                 syntax_errors(quiet)
               ]),
    (   DOM0 = [Element],
        Element = element(dt, _, _)
    ->  Description = [Element|More],
        parse_dts_upto_dd(Parser, In, More)
    ;   Description = DOM0
    ).

section_start(Path, Nr, Pos) :-
    manual_object(section(_,Nr,_,_), _, Path, _, Pos).

%!  resolve_section(+SecIn, -SecOut) is det.
%
%   Resolve symbolic path reference and fill   in  level and section
%   number if this information is missing.   The latter allows us to
%   refer to files of the manual.

resolve_section(section(Level, No, Spec), Section) :-
    !,
    resolve_section(section(Level, No, _, Spec), Section).
resolve_section(section(Level, No, ID, Path),
                section(Level, No, ID, Path)) :-
    nonvar(ID),
    manual_object(section(Level,No,ID,Path), _, _, _, _),
    !.
resolve_section(section(Level, No, ID, Spec),
                section(Level, No, ID, Path)) :-
    ground(Spec),
    absolute_file_name(Spec, Path,
                       [ access(read)
                       ]),
    (   manual_object(section(Level, No, ID, Path), _, _, _, _)
    ->  true
    ;   path_allowed(Path)
    ->  true
    ;   permission_error(read, manual_file, Spec)
    ).


path_allowed(Path) :-                   % allow all files from swi/doc
    absolute_file_name(swi(doc), Parent,
                       [ access(read),
                         file_type(directory)
                       ]),
    sub_atom(Path, 0, _, _, Parent).


%!  parent_section(+Section, -Parent) is det.
%
%   Parent is the parent-section  of   Section.  First  computes the
%   section number and than finds the   required  number in the same
%   file or same directory. If this doesn't exist, get the file as a
%   whole.

parent_section(section(Level, Nr, _ID, File), Parent) :-
    integer(Level),
    Parent = section(PL, PNr, _PID, _PFile),
    PL is Level - 1,
    findall(B, sub_atom(Nr, B, _, _, '.'), BL),
    last(BL, Before),
    sub_atom(Nr, 0, Before, _, PNr),
    (   manual_object(Parent, _, File, _, _)
    ->  true
    ;   manual_object(Parent, _, ParentFile, _, _),
        same_dir(File, ParentFile)
    ->  true
    ;   manual_object(Parent, _, _, _, _)
    ),
    !.
parent_section(section(Level, _, _, File), Parent) :-
    Parent = section(ParentLevel, _, _, File),
    manual_object(Parent, _, _, _, _),
    ParentLevel < Level,
    !.
parent_section(section(_, _, _, File), File).


%!  object_section(+Path, +Position, -Section) is semidet.
%
%   Section is the section in which object appears.  This is the
%   last section object before position.

object_section(Path, Pos, Section) :-
    Section = section(_,_,_,_),
    findall(Section,
           (manual_object(Section, _, Path, _, SecPos), SecPos =< Pos),
            List),
    last(List, Section).

same_dir(File1, File2) :-
    file_directory_name(File1, Dir),
    file_directory_name(File2, Dir).

%!  object_spec(+Atom, -SpecTerm)
%
%   Tranform the Name/Arity, etc strings as   received from the HTTP
%   into a term.  Must return unique results.

object_spec(Spec, Spec) :-
    compound(Spec),
    !.
object_spec(Atom, Spec) :-
    catch(atom_to_term(Atom, Spec, _), _, fail),
    !,
    Atom \== Spec.
object_spec(Atom, PI) :-
    atom_to_object(Atom, PI).


                 /*******************************
                 *            EMIT              *
                 *******************************/

%!  man_page(+Obj, +Options)// is semidet.
%
%   Produce a Prolog manual page for  Obj.   The  page consists of a
%   link to the section-file and  a   search  field, followed by the
%   predicate description.  Obj is one of:
%
%       * Name/Arity
%       Predicate indicator: display documentation of the predicate
%
%       * f(Name/Arity)
%       display documentation of an arithmetic function
%
%       * c(Function)
%       display documentation of a C API function
%
%       * section(Level, Number, Id, File)
%       Display a section of the manual
%
%       * sec(DocFile#Id)
%       Display a section of the manual (from short form)
%
%   Options:
%
%           * no_manual(Action)
%           If Action = =fail=, fail instead of displaying a
%           not-found message.
%
%           * synopsis(Bool)
%           If `false`, omit the synopsis line
%
%           * links(Bool)
%           If =true= (default), include links to the parent object;
%           if =false=, just emit the manual material.
%
%           * navtree(Bool)
%           If `true` (default), display the navigation tree, otherwise
%           suppress it.

man_page(Obj, Options) -->
    { ground(Obj),
      special_node(Obj)
    },
    !,
    html_requires(pldoc),
    man_links([], Options),
    man_matches([Obj], Obj, Options).
man_page(Obj0, Options) -->                     % Manual stuff
    { full_page(Obj0, Obj),
      findall((Parent+Path)-(Obj+DOM),
              load_man_object(Obj, Parent, Path, DOM),
              Matches),
      Matches = [_|_],
      !,
      pairs_keys(Matches, ParentPaths),
      Matches = [Parent+Path-_|_]
    },
    html_requires(pldoc),
    man_links(ParentPaths, Options),
    man_matches(Matches, Obj, Options).
man_page(Obj, Options) -->                      % PlDoc predicates, etc.
    { full_object(Obj, Full),
      findall(Full-File,
              ( doc_comment(Full, File:_, _, _),
                \+ private(Full, Options)
              ),
              Pairs),
      Pairs \== [],
      pairs_keys(Pairs, Objs)
    },
    !,
    html_requires(pldoc),
    (   { Pairs = [_-File] }
    ->  object_page_header(File, Options)
    ;   object_page_header(-, Options)
    ),
    { merge_options(Options,
                    [ synopsis(true),
                      navtree(true)
                    ], Options2)
    },
    objects(Objs, Options2).
man_page(Obj, Options) -->                      % failure
    { \+ option(no_manual(fail), Options)
    },
    html_requires(pldoc),
    man_links([], Options),
    html(p(class(noman),
           [ 'Sorry, No manual entry for ',
             b('~w'-[Obj])
           ])).

%special_node(manual).          % redirected to the Introduction section
special_node(root).
special_node(packages).

full_page(Obj, _) :-
    var(Obj), !, fail.
full_page(Obj, Obj) :-
    Obj = section(_,_,_,_),
    !.
full_page(section(ID), section(_,_,ID,_)) :- !.
full_page(manual, section(_,_,'sec:intro',_)) :- !.
full_page(Obj0, Obj) :-
    ground(Obj0),
    alt_obj(Obj0, Obj),
    manual_object(Obj, _, _, _, _),
    !.
full_page(Obj, Obj) :-
    ground(Obj).

alt_obj(Obj, Obj).
alt_obj(Name/Arity, Name//DCGArity) :-
    integer(Arity),
    Arity >= 2,
    DCGArity is Arity - 2.
alt_obj(Name//DCGArity, Name/Arity) :-
    integer(DCGArity),
    Arity is DCGArity + 2.

%!  full_object(+Object, -Full) is semidet.
%
%   Translate to canonical PlDoc object

full_object(Object, M:Obj) :-
    qualify(Object, M:Obj0),
    alt_obj(Obj0, Obj),
    doc_comment(M:Obj, _, _, _),
    !.

qualify(M:O, M:O).
qualify(O, _:O).

%!  man_qualified_object(+Object, +Parent,
%!                       -LibraryOpt, -QObject, -Section) is semidet.
%
%   Get a qualified predicate description from  Text that appears in
%   the section Parent.
%
%   The tricky part is that there   are cases where multiple modules
%   export the same predicate. We must find   from  the title of the
%   manual section which library is documented.

man_qualified_object(Text, Parent, LibOpt, Object, Section) :-
    atom(Text),
    atom_pi(Text, PI),
    ground(PI),
    !,
    man_qualified_object_2(PI, Parent, LibOpt, Object, Section).
man_qualified_object(Object0, Parent, LibOpt, Object, Section) :-
    man_qualified_object_2(Object0, Parent, LibOpt, Object, Section).

man_qualified_object_2(Name/Arity, Parent,
                       LibOpt, Module:Name/Arity, Section) :-
    object_module(Parent, Module, Section, LibOpt),
    !.
man_qualified_object_2(Object, Parent, [], Object, Parent).


%!  man_synopsis(+Object, +Section)//
%
%   Give synopsis details for a  fully specified predicate indicator
%   and link this to the section.

:- public
    man_synopsis//2.                % called from man_match//2

man_synopsis(PI, Section) -->
    man_synopsis(PI, Section, []).

man_synopsis(PI, Section, Options) -->
    { object_href(Section, HREF)
    },
    object_synopsis(PI, [href(HREF)|Options]).

%!  object_module(+Section0, -Module, -Section, -LibOpt) is semidet.
%
%   Find the module documented by Section.

object_module(Section0, Module, Section, [source(Term)]) :-
    parent_section_ndet(Section0, Section),
    manual_object(Section, Title, _File, _Class, _Offset),
    (   once(sub_atom(Title, B, _, _, :)),
        sub_atom(Title, 0, B, _, Atom),
        catch(term_to_atom(Term, Atom), _, fail),
        ground(Term),
        Term = library(_)
    ->  !,
        absolute_file_name(Term, PlFile,
                           [ file_type(prolog),
                             access(read),
                             file_errors(fail)
                           ]),
        (   module_property(Module, file(PlFile))
        ->  true
        ;   xref_public_list(PlFile, -,         % module is not loaded
                             [ module(Module)
                             ])
        )
    ).

parent_section_ndet(Section, Section).
parent_section_ndet(Section, Parent) :-
    parent_section(Section, Parent0),
    parent_section_ndet(Parent0, Parent).


man_matches(Matches, Object, Options) -->
    { option(navtree(false), Options) },
    !,
    man_matches_nt(Matches, Object, Options).
man_matches(Matches, Object, Options) -->
    html([ div(class(navtree),
               div(class(navwindow),
                   \man_nav_tree(Object, Options))),
           div(class(navcontent),
               \man_matches_nt(Matches, Object, Options))
         ]).


man_matches_nt([Match], Object, Options) -->
    { option(footer(true), Options, true) },
    !,
    man_match(Match, Object, Options),
    object_page_footer(Object, []).
man_matches_nt(Matches, Object, Options) -->
    man_matches_list(Matches, Object, Options).

man_matches_list([], _, _) --> [].
man_matches_list([H|T], Obj, Options) -->
    man_match(H, Obj, Options),
    man_matches_list(T, Obj, Options).

%!  man_match(+Term, +Object, +Options)// is det.
%
%   If  possible,  insert  the  synopsis  into   the  title  of  the
%   description.

man_match(packages, packages, _) -->
    !,
    html({|html||
          <p>
          Packages are relatively independent add-on libraries that
          may not be available in all installations.  Packages are
          part of the source code releases of SWI-Prolog and may be
          enabled or disabled during the build.</p>

          <p>
          See also <a href="/pack/list">Add-ons</a> for extensions
          provided by the community that must be installed separately
          using
          <a href="/pldoc/doc_for?object=pack_install/1">pack_install/1</a>.</p>
         |}).
man_match(root, root, _) -->
    !,
    man_overview([]).
man_match((Parent+Path)-(Obj+[element(dt,A,C0)|DD]), Obj, Options) -->
    { \+ option(synopsis(false), Options),
      option(link_source(Link), Options, true),
      man_qualified_object(Obj, Parent, LibOpt, QObj, Section),
      !,
      C = [ span(style('float:right;margin-left:5px;'),
                 \object_source_button(QObj, [source_link(Link)]))
          | C0
          ]
    },
    dom_list([ element(dt,[],[\man_synopsis(QObj, Section, LibOpt)]),
               element(dt,A,C)
             | DD
             ], Path, Options).
man_match((_Parent+Path)-(Obj+DOM), Obj, Options) -->
    dom_list(DOM, Path, Options).


:- html_meta
    dom_list(html, +, +, ?, ?).

dom_list(_:[], _, _) -->
    !,
    [].
dom_list(M:[H|T], Path, Options) -->
    dom(H, Path, Options),
    dom_list(M:T, Path, Options).

dom(element(E, Atts, Content), Path, Options) -->
    !,
    dom_element(E, Atts, Content, Path, Options).
dom(CDATA, _, _) -->
    html(CDATA).

dom_element(a, _, [], _, _) -->                % Useless back-references
    !,
    [].
dom_element(a, Att, Content, Path, Options) -->
    { memberchk(href=HREF, Att),
      (   memberchk(class=Class, Att)
      ->  true
      ;   Class = unknown
      ),
      rewrite_ref(Class, HREF, Path, Myref, Options)
    },
    !,
    html(a(href(Myref), \dom_list(Content, Path, Options))).
dom_element(span, Att, [CDATA], _, Options) -->
    { memberchk(class='pred-ext', Att),
      atom_pi(CDATA, PI),
      documented(PI),
      (   option(server(false), Options)
      ->  public_link(predicate(CDATA), HREF)
      ;   http_link_to_id(pldoc_man, [predicate=CDATA], HREF)
      )
    },
    !,
    html(a(href(HREF), CDATA)).
dom_element(img, Att0, [], Path, _Options) -->
    { selectchk(src=Src, Att0, Att1),
      relative_file_name(ImgFile, Path, Src),
      handler_alias(Handler, DirAlias),
      absolute_file_name(DirAlias, Dir,
                         [ file_errors(fail),
                           solutions(all),
                           file_type(directory)
                         ]),
      ensure_slash(Dir, DirS),
      atom_concat(DirS, NewSrc, ImgFile),
      !,
      http_link_to_id(Handler, [], ManRef),
      directory_file_path(ManRef, NewSrc, NewPath),
      Begin =.. [img, src(NewPath) | Att1]
    },
    html_begin(Begin),
    html_end(img).
dom_element(div, Att, _, _, _) -->
    { memberchk(class=navigate, Att) },
    !.
dom_element(html, _, Content, Path, Options) -->
    !,                              % do not emit a html for the second time
    dom_list(Content, Path, Options).
dom_element(head, _, Content, Path, Options) -->
    !,                              % do not emit a head for the second time
    dom_list(Content, Path, Options).
dom_element(title, _, _, _, _) --> !.
dom_element(link, _, _, _, _) --> !.
dom_element(body, _, Content, Path, Options) -->
    !,                              % do not emit a body for the second time
    dom_list(Content, Path, Options).
dom_element(Name, Attrs, Content, Path, Options) -->
    { Begin =.. [Name|Attrs] },
    html_begin(Begin),
    dom_list(Content, Path, Options),
    html_end(Name).

handler_alias(manual_file,   swi_man_manual(.)).
handler_alias(pldoc_package, swi_man_packages(.)).

ensure_slash(Dir, DirS) :-
    (   sub_atom(Dir, _, _, 0, /)
    ->  DirS = Dir
    ;   atom_concat(Dir, /, DirS)
    ).

%!  public_link(+Spec, -HREF)
%
%   We do not have a web server.  Create   a  link  to the public server
%   instead.
%
%   @bug The predicate may not be there.

public_link(predicate(CDATA), HREF) :-
    uri_encoded(query_value, CDATA, Encoded),
    atom_concat('https://www.swi-prolog.org/pldoc/doc_for?object=',
                Encoded, HREF).


%!  documented(+PI) is semidet.
%
%   True if we have documentation about PI

documented(PI) :-
    manual_object(PI, _, _, _, _),
    !.
documented(PI) :-
    full_object(PI, _Obj).

%!  rewrite_ref(+Class, +Ref0, +Path, -ManRef, +Options) is semidet.
%
%   Rewrite Ref0 from the HTML reference manual format to the server
%   format. Reformatted:
%
%       $ File#Name/Arity :
%       Local reference using the manual presentation
%       =|/man?predicate=PI|=.
%
%       $ File#sec:NR :
%       Rewrite to =|section(Level, NT, ID, FilePath)|=
%
%       $ File#flag:Name :
%       Rewrite to =|section(Level, NT, ID, FilePath)#flag:Name|=
%
%       $ File#gloss:Name :
%       Rewrite to =|section(Level, NT, ID, FilePath)#gloss:Name|=
%
%       $ File#Name()
%       Rewrite to /man/CAPI=Name
%
%   @param Class    Class of the <A>.  Supported classes are
%
%           | sec   | Link to a section     |
%           | pred  | Link to a predicate   |
%           | flag  | Link to a Prolog flag |
%           | gloss | Link to a glossary    |
%
%   @param Ref0     Initial reference from the =a= element
%   @param Path     Currently loaded file
%   @param ManRef   PlDoc server reference

rewrite_ref(_Class, Ref, _Path, Ref, Options) :-
    option(server(false), Options),
    !.
rewrite_ref(Class, Ref0, Path, ManRef, _Options) :-
    rewrite_ref(Class, Ref0, Path, ManRef).

rewrite_ref(pred, Ref0, _, Ref) :-              % Predicate/DCG reference
    sub_atom(Ref0, _, _, A, '#'),
    !,
    sub_atom(Ref0, _, A, 0, Fragment),
    atom_to_object(Fragment, PI),
    manual_object(PI, _, _, _, _),
    uri_encoded(query_value, Fragment, Enc),
    http_location_by_id(pldoc_man, ManHandler),
    format(string(Ref), '~w?predicate=~w', [ManHandler, Enc]).
rewrite_ref(function, Ref0, _, Ref) :-          % Arithmetic function reference
    sub_atom(Ref0, _, _, A, '#'),
    !,
    sub_atom(Ref0, _, A, 0, Fragment),
    atom_to_object(Fragment, PI),
    manual_object(PI, _, _, _, _),
    PI=f(Name/Arity),
    format(atom(PIName), '~w/~w', [Name,Arity]),
    uri_encoded(query_value, PIName, Enc),
    http_location_by_id(pldoc_man, ManHandler),
    format(string(Ref), '~w?function=~w', [ManHandler, Enc]).
rewrite_ref(func, Ref0, _, Ref) :-              % C-API reference
    sub_atom(Ref0, _, _, A, '#'),
    !,
    sub_atom(Ref0, _, A, 0, Fragment),
    atom_to_object(Fragment, Obj),
    manual_object(Obj, _, _, _, _),
    Obj = c(Function),
    uri_encoded(query_value, Function, Enc),
    http_location_by_id(pldoc_man, ManHandler),
    format(string(Ref), '~w?CAPI=~w', [ManHandler, Enc]).
rewrite_ref(sec, Ref0, Path, Ref) :-            % Section inside a file
    sub_atom(Ref0, B, _, A, '#'),
    !,
    sub_atom(Ref0, _, A, 0, Fragment),
    sub_atom(Ref0, 0, B, _, File),
    referenced_section(Fragment, File, Path, Section),
    object_href(Section, Ref).
rewrite_ref(sec, File, Path, Ref) :-            % Section is a file
    file_directory_name(Path, Dir),
    atomic_list_concat([Dir, /, File], SecPath),
    Obj = section(_, _, _, SecPath),
    manual_object(Obj, _, _, _, _),
    !,
    object_href(Obj, Ref).
rewrite_ref(cite, Ref0, Path, Ref) :-           % Citation (bit hard-wired)
    debug(pldoc(cite), 'Cite ref ~q ~q', [Ref0, Path]),
    sub_atom(Ref0, _, _, A, '#'),
    !,
    sub_atom(Ref0, _, A, 0, Fragment),
    uri_encoded(query_value, Fragment, Enc),
    http_location_by_id(pldoc_man, ManHandler),
    format(string(Ref), '~w?section=bibliography#~w', [ManHandler, Enc]).
rewrite_ref(flag, Ref0, Path, Ref) :-
    sub_atom(Ref0, B, _, A, '#'),
    !,
    sub_atom(Ref0, 0, B, _, File),
    sub_atom(Ref0, _, A, 0, Fragment),
    file_directory_name(Path, Dir),
    atomic_list_concat([Dir, /, File], SecPath),
    Obj = section(_, _, _, SecPath),
    manual_object(Obj, _, _, _, _),
    !,
    object_href(Obj, Ref1),
    format(string(Ref), '~w#~w', [Ref1, Fragment]).
rewrite_ref(gloss, Ref0, Path, Ref) :-
    sub_atom(Ref0, B, _, A, '#'),
    !,
    sub_atom(Ref0, 0, B, _, File),
    sub_atom(Ref0, _, A, 0, Fragment),
    file_directory_name(Path, Dir),
    atomic_list_concat([Dir, /, File], SecPath),
    Obj = section(_, _, _, SecPath),
    manual_object(Obj, _, _, _, _),
    !,
    object_href(Obj, Ref1),
    format(string(Ref), '~w#~w', [Ref1, Fragment]).

%!  referenced_section(+Fragment, +File, +Path, -Section)

referenced_section(Fragment, File, Path, section(Level, Nr, ID, SecPath)) :-
    atom_concat('sec:', Nr, Fragment),
    (   File == ''
    ->  SecPath = Path
    ;   file_directory_name(Path, Dir),
        atomic_list_concat([Dir, /, File], SecPath)
    ),
    manual_object(section(Level, Nr, ID, SecPath), _, _, _, _).


%!  man_links(+ParentPaths, +Options)// is det.
%
%   Create top link structure for manual pages.

man_links(ParentPaths, Options) -->
    prolog:doc_page_header(parents(ParentPaths), Options),
    !.
man_links(ParentPaths, Options) -->
    { option(links(true), Options, true),
      option(header(true), Options, true)
    },
    !,
    html([ div(class(navhdr),
               [ div(class(jump), \man_parent(ParentPaths)),
                 div(class(search), \search_form(Options)),
                 br(clear(right))
               ]),
           p([])
         ]).
man_links(_, _) -->
    [].

man_parent(ParentPaths) -->
    { maplist(parent_to_section, ParentPaths, [Section|MoreSections]),
      maplist(=(Section), MoreSections)
    },
    !,
    object_ref(Section, [secref_style(number_title)]).
man_parent(_) --> [].

parent_to_section(X+_, X) :-
    X = section(_,_,_,_),
    !.
parent_to_section(File+_, Section) :-
    atom(File),
    manual_object(Section, _Title, File, _Class, _Offset),
    !.

%!  section_link(+Obj, +Options)// is det.
%
%   Create link to a section.  Options recognised:
%
%           * secref_style(+Style)
%           One of =number=, =title= or =number_title=.

section_link(Section, Options) -->
    { option(secref_style(Style), Options, number)
    },
    section_link(Style, Section, Options).

section_link(number, section(_, Number, _, _), _Options) -->
    !,
    (   {Number == '0'}             % Title.  Package?
    ->  []
    ;   html(['Sec. ', Number])
    ).
section_link(title, Obj, _Options) -->
    !,
    { manual_object(Obj, Title, _File, _Class, _Offset)
    },
    html(Title).
section_link(_, Obj, _Options) -->
    !,
    { Obj = section(_, Number, _, _),
      manual_object(Obj, Title, _File, _Class, _Offset)
    },
    (   { Number == '0' }
    ->  html(Title)
    ;   html([Number, ' ', Title])
    ).

%!  function_link(+Function, +Options) is det.
%
%   Create a link to a C-function

function_link(Function, _) -->
    html([Function, '()']).


                 /*******************************
                 *       INDICES & OVERVIEW     *
                 *******************************/

%!  man_overview(+Options)// is det.
%
%   Provide a toplevel overview on the  manual: the reference manual
%   and the available packages.

man_overview(Options) -->
    { http_absolute_location(pldoc_man(.), RefMan, [])
    },
    html([ h1('SWI-Prolog documentation'),
           blockquote(class(refman_link),
                      a(href(RefMan),
                        'SWI-Prolog reference manual')),
           \package_overview(Options),
           \paperback(Options)
         ]).

package_overview(Options) -->
    html([ h2(class(package_doc_title),
              'SWI-Prolog package documentation'),
           blockquote(class(package_overview),
                      \packages(Options))
         ]).

packages(Options) -->
    { findall(Pkg, current_package(Pkg), Pkgs)
    },
    packages(Pkgs, Options).

packages([], _) -->
    [].
packages([Pkg|T], Options) -->
    package(Pkg, Options),
    packages(T, Options).

package(pkg(Title, HREF, HavePackage), Options) -->
    { package_class(HavePackage, Class, Options)
    },
    html(div(class(Class),
             a([href(HREF)], Title))).

package_class(true,  pkg_link, _).
package_class(false, no_pkg_link, _).

current_package(pkg(Title, HREF, HavePackage)) :-
    manual_object(section(0, _, _, _), Title, File, packages, _),
    file_base_name(File, FileNoDir),
    file_name_extension(Base, _, FileNoDir),
    (   exists_source(library(Base))
    ->  HavePackage = true
    ;   HavePackage = false
    ),
    http_absolute_location(pldoc_pkg(FileNoDir), HREF, []).


:- http_handler(pldoc(jpl),      pldoc_jpl,              [prefix]).
:- http_handler(pldoc_pkg(.),    pldoc_package,          [prefix]).
:- http_handler(pldoc_man(.),    pldoc_refman,           [prefix]).
:- http_handler(pldoc(packages), pldoc_package_overview, []).

%!  pldoc_jpl(+Request)
%
%   Hack to include JPL documentation in server.

pldoc_jpl(Request) :-
    memberchk(path_info(JPLFile), Request),
    atom_concat('doc/packages/jpl', JPLFile, Path),
    http_reply_file(swi(Path), [], Request).

%!  pldoc_package(+Request)
%
%   HTTP  handler  for   PlDoc    package   documentation.   Accepts
%   /pldoc/package/<package>.{html,gif}.          The           path
%   =/pldoc/package/<package>= is redirected to the canonical object
%   version.

pldoc_package(Request) :-
    (   \+ option(path_info(_), Request)
    ->  true
    ;   option(path_info(/), Request)
    ),
    http_link_to_id(pldoc_object, [object=packages], HREF),
    http_redirect(see_other, HREF, Request).
pldoc_package(Request) :-
    memberchk(path_info(Img), Request),
    file_mime_type(Img, image/_),
    !,
    http_reply_file(swi_man_packages(Img), [], Request).
pldoc_package(Request) :-
    memberchk(path_info('jpl'), Request),
    !,
    memberchk(path(Path0), Request),
    atom_concat(Path0, /, Path),
    http_redirect(moved, Path, Request).
pldoc_package(Request) :-
    memberchk(path_info(JPLFile), Request),
    (   JPLFile == 'jpl/'
    ->  Path = 'doc/packages/jpl/index.html'
    ;   sub_atom(JPLFile, 0, _, _, 'jpl/')
    ->  atom_concat('doc/packages/', JPLFile, Path)
    ),
    http_reply_file(swi(Path), [], Request).
pldoc_package(Request) :-
    memberchk(path_info(PkgDoc), Request),
    ensure_html_ext(PkgDoc, PkgHtml),
    atom_concat('packages/', PkgHtml, Path),
    term_to_atom(section(Path), Object),
    http_link_to_id(pldoc_object, [object=Object], HREF),
    http_redirect(see_other, HREF, Request).

ensure_html_ext(Pkg, PkgHtml) :-
    file_name_extension(_, html, Pkg),
    !,
    PkgHtml = Pkg.
ensure_html_ext(Pkg, PkgHtml) :-
    file_name_extension(Pkg, html, PkgHtml).

%!  pldoc_package_overview(+Request)
%
%   Provide an overview of the package documentation

pldoc_package_overview(_Request) :-
    reply_html_page(
        pldoc(packages),
        title('SWI-Prolog package documentation'),
        \package_overview([])).

%!  paperback(+Options)//
%
%   Link to the paperback version of the manual.

paperback(_Options) -->
    { expand_url_path(swipl_book(.), HREF)
    },
    html([ h2('The manual as a book'),
           p([ 'A paperback version of the manual is ',
               a(href(HREF), 'available'), '.'
             ])
         ]).

%!  pldoc_refman(+Request)
%
%   HTTP handler for PlDoc Reference Manual access.  Accepts
%   /refman/[<package>.html.]

pldoc_refman(Request) :-
    memberchk(path_info(Section), Request),
    \+ sub_atom(Section, _, _, _, /),
    Obj = section(0,_,_,_),
    manual_object(Obj, Title, File, manual, _),
    file_base_name(File, Section),
    !,
    reply_html_page(pldoc(man),
                    title(Title),
                    \object_page(Obj, [])).
pldoc_refman(Request) :-                % server Contents.html
    \+ memberchk(path_info(_), Request),
    !,
    http_link_to_id(pldoc_object, [object(manual)], HREF),
    http_redirect(see_other, HREF, Request).
pldoc_refman(Request) :-
    memberchk(path(Path), Request),
    existence_error(http_location, Path).


                 /*******************************
                 *          HOOK SEARCH         *
                 *******************************/

prolog:doc_object_summary(section(ID), Class, File, Summary) :-
    nonvar(ID),                     % when generating, only do full ones
    manual_object(section(_Level, _No, ID, _Path), Summary, File, Class, _Offset).
prolog:doc_object_summary(Obj, Class, File, Summary) :-
    manual_object(Obj, Summary, File, Class, _Offset).

prolog:doc_object_page(Obj, Options) -->
    man_page(Obj, [no_manual(fail),footer(false)|Options]).

%!  prolog:doc_object_link(+Obj, +Options)//
%
%   Provide the HTML to describe Obj for linking purposes.

prolog:doc_object_link(Obj, Options) -->
    { Obj = section(_,_,_,_) },
    !,
    section_link(Obj, Options).
prolog:doc_object_link(Obj0, Options) -->
    { Obj0 = section(ID),
      Obj = section(_Level, _No, ID, _Path),
      manual_object(Obj, _, _, _, _)
    },
    !,
    section_link(Obj, Options).
prolog:doc_object_link(Obj, Options) -->
    { Obj = c(Function) },
    !,
    function_link(Function, Options).
prolog:doc_object_link(root, _) -->
    !,
    html('Documentation').
prolog:doc_object_link(manual, _Options) -->
    !,
    html('Reference manual').
prolog:doc_object_link(packages, _) -->
    html('Packages').

prolog:doc_category(manual,   30, 'Reference Manual').
prolog:doc_category(packages, 40, 'Packages').

prolog:doc_file_index_header(File, Options) -->
    { Section = section(_Level, _No, _ID, File),
      manual_object(Section, _Summary, File, _Cat, _Offset)
    },
    !,
    html(tr(th([colspan(3), class(section)],
               [ \object_ref(Section,
                             [ secref_style(number_title)
                             | Options
                             ])
               ]))).

prolog:doc_object_title(Obj, Title) :-
    Obj = section(_,_,_,_),
    manual_object(Obj, Title, _, _, _),
    !.

prolog:doc_canonical_object(section(_Level, _No, ID, _Path),
                            section(ID)).

%!  prolog:doc_object_href(+Object, -HREF) is semidet.
%
%   Produce a HREF for section objects.

prolog:doc_object_href(section(ID), HREF) :-
    nonvar(ID),
    atom_concat('sec:', Sec, ID),
    http_link_to_id(pldoc_man, [section(Sec)], HREF).
prolog:doc_object_href(section(_Level, _No, ID, _Path), HREF) :-
    nonvar(ID),
    atom_concat('sec:', Sec, ID),
    http_link_to_id(pldoc_man, [section(Sec)], HREF).


                 /*******************************
                 *           MESSAGES           *
                 *******************************/

:- multifile prolog:message//1.

prolog:message(pldoc(no_section_id(File, Title))) -->
    [ 'PlDoc: ~w: no id for section "~w"'-[File, Title] ].
prolog:message(pldoc(duplicate_ids(L))) -->
    [ 'PlDoc: duplicate manual section IDs:'-[], nl
    ],
    duplicate_ids(L).

duplicate_ids([]) --> [].
duplicate_ids([H|T]) --> duplicate_id(H), duplicate_ids(T).

duplicate_id(Id) -->
    { findall(File, manual_object(section(_,_,Id,File),_,_,_,_), Files) },
    [ '    ~w: ~p'-[Id, Files], nl ].

