/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1997-2020, University of Amsterdam
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

:- module(latex2html4pl, []).
:- use_module(latex2html,
              [ latex2html_module/0,
                tex_load_commands/1,
                translate_table/3,
                clean_tt/2,
                add_to_index/2,
                add_to_index/1,
                translate_section/5,
                translate_section/4,
                op(_,_,_)
              ]).
:- autoload(library(apply),[maplist/3,maplist/2]).
:- autoload(library(lists),[append/3,delete/3]).
:- autoload(library(occurs),[sub_term/2]).
:- autoload(library(readutil),[read_line_to_codes/2]).

:- latex2html_module.
:- tex_load_commands(pl).

                 /*******************************
                 *             MACROS           *
                 *******************************/

#(defitem(Class,Label), [ html(Begin), Label, html('</dt>'),
                          html('<dd class="defbody">')
                        ]) :-
    format(atom(Begin), '<dt class="~w">', [Class]).
#(defitem(Label),       [ html('<dt>'), Label,
                          html('<dd class="defbody">')
                        ]).
#(predtag(Value),       [ html('<span class="pred-tag">'), Value,
                          html('</span>')
                        ]).
#(mod(Module),          [ html('<span class="module">'), Module,
                          html('</span>')
                        ]).

                 /*******************************
                 *         ENVIRONMENTS         *
                 *******************************/

list_command(tags,       _, html('<dl class="tags">'), html('</dl>')).

env(tabularlp([{_}], Tokens), HTML) :-
    translate_table(ll, Tokens, HTML).

                 /*******************************
                 *          INDEX HACK          *
                 *******************************/

cmd(+, index, nospace('\\+')).
cmd(=, index, nospace('\\=')).

                 /*******************************
                 *           COMMANDS           *
                 *******************************/

cmd(spaces({X}), html(Spaces)) :-
    atom_number(X, N),
    n_list(N, '&nbsp;', L),
    atomic_list_concat(L, Spaces).
cmd(hrule, html('<hr>')).
cmd(bug({TeX}), #footnote(bug, +TeX)).
cmd(fileext({Ext}), #code(Text)) :-
    format(string(Text), '.~w', [Ext]).

cmd(var(                {A1}), #var(+A1)).
cmd(arg(                {A1}), #var(+A1)).
cmd(metafile(           {A1}), #code(+A1)).
cmd(file(               {A0}), #code(A1)) :-
    clean_tt(A0, A1).
cmd(clib(               {A1}), #code(+A1)).
cmd(cglobalvar(         {A1}), #code(+A1)).
cmd(ctype(              {A1}), #code(+A1)).
cmd(type(               {A1}), #code(+A1)).
cmd(pllib(              {A1}), #code([library, #embrace(+A1)])).
cmd(hook(               {_A1}), #i(#embrace(hook))).
cmd(env(                {A1}), #code(+A1)).
cmd(program(            {A1}), #b(A1)).
cmd(exam(               {A1}), #code(+A1)).
cmd(jargon(             {A1}), #em(+A1)).
cmd(chr(                {A1}), #code(+A1)).
cmd(const(              {A1}), #code(+A1)).
cmd(resource(           {A1}), #code(A1)).
cmd(key(                {A1}), #code(+A1)).
cmd(plflag(             {A1}), #code(+A1)).
cmd(module(             {A1}), #code(+A1)).
cmd(except(             {A1}), #code(+A1)).
cmd(op(                 {A1}), #strong(+A1)).
cmd(cmdlineoption(      {A1}), #strong(+A1)).
cmd(menu({A1},{[]}),           #strong(+A1)).
cmd(menu({A1},{A2}),           [#strong(+A1), ' ', #embrace(#code(+A2))]).
cmd(longoption(    {A1},{[]}), [#strong([nospace(--), +A1])]).
cmd(longoption(    {A1},{A2}), [#strong([nospace(--), +A1,
                                         nospace(=)]), #var(+A2)]).
cmd(fmtseq(             {A1}), #code(A1)).
cmd(versionshort,           _, nospace(Version)) :-
    current_prolog_flag(version, V),
    Major is V // 10000,
    Minor is (V // 100) mod 100,
    Patch is V mod 100,
    atomic_list_concat([Major, Minor, Patch], '.', Version).
cmd(bnfor, '|').
cmd(bnfmeta({Meta}), [nospace('<'), #var(+Meta), nospace('>')]).
cmd(argoption({RawName}, {ArgName}),
    [ #strong(Name), ' ', #var(ArgName)
    ]) :-
    clean_tt(RawName, Name).
cmd(predref({RawName}, {Arity}), #lref(pred, RefName, Text)) :-
    clean_name(RawName, Name),
    predicate_refname(Name, Arity, RefName),
    format(string(Text), '~w/~w', [Name, Arity]).
cmd(funcref({RawName}, {Arity}), #lref(function, RefName, Text)) :-
    clean_name(RawName, Name),
    function_refname(Name, Arity, RefName),
    format(string(Text), '~w/~w', [Name, Arity]).
cmd(dcgref({RawName}, {DCGArity}), #lref(pred, RefName, Text)) :-
    clean_name(RawName, Name),
    atom_number(DCGArity, Arity),
    dcg_refname(Name, Arity, RefName),
    format(string(Text), '~w//~w', [Name, Arity]).
cmd(qpredref({Module}, {RawName}, {Arity}), #lref(pred, RefName, Text)) :-
    clean_name(RawName, Name),
    predicate_refname(Module:Name, Arity, RefName),
    format(string(Text), '~w:~w/~w', [Module, Name, Arity]).
cmd(qdcgref({Module}, {RawName}, {DCGArity}), #lref(pred, RefName, Text)) :-
    clean_name(RawName, Name),
    atom_number(DCGArity, Arity),
    dcg_refname(Module:Name, Arity, RefName),
    format(string(Text), '~w:~w//~w', [Module, Name, DCGArity]).
cmd(nopredref({RawName}, {Arity}), Text) :-
    clean_name(RawName, Name),
    format(string(Text), '~w/~w', [Name, Arity]).
cmd(libpredref({RawName}, {Arity}), Text) :-
    clean_name(RawName, Name),
    format(string(Text), '~w/~w', [Name, Arity]).
cmd(nodcgref({RawName}, {Arity}), Text) :-
    clean_name(RawName, Name),
    format(string(Text), '~w//~w', [Name, Arity]).
cmd(prologflag({Name}), #lref(flag, RefName, Name)) :-
    atom_concat('flag:', Name, RefName).
cmd(functor({Name}, {Arity}), #code([+Name, nospace(/), +Arity])).
cmd(compound({Name}, {Args}), #code([+Name, #embrace(+Args)])).
cmd(term({Name}, {[]}), #code([+Name])) :- !.
cmd(term({Name}, {Args}), #code([+Name, #embrace(+Args)])).
cmd(errorterm({Name}, {Args}), #code([+Name, #embrace(+Args)])).
cmd(infixterm({RawName},{A1},{A2}), #code([+A1, Op, +A2])) :-
    clean_name(RawName, Name),
    (   nospace_op(Name)
    ->  Op = nospace(Name)
    ;   Op = Name
    ).
cmd(prefixterm({RawName},{A1}), #code([+A1, Name])) :-
    clean_name(RawName, Name).
cmd(manref({RawName}, {Section}),
    [#strong(Name), #embrace(Section)]) :-
    clean_tt(RawName, Name).
cmd(cfuncref({RawName}, {Args}),
    #lref(func, RefName, [Name, #embrace(+Args)])) :-
    clean_name(RawName, Name),
    cfunction_refname(Name, RefName).
cmd(definition({Tag}),
    #defitem(#b(+Tag))).
cmd('DCG'(A,B,C), X) :-
    cmd(predicate(A,B,C), X).
cmd(predicate(A, {RawName}, {'0'}, {_}),
    #defitem(Class, Content)) :-
    pred_class(A, Class),
    pred_tag(A, Content, [#label(RefName, #strong(Name))]),
    clean_name(RawName, Name),
    predicate_refname(Name, 0, RefName),
    add_to_index(RefName, +RefName).
cmd(predicate(A, {RawName}, {Arity}, {Args}),
    #defitem(Class, Content)) :-
    pred_class(A, Class),
    pred_tag(A, Content,
             [#label(RefName, [#strong(Name), #embrace(#var(+Args))])]),
    clean_name(RawName, Name),
    predicate_refname(Name, Arity, RefName),
    add_to_index(RefName, +RefName).
cmd(function(A, {RawName}, {'0'}, {_}),
    #defitem(Class, Content)) :-
    pred_class(A, Class),
    pred_tag(A, Content, [#label(RefName, #strong(Name))]),
    clean_name(RawName, Name),
    function_refname(Name, 0, RefName),
    add_to_index(RefName, +RefName).
cmd(function(A, {RawName}, {Arity}, {Args}),
    #defitem(Class, Content)) :-
    pred_class(A, Class),
    pred_tag(A, Content,
             [#label(RefName, [#strong(Name), #embrace(#var(+Args))])]),
    clean_name(RawName, Name),
    function_refname(Name, Arity, RefName),
    add_to_index(RefName, +RefName).
cmd(dictfunction(A, {RawName}, {Arity}, {Args}),
    #defitem(Class, Content)) :-
    pred_class(A, Class),
    pred_tag(A, Content,
             [#label(RefName, [#strong(Name), #embrace(#var(+Args))])]),
    clean_name(RawName, Name),
    format(string(RefName), 'm-~w-~w', [Name, Arity]),
    add_to_index(RefName, +RefName).
cmd(qpredicate(A, {RawM}, {RawName}, {'0'}, {_}),
    #defitem(Class, Content)) :-
    pred_class(A, Class),
    pred_tag(A, Content, [#label(RefName, [#mod(Module), nospace(:), #strong(Name)])]),
    clean_name(RawM, Module),
    clean_name(RawName, Name),
    predicate_refname(Module:Name, 0, RefName),
    add_to_index(RefName, +RefName).
cmd(qpredicate(A, {RawM}, {RawName}, {Arity}, {Args}),
    #defitem(Class, Content)) :-
    pred_class(A, Class),
    pred_tag(A, Content,
             [#label(RefName, [#mod(Module), nospace(:), #strong(Name), #embrace(#var(+Args))])]),
    clean_name(RawM, Module),
    clean_name(RawName, Name),
    predicate_refname(Module:Name, Arity, RefName),
    add_to_index(RefName, +RefName).
cmd(dcg(A, {RawName}, {'0'}, {_}),
    #defitem(pubdef, Content)) :-
    pred_tag(A, Content, [#label(RefName, #strong(Name)), #code(//)]),
    clean_name(RawName, Name),
    dcg_refname(Name, 0, RefName),
    add_to_index(RefName, +RefName).
cmd(dcg(A, {RawName}, {ArityS}, {Args}),
    #defitem(pubdef, Content)) :-
    pred_tag(A, Content,
             [ #label(RefName,
                      [ #strong(Name), #embrace(#var(+Args))
                      ]),
               #code(//)
             ]),
    clean_name(RawName, Name),
    atom_number(ArityS, Arity),
    dcg_refname(Name, Arity, RefName),
    add_to_index(RefName, +RefName).
cmd(directive(A, {RawName}, {'0'}, {_}),
    #defitem(pubdef, Content)) :-
    pred_tag(A, Content,
             [ #label(RefName,
                      [ ':- ', #strong(Name)
                      ])
             ]),
    clean_name(RawName, Name),
    predicate_refname(Name, 0, RefName),
    add_to_index(RefName, +RefName).
cmd(directive(A, {RawName}, {Arity}, {Args}),
    #defitem(pubdef, Content)) :-
    pred_tag(A, Content,
             [ #label(RefName,
                      [ ':- ', #strong(Name), #embrace(#var(+Args))
                      ])
             ]),
    clean_name(RawName, Name),
    predicate_refname(Name, Arity, RefName),
    add_to_index(RefName, +RefName).
cmd(cfunction({RType}, {RawName}, {Args}),
    #defitem(pubdef, #label(RefName,
                            [ #var(RType), ' ', #strong(+RawName),
                              #embrace(#var(+Args))
                            ]))) :-
    clean_name(RawName, Name),
    cfunction_refname(Name, RefName),
    add_to_index(RefName, +RefName).
cmd(cmacro({RType}, {Name}, {Args}),
    #defitem(pubdef, #label(RefName,
                            [ #var(RType), ' ', #strong(Name),
                              #embrace(#var(+Args))
                            ]))) :-
    cfunction_refname(Name, RefName),
    add_to_index(RefName, +RefName).
cmd(resitem({Resource}),
    #defitem(pubdef, #label(Resource,
                            [ #strong(Resource)
                            ]))) :-
    add_to_index(Resource, +Resource).
cmd(prefixop(A, {RawName}, {Arg}),
    #defitem(pubdef, Content)) :-
    pred_tag(A, Content,
             #label(RefName, [#strong(Name), ' ', #var(+Arg)])),
    clean_name(RawName, Name),
    predicate_refname(Name, 1, RefName),
    add_to_index(RefName, +RefName).
cmd(prefixfunction(A, {RawName}, {Arg}),
    #defitem(pubdef, Content)) :-
    pred_tag(A, Content,
             #label(RefName, [#strong(Name), ' ', #var(Arg)])),
    clean_name(RawName, Name),
    function_refname(Name, 1, RefName),
    add_to_index(RefName, +RefName).
cmd(infixop(A, {RawName}, {Arg1}, {Arg2}),
    #defitem(pubdef, Content)) :-
    pred_tag(A, Content,
             #label(RefName,
                    [ #var(Arg1), ' ', #strong(Name), ' ', #var(Arg2)
                    ])),
    clean_name(RawName, Name),
    predicate_refname(Name, 2, RefName),
    add_to_index(RefName, +RefName).
cmd(infixfunction(A, {RawName}, {Arg1}, {Arg2}),
    #defitem(pubdef, Content)) :-
    pred_tag(A, Content,
             #label(RefName,
                    [ #var(Arg1), ' ', #strong(Name), ' ', #var(Arg2)
                    ])),
    clean_name(RawName, Name),
    function_refname(Name, 2, RefName),
    add_to_index(RefName, +RefName).
cmd(constitem({Name}), #defitem(#label(RefName, #strong(+Name)))) :-
    clean_name(Name, RefName),
    add_to_index(RefName, +RefName).
cmd(termitem({Name}, {[]}), #defitem(#strong(+Name))).
cmd(termitem({Name}, {Arg}),
    #defitem([#strong(+Name), #embrace(#var(+Arg))])).
cmd(curltermitem({Arg}),
    #defitem([#embrace("{}", #var(+Arg))])).
cmd(dictitem({Name}, {Arg}),
    #defitem([#strong(+Name), #embrace("{}", #var(+Arg))])).
cmd(prefixtermitem({Name}, {Right}),
    #defitem([#strong(+Name), ' ', #var(+Right)])).
cmd(infixtermitem({Name}, {Left}, {Right}),
    #defitem([#var(+Left), Sep, #strong(+Name), Sep, #var(+Right)])) :-
    clean_name(Name, Clean),
    (   nospace_op(Clean)
    ->  Sep = []
    ;   Sep = ' '
    ).
cmd(prologflagitem({Name}, {Type}, {Access}),
    #defitem(pubdef, #label(RefName, [#strong(Name), #embrace([#var(Type)|Change])]))) :-
    atom_concat('flag:', Name, RefName),
    (   Access == r
    ->  Change = []
    ;   Change = nospace(', changeable')
    ).
cmd(fmtchar({Name}), [html('<li>'), #code(+Name), html('<br>')]).
cmd(optionval({Value}), #defitem(#strong(+Value))).
cmd(cmdlineoptionitem(M, {Option}, {Arg}),
    #defitem([#strong(+Option), Sep, #var(+Arg)])) :-
    (   M = *
    ->  Sep = []
    ;   Sep = [' ']
    ).
cmd(longoptionitem({Name}, {[]}), #defitem(#strong([nospace(--), +Name]))).
cmd(longoptionitem({Name}, {Arg}), #defitem(#strong([nospace(--), +Name,
                                                     nospace(=),
                                                     #var(+Arg)]))).
cmd(optionarg({Option}, {Arg}),
    #defitem([#strong(Option), #var(Arg)])).
cmd(traceoption({CharSpec}, {Name}, {Description}),
    [ #defitem([#strong(Name), ' ', #embrace(#code(Char))]),
      +Description
    ]) :-
    clean_name(CharSpec, Char).
cmd(pleaseoption({Name}, {Type}, {Default}),
    #defitem([ #strong(Name), ' ', #embrace(#var(Type)), ' ',
               'Default:', ' ', Default
             ])).
cmd(featureoption({Name}, {Type}),
    #defitem([#strong(Name), ' ', #embrace(#var(Type))])).
cmd(menuitem({Name}, {[]}),
    #defitem(#label(RefName, #strong(+Name)))) :-
    clean_name(Name, RefName0),
    atom_concat('menu:', RefName0, RefName),
    atomic_list_concat(Name, ' ', Atom),
    add_to_index(Atom, +RefName).
cmd(menuitem({Name}, {Arg}),
    #defitem([#label(RefName, #strong(+Name)), ' ', #embrace(#var(+Arg))])) :-
    clean_name(Name, RefName0),
    atom_concat('menu:', RefName0, RefName),
    atomic_list_concat(Name, ' ', Atom),
    add_to_index(Atom, +RefName).
cmd(escapeitem({Name}), #defitem(#code([nospace('\\'), +Name]))).
cmd(ttdef({Def}), #defitem(#code(+Def))).
cmd(predicatesummary({RawName}, {Arity}, {Summary}),
    #row([#predref(Name, Arity), +Summary])) :-
    clean_name(RawName, Name).
cmd(dcgsummary({RawName}, {Arity}, {Summary}),
    #row([#dcgref(Name, Arity), +Summary])) :-
    clean_name(RawName, Name).
cmd(oppredsummary({RawName}, {Arity}, {_Assoc}, {_Pri}, {Summary}),
    #row([#predref(Name, Arity), +Summary])) :-
    clean_name(RawName, Name).
cmd(functionsummary({RawName}, {Arity}, {Summary}),
    #row([#funcref(Name, Arity), +Summary])) :-
    clean_name(RawName, Name).
cmd(opfuncsummary({RawName}, {Arity}, {_Assoc}, {_Pri}, {Summary}),
    #row([#funcref(Name, Arity), +Summary])) :-
    clean_name(RawName, Name).
cmd(opsummary({Pri}, {Assoc}, {RawName}, {Summary}),
    #row([Pri, Assoc, Name, +Summary])) :-
    clean_name(RawName, Name).

cmd(texcmd({Name}), #code([nospace(\), Name])).
cmd(texenv({Name}), #code(Name)).
cmd(texmode({Name}), #var(Name)).

% C++ Documentation (packages/cpp)

cmd(classitem({Class}),
    #defitem(#label(RefName, #strong(Class)))) :-
    format(string(RefName), 'class:~w', [Class]).
cmd(constructor({Class}, {Args}),
    #defitem([#strong([Class, ::, Class]), #embrace(#var(+Args))])).
cmd(destructor({Class}),
    #defitem([#strong([~, Class]), #embrace(#var(''))])).
cmd(cppcast({Class}, {Type}),
    #defitem([#strong([Class, '::operator', Type]), #embrace(#var(void))])).
cmd(nodescription, []).

% Some XPCE things

cmd(class({Name}),              #lref(Label, Name)) :-
    atom_concat('class:', Name, Label),
    add_to_index(Name).
cmd(noclass({Name}),            #i(Name)).
cmd(menuref({A1}),              #lref(RefName, Name)) :-
    clean_name(A1, RefName0),
    atom_concat('menu:', RefName0, RefName),
    atomic_list_concat(A1, ' ', Name),
    add_to_index(Name).

% Glossary support

cmd(glossitem({Term}), #defitem(#label(RefName, #strong(Term)))) :-
    canonicalise_glossitem(Term, Ref),
    format(string(RefName), 'gloss:~w', [Ref]).
cmd(g({Term}),  #lref(gloss, RefName, Term)) :-
    canonicalise_glossitem(Term, Ref),
    format(string(RefName), 'gloss:~w', [Ref]).

% library stuff
cmd(libdoc({Name}, {Summary}),
    [HTML, #label(SecLabel, [], Tag)]) :-
    atom_concat('sec:', Name, SecLabel),
    filebase(Name, File),
    format(atom(Label), '~w:', [library(Name)]),
    translate_section(2, -,
                      [Label, Summary],
                      HTML,
                      File),
    tex:label_tag(SecLabel, Tag).
cmd(libsummary({Name}),
    [HTML, #label(SecLabel, [], Tag)]) :-
    path_minus(Name, RefName),
    atom_concat('sec:summary-lib-', RefName, SecLabel),
    format(atom(Label), '~w', [library(Name)]),
    translate_section(3, -, [Label], HTML),
    tex:label_tag(SecLabel, Tag).

%!  path_minus(+Path, -Minus) is det.
%
%   Replace / in paths  with  -.  /   is  not  allowed  in SGML NAME
%   attributes.

path_minus(Path, Minus) :-
    sub_atom(Path, _, _, _, /),
    !,
    atomic_list_concat(Segments, /, Path),
    atomic_list_concat(Segments, -, Minus).
path_minus(Path, Path).

filebase(Name, File) :-
    atom_codes(Name, Codes),
    select_csym(Codes, Alnums),
    atom_codes(File, Alnums).

select_csym([], []).
select_csym([H|T0], [H|T]) :-
    code_type(H, csymf),
    !,
    select_csym(T0, T).
select_csym([_|T0], T) :-
    select_csym(T0, T).


pred_tag([], L, L).
pred_tag([Value], [#predtag(#embrace("[]", +Value))|L], L).

pred_class([Opt], Class) :-
    sub_term(Tag, Opt),
    atom(Tag),
    sub_atom(Tag, _, _, _, multifile),
    !,
    Class = multidef.
pred_class(_, pubdef).


                 /*******************************
                 *           GLOSSARY           *
                 *******************************/

canonicalise_glossitem(In, Out) :-
    downcase_atom(In, In1),
    atom_codes(In1, Chars0),
    (   append(CharsPre, [0'[|_], Chars0)
    ->  remove_trailing_spaces(CharsPre, Chars1)
    ;   Chars1 = Chars0
    ),
    (   append(Chars2, [0's], Chars1)
    ->  true
    ;   Chars2 = Chars1
    ),
    maplist(canonical_char, Chars2, Chars),
    atom_codes(Out0, Chars),
    canonical(Out0, Out).

canonical(unified, unify) :- !.
canonical(bound, binding) :- !.
canonical(proven, prove) :- !.
canonical(succeeded, succeed) :- !.
canonical(compiled, compile) :- !.
canonical(propertie, property) :- !.    % s has alredy gone
canonical(X, X).

canonical_char(0' , 0'-) :- !.
canonical_char(0'_, 0'-) :- !.
canonical_char(X, X).

remove_trailing_spaces([], []).
remove_trailing_spaces([0' |T], []) :-
    maplist(=(0' ), T),
    !.          % '
remove_trailing_spaces([H|T0], [H|T]) :-
    remove_trailing_spaces(T0, T).


                 /*******************************
                 *         PlDoc KEYWORDS       *
                 *******************************/

cmd(tag({Tag}),
    [ html('<dt class="tag">'), +Tag, html('<dd>') ]).
cmd(mtag({Tag}),
    [ html('<dt class="mtag">'), +Tag, html('<dd>') ]).

cmd(param({Param}, {Description}),
    [ html('<tr>'),
      html('<td class="param">'), +Param, html('</td>'),
      html('<td class="argdescr"> -'), +Description, html('<td>'),
      html('</tr>')
    ]).
cmd(arg({Param}, {Description}),
    [ html('<tr>'),
      html('<td class="param">'), +Param, html('</td>'),
      html('<td class="argdescr"> -'), +Description, html('<td>'),
      html('</tr>')
    ]).


                 /*******************************
                 *               C              *
                 *******************************/

cmd(backslash, #code(\)).
cmd(bsl, #code(\)).
cmd(Cmd, HTML) :-
    urldef(Cmd, Atom),
    !,
    HTML = #code(Atom).


                 /*******************************
                 *    LATEX SPECIAL SEQUENCES   *
                 *******************************/

%       NOTE: This code is copied from doc_latex.pl from PlDoc.

%!  urldef(?DefName, ?String)
%
%   True if \DefName is  a  urldef   for  String.  UrlDefs are LaTeX
%   sequences that can be used to  represent strings with symbols in
%   fragile environments. Whenever a word can   be  expressed with a
%   urldef, we will  do  this  to   enhance  the  robustness  of the
%   generated LaTeX code.

:- dynamic
    urldef/2,
    urldefs_loaded/1.

%!  load_urldefs.
%!  load_urldefs(+File)
%
%   Load   =|\urldef|=   definitions   from    File   and   populate
%   urldef_name/2. See =|pldoc.sty|= for details.

load_urldefs :-
    urldefs_loaded(_),
    !.
load_urldefs :-
    absolute_file_name(library('pldoc/pldoc.sty'), File,
                       [ access(read) ]),
    load_urldefs(File).

load_urldefs(File) :-
    urldefs_loaded(File),
    !.
load_urldefs(File) :-
    open(File, read, In),
    call_cleanup((   read_line_to_codes(In, L0),
                     process_urldefs(L0, In)),
                 close(In)),
    assert(urldefs_loaded(File)).

process_urldefs(end_of_file, _) :- !.
process_urldefs(Line, In) :-
    (   phrase(urldef(Name, String), Line)
    ->  assert(urldef(Name, String))
    ;   true
    ),
    read_line_to_codes(In, L2),
    process_urldefs(L2, In).

urldef(Name, String) -->
    "\\urldef{\\", string(NameS), "}\\satom{", string(StringS), "}",
    ws,
    (   "%"
    ->  string(_)
    ;   []
    ),
    eol,
    !,
    { atom_codes(Name, NameS),
      atom_codes(String, StringS)
    }.

ws --> [C], { C =< 32 }, !, ws.
ws --> [].

string([]) --> [].
string([H|T]) --> [H], string(T).

eol([],[]).


clean_name([\Special], Out) :-
    urldef(Special, Out),
    !.
clean_name([\tt, Out], Out) :- !.
clean_name([' '|T], Out) :-
    !,
    clean_name(T, Out).
clean_name($(Out), Out) :- !.
clean_name([Out], Out) :- !.
clean_name(\($), $) :- !.
clean_name(\Special, Out) :-
    urldef(Special, Out),
    !.
clean_name(X, X) :-
    atomic(X),
    !.
clean_name(L, Out) :-
    maplist(clean_name, L, L2),
    delete(L2, [], L3),
    atomic_list_concat(L3, Out).

nospace_op(:).

%!  predicate_refname(+Name, +Arity, -Ref) is det.
%!  dcg_refname(+Name, +Arity, -Ref) is det.
%
%   Reference name for predicates.

predicate_refname(Name, Arity, Ref) :-
    pred_refname(Name, Arity, Ref, /).
dcg_refname(Name, Arity, Ref) :-
    pred_refname(Name, Arity, Ref, //).

pred_refname(Module:Name, Arity, Ref, Type) :-
    !,
    format(atom(Ref), '~w:~w~w~w', [Module, Name, Type, Arity]).
pred_refname(Symbol, Arity, Ref, Type) :-
    symbol_name(Symbol, Name),
    !,
    format(atom(Ref), '~w~w~w', [Name, Type, Arity]).
pred_refname(Name, Arity, Ref, Type) :-
    format(atom(Ref), '~w~w~w', [Name, Type, Arity]).

%!  function_refname(+Name, +Arity, -Ref)
%
%   Reference name for arithmetic functions.

function_refname(Symbol, Arity, Ref) :-
    symbol_name(Symbol, Name),
    !,
    format(string(Ref), 'f-~w/~w', [Name, Arity]).
function_refname(Name, Arity, Ref) :-
    format(string(Ref), 'f-~w/~w', [Name, Arity]).

%!  cfunction_refname(+Name, +Arity, -Ref)
%
%   Reference name for C API functions.

cfunction_refname(Op, Ref) :-
    sub_atom(Op, B, _, A, '::operator'),
    !,
    sub_atom(Op, 0, B, _, Prefix),
    sub_atom(Op, _, A, 0, Postfix0),
    normalize_space(atom(Postfix), Postfix0),
    (   symbol_name(Postfix, Name)
    ->  true
    ;   Name = Postfix
    ),
    format(atom(Ref), '~w~w()', [Prefix, Name]).
cfunction_refname(Name, Ref) :-
    format(atom(Ref), '~w()', [Name]).

:- if(false).
symbol_name('->', send_arrow).
symbol_name('<-', get_arrow).
:- else.
symbol_name(_,_) :- fail.
:- endif.

n_list(0, _, []) :- !.
n_list(N, X, [X|T]) :-
    N > 0,
    N2 is N - 1,
    n_list(N2, X, T).

:- load_urldefs.
