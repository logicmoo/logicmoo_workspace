/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2014, VU University Amsterdam
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

:- module(term_html,
          [ term//2                             % +Term, +Options
          ]).
:- use_module(library(http/html_write)).
:- use_module(library(option)).
:- use_module(library(error)).
:- use_module(library(debug)).

:- multifile
    blob_rendering//3.              % +Type, +Blob, +Options

/** <module> Represent Prolog terms as HTML

This file is primarily designed to   support running Prolog applications
over the web. It provides a   replacement for write_term/2 which renders
terms as structured HTML.
*/

%!  term(@Term, +Options)// is det.
%
%   Render a Prolog term as  a   structured  HTML  tree. Options are
%   passed to write_term/3. In addition,   the following options are
%   processed:
%
%     - format(+Format)
%     Used for atomic values.  Typically this is used to
%     render a single value.
%     - float_format(+Format)
%     If a float is rendered, it is rendered using
%     `format(string(S), Format, [Float])`
%
%   @tbd    Cyclic terms.
%   @tbd    Attributed terms.
%   @tbd    Portray
%   @tbd    Test with Ulrich's write test set.
%   @tbd    Deal with numbervars and canonical.

term(Term, Options) -->
    { must_be(acyclic, Term),
      merge_options(Options,
                    [ priority(1200),
                      max_depth(1 000 000 000),
                      depth(0)
                    ],
                    Options1),
      dict_create(Dict, _, Options1)
    },
    any(Term, Dict).


any(_, Options) -->
    { Options.depth >= Options.max_depth },
    !,
    html(span(class('pl-ellipsis'), ...)).
any(Term, Options) -->
    { primitive(Term, Class0),
      !,
      quote_atomic(Term, S, Options),
      primitive_class(Class0, Term, S, Class)
    },
    html(span(class(Class), S)).
any(Term, Options) -->
    { blob(Term,Type), Term \== [] },
    !,
    (   blob_rendering(Type,Term,Options)
    ->  []
    ;   html(span(class('pl-blob'),['<',Type,'>']))
    ).
any(Term, Options) -->
    { is_dict(Term), !
    },
    dict(Term, Options).
any(Term, Options) -->
    { assertion((compound(Term);Term==[]))
    },
    compound(Term, Options).

%!  compound(+Compound, +Options)// is det.
%
%   Process a compound term.

compound('$VAR'(Var), Options) -->
    { Options.get(numbervars) == true,
      !,
      format(string(S), '~W', ['$VAR'(Var), [numbervars(true)]]),
      (   S == "_"
      ->  Class = 'pl-anon'
      ;   Class = 'pl-var'
      )
    },
    html(span(class(Class), S)).
compound(List, Options) -->
    { (   List == []
      ;   List = [_|_]                              % May have unbound tail
      ),
      !,
      arg_options(Options, _{priority:999}, ArgOptions)
    },
    list(List, ArgOptions).
compound({X}, Options) -->
    !,
    { arg_options(Options, _{priority:1200}, ArgOptions) },
    html(span(class('pl-curl'), [ '{', \any(X, ArgOptions), '}' ])).
compound(OpTerm, Options) -->
    { compound_name_arity(OpTerm, Name, 1),
      is_op1(Name, Type, Pri, ArgPri, Options),
      \+ Options.get(ignore_ops) == true
    },
    !,
    op1(Type, Pri, OpTerm, ArgPri, Options).
compound(OpTerm, Options) -->
    { compound_name_arity(OpTerm, Name, 2),
      is_op2(Name, LeftPri, Pri, RightPri, Options),
      \+ Options.get(ignore_ops) == true
    },
    !,
    op2(Pri, OpTerm, LeftPri, RightPri, Options).
compound(Compound, Options) -->
    { compound_name_arity(Compound, Name, Arity),
      quote_atomic(Name, S, Options.put(embrace, never)),
      arg_options(Options, _{priority:999}, ArgOptions),
      extra_classes(Classes, Options)
    },
    html(span(class(['pl-compound'|Classes]),
              [ span(class('pl-functor'), S),
                '(',
                \args(0, Arity, Compound, ArgOptions),
                ')'
              ])).

extra_classes(['pl-level-0'], Options) :-
    Options.depth == 0,
    !.
extra_classes([], _).

%!  arg_options(+Options, -OptionsOut) is det.
%!  arg_options(+Options, +Extra, -OptionsOut) is det.
%
%   Increment depth in Options.

arg_options(Options, Options.put(depth, NewDepth)) :-
    NewDepth is Options.depth+1.
arg_options(Options, Extra, Options.put(depth, NewDepth).put(Extra)) :-
    NewDepth is Options.depth+1.

%!  args(+Arg0, +Arity, +Compound, +Options)//
%
%   Emit arguments of a compound term.

args(Arity, Arity, _, _) --> !.
args(I, Arity, Compound, ArgOptions) -->
    { NI is I + 1,
      arg(NI, Compound, Arg)
    },
    any(Arg, ArgOptions),
    (   {NI == Arity}
    ->  []
    ;   html(', '),
        args(NI, Arity, Compound, ArgOptions)
    ).

%!  list(+List, +Options)//
%
%   Emit a list.  The List may have an unbound tail.

list(List, Options) -->
    html(span(class('pl-list'),
              ['[', \list_content(List, Options),
               ']'
              ])).

list_content([], _Options) -->
    !,
    [].
list_content([H|T], Options) -->
    !,
    { arg_options(Options, ArgOptions)
    },
    any(H, Options),
    (   {T == []}
    ->  []
    ;   { Options.depth + 1 >= Options.max_depth }
    ->  html(['|',span(class('pl-ellipsis'), ...)])
    ;   {var(T) ; \+ T = [_|_]}
    ->  html('|'),
        tail(T, ArgOptions)
    ;   html(', '),
        list_content(T, ArgOptions)
    ).

tail(Value, Options) -->
    {   var(Value)
    ->  Class = 'pl-var-tail'
    ;   Class = 'pl-nonvar-tail'
    },
    html(span(class(Class), \any(Value, Options))).

%!  is_op1(+Name, -Type, -Priority, -ArgPriority, +Options) is semidet.
%
%   True if Name is an operator taking one argument of Type.

is_op1(Name, Type, Pri, ArgPri, Options) :-
    operator_module(Module, Options),
    current_op(Pri, OpType, Module:Name),
    argpri(OpType, Type, Pri, ArgPri),
    !.

argpri(fx, prefix,  Pri0, Pri) :- Pri is Pri0 - 1.
argpri(fy, prefix,  Pri,  Pri).
argpri(xf, postfix, Pri0, Pri) :- Pri is Pri0 - 1.
argpri(yf, postfix, Pri,  Pri).

%!  is_op2(+Name, -LeftPri, -Pri, -RightPri, +Options) is semidet.
%
%   True if Name is an operator taking two arguments of Type.

is_op2(Name, LeftPri, Pri, RightPri, Options) :-
    operator_module(Module, Options),
    current_op(Pri, Type, Module:Name),
    infix_argpri(Type, LeftPri, Pri, RightPri),
    !.

infix_argpri(xfx, ArgPri, Pri, ArgPri) :- ArgPri is Pri - 1.
infix_argpri(yfx, Pri, Pri, ArgPri) :- ArgPri is Pri - 1.
infix_argpri(xfy, ArgPri, Pri, Pri) :- ArgPri is Pri - 1.

%!  operator_module(-Module, +Options) is det.
%
%   Find the module for evaluating operators.

operator_module(Module, Options) :-
    Module = Options.get(module),
    !.
operator_module(TypeIn, _) :-
    '$module'(TypeIn, TypeIn).

%!  op1(+Type, +Pri, +Term, +ArgPri, +Options)// is det.

op1(Type, Pri, Term, ArgPri, Options) -->
    { Pri > Options.priority },
    !,
    html(['(', \op1(Type, Term, ArgPri, Options), ')']).
op1(Type, _, Term, ArgPri, Options) -->
    op1(Type, Term, ArgPri, Options).

op1(prefix, Term, ArgPri, Options) -->
    { Term =.. [Functor,Arg],
      arg_options(Options, DepthOptions),
      FuncOptions = DepthOptions.put(embrace, never),
      ArgOptions  = DepthOptions.put(priority, ArgPri),
      quote_atomic(Functor, S, FuncOptions),
      extra_classes(Classes, Options)
    },
    html(span(class(['pl-compound'|Classes]),
              [ span(class('pl-prefix'), S),
                \space(Functor, Arg, FuncOptions, ArgOptions),
                \any(Arg, ArgOptions)
              ])).
op1(postfix, Term, ArgPri, Options) -->
    { Term =.. [Functor,Arg],
      arg_options(Options, DepthOptions),
      ArgOptions = DepthOptions.put(priority, ArgPri),
      FuncOptions = DepthOptions.put(embrace, never),
      quote_atomic(Functor, S, FuncOptions),
      extra_classes(Classes, Options)
    },
    html(span(class(['pl-compound'|Classes]),
              [ \any(Arg, ArgOptions),
                \space(Arg, Functor, ArgOptions, FuncOptions),
                span(class('pl-postfix'), S)
              ])).

%!  op2(+Pri, +Term, +LeftPri, +RightPri, +Options)// is det.

op2(Pri, Term, LeftPri, RightPri, Options) -->
    { Pri > Options.priority },
    !,
    html(['(', \op2(Term, LeftPri, RightPri, Options), ')']).
op2(_, Term, LeftPri, RightPri, Options) -->
    op2(Term, LeftPri, RightPri, Options).

op2(Term, LeftPri, RightPri, Options) -->
    { Term =.. [Functor,Left,Right],
      arg_options(Options, DepthOptions),
      LeftOptions  = DepthOptions.put(priority, LeftPri),
      FuncOptions  = DepthOptions.put(embrace, never),
      RightOptions = DepthOptions.put(priority, RightPri),
      (   (   need_space(Left, Functor, LeftOptions, FuncOptions)
          ;   need_space(Functor, Right, FuncOptions, RightOptions)
          )
      ->  Space = ' '
      ;   Space = ''
      ),
      quote_op(Functor, S, Options),
      extra_classes(Classes, Options)
    },
    html(span(class(['pl-compound'|Classes]),
              [ \any(Left, LeftOptions),
                Space,
                span(class('pl-infix'), S),
                Space,
                \any(Right, RightOptions)
              ])).

%!  space(@T1, @T2, +Options)//
%
%   Emit a space if omitting a space   between T1 and T2 would cause
%   the two terms to join.

space(T1, T2, LeftOptions, RightOptions) -->
    { need_space(T1, T2, LeftOptions, RightOptions) },
    html(' ').
space(_, _, _, _) -->
    [].

need_space(T1, T2, _, _) :-
    (   is_solo(T1)
    ;   is_solo(T2)
    ),
    !,
    fail.
need_space(T1, T2, LeftOptions, RightOptions) :-
    end_code_type(T1, TypeR, LeftOptions.put(side, right)),
    end_code_type(T2, TypeL, RightOptions.put(side, left)),
    \+ no_space(TypeR, TypeL).

no_space(punct, _).
no_space(_, punct).
no_space(quote(R), quote(L)) :-
    !,
    R \== L.
no_space(alnum, symbol).
no_space(symbol, alnum).

%!  end_code_type(+Term, -Code, Options)
%
%   True when code is the first/last character code that is emitted
%   by printing Term using Options.

end_code_type(_, Type, Options) :-
    Options.depth >= Options.max_depth,
    !,
    Type = symbol.
end_code_type(Term, Type, Options) :-
    primitive(Term, _),
    !,
    quote_atomic(Term, S, Options),
    end_type(S, Type, Options).
end_code_type(Dict, Type, Options) :-
    is_dict(Dict, Tag),
    !,
    (   Options.side == left
    ->  end_code_type(Tag, Type, Options)
    ;   Type = punct
    ).
end_code_type('$VAR'(Var), Type, Options) :-
    Options.get(numbervars) == true,
    !,
    format(string(S), '~W', ['$VAR'(Var), [numbervars(true)]]),
    end_type(S, Type, Options).
end_code_type(List, Type, _) :-
    (   List == []
    ;   List = [_|_]
    ),
    !,
    Type = punct.
end_code_type(OpTerm, Type, Options) :-
    compound_name_arity(OpTerm, Name, 1),
    is_op1(Name, OpType, Pri, ArgPri, Options),
    \+ Options.get(ignore_ops) == true,
    !,
    (   Pri > Options.priority
    ->  Type = punct
    ;   (   OpType == prefix
        ->  end_code_type(Name, Type, Options)
        ;   arg(1, OpTerm, Arg),
            arg_options(Options, ArgOptions),
            end_code_type(Arg, Type, ArgOptions.put(priority, ArgPri))
        )
    ).
end_code_type(OpTerm, Type, Options) :-
    compound_name_arity(OpTerm, Name, 2),
    is_op2(Name, LeftPri, Pri, _RightPri, Options),
    \+ Options.get(ignore_ops) == true,
    !,
    (   Pri > Options.priority
    ->  Type = punct
    ;   arg(1, OpTerm, Arg),
        arg_options(Options, ArgOptions),
        end_code_type(Arg, Type, ArgOptions.put(priority, LeftPri))
    ).
end_code_type(Compound, Type, Options) :-
    compound_name_arity(Compound, Name, _),
    end_code_type(Name, Type, Options).

end_type(S, Type, Options) :-
    number(S),
    !,
    (   (S < 0 ; S == -0.0),
        Options.side == left
    ->  Type = symbol
    ;   Type = alnum
    ).
end_type(S, Type, Options) :-
    Options.side == left,
    !,
    sub_string(S, 0, 1, _, Start),
    syntax_type(Start, Type).
end_type(S, Type, _) :-
    sub_string(S, _, 1, 0, End),
    syntax_type(End, Type).

syntax_type("\"", quote(double)) :- !.
syntax_type("\'", quote(single)) :- !.
syntax_type("\`", quote(back))   :- !.
syntax_type(S, Type) :-
    string_code(1, S, C),
    (   code_type(C, prolog_identifier_continue)
    ->  Type = alnum
    ;   code_type(C, prolog_symbol)
    ->  Type = symbol
    ;   code_type(C, space)
    ->  Type = layout
    ;   Type = punct
    ).


%!  dict(+Term, +Options)//

dict(Term, Options) -->
    { dict_pairs(Term, Tag, Pairs),
      quote_atomic(Tag, S, Options.put(embrace, never)),
      arg_options(Options, ArgOptions)
    },
    html(span(class('pl-dict'),
              [ span(class('pl-tag'), S),
                '{',
                \dict_kvs(Pairs, ArgOptions),
                '}'
              ])).

dict_kvs([], _) --> [].
dict_kvs(_, Options) -->
    { Options.depth >= Options.max_depth },
    !,
    html(span(class('pl-ellipsis'), ...)).
dict_kvs(KVs, Options) -->
    dict_kvs2(KVs, Options).

dict_kvs2([K-V|T], Options) -->
    { quote_atomic(K, S, Options),
      end_code_type(V, VType, Options.put(side, left)),
      (   VType == symbol
      ->  VSpace = ' '
      ;   VSpace = ''
      ),
      arg_options(Options, ArgOptions)
    },
    html([ span(class('pl-key'), S),
           ':',                             % FIXME: spacing
           VSpace,
           \any(V, ArgOptions)
         ]),
    (   {T==[]}
    ->  []
    ;   html(', '),
        dict_kvs2(T, Options)
    ).

quote_atomic(Float, String, Options) :-
    float(Float),
    Format = Options.get(float_format),
    !,
    format(string(String), Format, [Float]).
quote_atomic(Plain, String, Options) :-
    atomic(Plain),
    Format = Options.get(format),
    !,
    format(string(String), Format, [Plain]).
quote_atomic(Plain, String, Options) :-
    rational(Plain),
    \+ integer(Plain),
    !,
    operator_module(Module, Options),
    format(string(String), '~W', [Plain, [module(Module)]]).
quote_atomic(Plain, Plain, _) :-
    number(Plain),
    !.
quote_atomic(Plain, String, Options) :-
    Options.get(quoted) == true,
    !,
    (   Options.get(embrace) == never
    ->  format(string(String), '~q', [Plain])
    ;   format(string(String), '~W', [Plain, Options])
    ).
quote_atomic(Var, String, Options) :-
    var(Var),
    !,
    format(string(String), '~W', [Var, Options]).
quote_atomic(Plain, Plain, _).

quote_op(Op, S, _Options) :-
    is_solo(Op),
    !,
    S = Op.
quote_op(Op, S, Options) :-
    quote_atomic(Op, S, Options.put(embrace,never)).

is_solo(Var) :-
    var(Var), !, fail.
is_solo(',').
is_solo(';').
is_solo('!').

%!  primitive(+Term, -Class) is semidet.
%
%   True if Term is a primitive term, rendered using the CSS
%   class Class.

primitive(Term, Type) :- var(Term),      !, Type = 'pl-avar'.
primitive(Term, Type) :- atom(Term),     !, Type = 'pl-atom'.
primitive(Term, Type) :- string(Term),   !, Type = 'pl-string'.
primitive(Term, Type) :- integer(Term),  !, Type = 'pl-int'.
primitive(Term, Type) :- rational(Term), !, Type = 'pl-rational'.
primitive(Term, Type) :- float(Term),    !, Type = 'pl-float'.

%!  primitive_class(+Class0, +Value, -String, -Class) is det.
%
%   Fixup the CSS class for lexical variations.  Used to find
%   quoted atoms.

primitive_class('pl-atom', Atom, String, Class) :-
    \+ atom_string(Atom, String),
    !,
    Class = 'pl-quoted-atom'.
primitive_class(Class, _, _, Class).


                 /*******************************
                 *             HOOKS            *
                 *******************************/

%!  blob_rendering(+BlobType, +Blob, +WriteOptions)// is semidet.
%
%   Hook to render blob atoms as HTML.  This hook is called whenever
%   a blob atom is encountered while   rendering  a compound term as
%   HTML. The blob type is  provided   to  allow  efficient indexing
%   without having to examine the blob. If this predicate fails, the
%   blob is rendered as an HTML SPAN with class 'pl-blob' containing
%   BlobType as text.
