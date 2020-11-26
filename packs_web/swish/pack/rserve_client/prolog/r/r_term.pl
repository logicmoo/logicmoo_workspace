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

:- module(r_term,
	  [ r_expression//2,		% +Expression, -Assignments

	    op(400, yfx, $),
	    op(100, yf,  [])
	  ]).
:- use_module(r_grammar).
:- use_module(r_expand_dot).
:- use_module(library(error)).
:- use_module(library(dcg/basics)).

/** <module> Translate a Prolog term into an R expression

This module deals with representing an R   expression  as a Prolog term.
The non-terminal r_expression//2  translates  the   Prolog  term  into a
string that can be sent to R.

The            design            is              inspired             by
[real](http://stoics.org.uk/~nicos/sware/real/) from Nicos Angelopoulos.
*/

%%	r_expression(+Term, -Assignments)//
%
%	Grammar that creates an R  command   from  a  Prolog term. Terms
%	recognised:
%
%	  - The atoms `true` and `false` are mapped to TRUE and FALSE.
%	  - Other Prolog *atoms* are mapped to an R _name_. If required,
%	    the name is quoted using backquotes.
%	  - A term +(Atom) is mapped to an R string.
%	  - A Prolog *string* is mapped to an R string. The server
%	    should run in UTF-8 mode for exchange of Unicode data.
%	  - A Prolog *number* is mapped to an R number.
%	  - A Prolog *list* is added to Assignments.  These are used
%	    to create a temporary R variable. The R command translation
%	    contains the variable name
%	  - =|Left$Right|= is translated as is.
%	  - An array index =|X[I,...]|= is translated as is.  Empty
%	    elements in the index, e.g., the R expression =|a[,3]|=
%	    can be written as `a['',3]`, `a[-,3]` or `a[*,3]`.
%	  - Known operators are passed as infix operators.  The
%	    following operators are known: =|+, -, *, /, mod, '%%', ^,
%	    >=, >, ==, <, <=, =<, \=, '!=', :, <-|=
%	  - `Expr1,Expr2` is translated into two R statements separated
%	    by a newline.
%	  - Compound terms are translated to function calls.
%
%	This library loads r_expand_dot.pl,  which   uses  the `.` infix
%	operator to make =|a.b|= and =|a.b()|= valid syntax.
%
%	@arg Assignments is a list Name=Value for data assignments.

r_expression(Term, Assignments) -->
	{ Ctx = r{v:v{tmpvar:0, assignments:[]}, priority:999} },
	r_expr(Term, Ctx),
	{ Assignments = Ctx.v.assignments }.

r_expr(Var, _) -->
	{ var(Var), !,
	  instantiation_error(Var)
	}.
r_expr(true, _) --> !, "TRUE".
r_expr(false, _) --> !, "FALSE".
r_expr(Identifier, _) -->
	{ atom(Identifier)
	}, !,
	(   { r_identifier(Identifier) }
	->  atom(Identifier)
	;   { atom_codes(Identifier, Codes) },
	    "`", r_string_codes(Codes, 0'`), "`"
	).
r_expr(String, _) -->
	{ string(String),
	  string_codes(String, Codes)
	}, !,
	"\"", r_string_codes(Codes, 0'"), "\"".
r_expr(+Atom, _) -->
	{ atomic(Atom), !,
	  atom_codes(Atom, Codes)
	},
	"\"", r_string_codes(Codes, 0'"), "\"".
r_expr(Number, _) -->
	{ number(Number) }, !,
	number(Number).
r_expr(List, Ctx) -->
	{ is_list(List), !,
	  assignment(List, Ctx, Var)
	},
	atom(Var).
r_expr(Left$Right, Ctx) --> !,
	r_expr(Left, Ctx), "$", r_expr(Right, Ctx).
r_expr([](Index, Array), Ctx) --> !,
	r_expr(Array, Ctx),
	"[", r_index(Index, Ctx.put(priority, 999)), "]".
r_expr((A,B), Ctx) --> !,
	r_expr(A, Ctx), "\n",
	r_expr(B, Ctx).
r_expr(Compound, Ctx) -->
	{ compound(Compound),
	  compound_name_arguments(Compound, Name, Args),
	  r_identifier(Name), !
	},
	atom(Name), "(", r_arguments(Args, Ctx.put(priority, 999)), ")".
r_expr(Compound, Ctx) -->
	{ compound(Compound),
	  compound_name_arguments(Compound, Name, [Left,Right]),
	  r_infix_op(Name, RName, Pri, Ass), !,
	  lr_pri(Pri, Ass, LPri, RPri)
	},
	(   {  Ctx.priority >= Pri }
	->  r_expr(Left, Ctx.put(priority,LPri)),
	    " ", atom(RName), " ",
	    r_expr(Right, Ctx.put(priority,RPri))
	;   "(",
	    r_expr(Left, Ctx.put(priority,LPri)),
	    " ", atom(RName), " ",
	    r_expr(Right, Ctx.put(priority,RPri)),
	    ")"
	).

r_arguments([], _) --> "".
r_arguments([H|T], Ctx) -->
	r_expr(H, Ctx),
	(   {T==[]}
	->  ""
	;   ", ",
	    r_arguments(T, Ctx)
	).

r_index([], _) --> "".
r_index([H|T], Ctx) -->
	r_index_elem(H, Ctx),
	(   {T==[]}
	->  ""
	;   ",",
	    r_index(T, Ctx)
	).

r_index_elem(Var, _) -->
	{ var(Var),
	  instantiation_error(Var)
	}.
r_index_elem('', _) -->
	!.
r_index_elem(-, _) -->
	!.
r_index_elem(*, _) -->
	!.
r_index_elem(Expr, Ctx) -->
	r_expr(Expr, Ctx).

assignment(Data, Ctx, Var) :-
	Vars = Ctx.v,
	_{tmpvar:I, assignments:A0} :< Vars,
	atom_concat('Rserve.tmp.', I, Var),
	I2 is I + 1,
	b_set_dict(tmpvar, Vars, I2),
	b_set_dict(assignments, Vars, [Var=Data|A0]).

%%	r_string_codes(+Codes, +Esc)//
%
%	Emit an escaped R string.
%	@tbd	Do we need to use escape characters?

r_string_codes([], _) --> [].
r_string_codes([H|T], Esc) --> r_string_code(H, Esc), r_string_codes(T, Esc).

r_string_code(0, _) --> !,
	{ domain_error(r_string_code, 0) }.
r_string_code(C, C) --> !, "\\", [C].
r_string_code(C, _) --> [C].

%%	r_infix_op(Op, Rop, Priority, Associativity)
%
%	True if Op is the Prolog representation for the R operator Rop.  The
%	R gammar doesn't specify the ranking of the operators.  We use Prolog's
%	rules for now.

r_infix_op(+,	 +,    500, yfx).
r_infix_op(-,	 -,    500, yfx).
r_infix_op(*,	 *,    400, yfx).
r_infix_op(/,	 /,    400, yfx).
r_infix_op(mod,  '%%', 400, yfx).
r_infix_op('%%', '%%', 400, yfx).
r_infix_op(^,	 ^,    200, xfy).

r_infix_op(>=,	 >=,   700, xfx).
r_infix_op(>,	 >,    700, xfx).
r_infix_op(==,	 ==,   700, xfx).
r_infix_op(<,	 <,    700, xfx).
r_infix_op(<=,	 <=,   700, xfx).
r_infix_op(=<,	 <=,   700, xfx).
r_infix_op(\=,	 '!=', 700, xfx).
r_infix_op('!=', '!=', 700, xfx).

r_infix_op(:,	 :,    100, xfx).	% range

r_infix_op(<-,	 <-,   900, xfx).
r_infix_op(=,	 =,    900, xfx).

lr_pri(Pri, xfx, APri, APri) :- !, APri is Pri - 1.
lr_pri(Pri, xfy, APri,  Pri) :- !, APri is Pri - 1.
lr_pri(Pri, yfx,  Pri, APri) :- !, APri is Pri - 1.
