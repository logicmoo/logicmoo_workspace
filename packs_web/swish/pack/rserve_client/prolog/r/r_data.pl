/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2017, VU University Amsterdam
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

:- module(r_data,
	  [ r_data_frame/3,		% +Rvar, +Columns, :Goal
	    r_data_frame_from_rows/2,	% +RVar, +Rows
	    r_data_frame_from_dicts/2,	% +DataFrame, +Rows

	    r_data_frame_to_dicts/2,	% +Rvar, -Dicts
	    r_data_frame_to_rows/3,	% +RVar, +Functor, -Rows

	    r_data_frame_colnames/2,	% +RVars, -ColNames
	    r_data_frame_rownames/2	% +RVars, -RowNames
	  ]).
:- use_module(r_call).
:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(pairs)).
:- use_module(library(lists)).

:- meta_predicate
	r_data_frame(+, +, 0).

/** <module> R data frame handling

This library provides predicates  for  creating   and  fetching  R  data
frames. R data frames are typically  2-dimensional arrays where the data
is organised in _columns_. In  Prolog,   data  is typically organised in
_rows_ (or _records_).
*/

%%	r_data_frame(+Rvar, +Columns, :Goal) is det.
%
%	Create an R data.frame from the solutions of Goal. The resulting
%	data frame is bound to the R variable Rvar.  For example:
%
%	```
%	?- r_data_frame(movieyear,
%		        [movie=Name, year=Year],
%			movie(Name, Year)).
%	```
%
%	@arg	Rvar is the name of the R output variable
%	@arg	Columns is a list Name=Var

r_data_frame(RVar, ColSpec, Goal) :-
	must_be(atom, RVar),
	maplist(arg(1), ColSpec, Names),
	maplist(arg(2), ColSpec, Vars),
	Templ =.. [v|Vars],
	findall(Templ, Goal, Rows),
	r_data_frame_from_rows(RVar, Rows),
	colnames(RVar) <- Names.

%%	r_data_frame_to_dicts(+DataFrame, -Dicts) is det.
%
%	Translate a DataFrame into a  list   of  dicts,  where each dict
%	represents a _row_. The  keys  of   the  dicts  are fetched from
%	`colnames(DataFrame)`.  For example:
%
%	```
%	?- r_data_frame_to_dicts(mtcars, Dicts).
%	Dicts = [ row{am:1, carb:4, cyl:6, disp:160.0, drat:3.9,
%		      gear:4, hp:110, mpg:21.0, qsec:16.46, vs:0,
%		      wt:2.62},
%		  ...
%		]
%	```

r_data_frame_to_dicts(DataFrame, Dicts) :-
	Cols <- DataFrame,
	ColNameStrings <- colnames(DataFrame),
	maplist(atom_string, ColNames, ColNameStrings),
	pairs_keys_values(Pairs, ColNames, _),
	dict_pairs(Templ, _, Pairs),
	maplist(dict_cols(Templ, Dicts), ColNames, Cols).

dict_cols(Templ, Dicts, Name, Col) :-
	maplist(fill_col(Templ, Name), Col, Dicts).

fill_col(_, Name, Value, Dict) :-
	nonvar(Dict), !,
	get_dict(Name, Dict, Value).
fill_col(Templ, Name, Value, Dict) :-
	copy_term(Templ, Dict),
	get_dict(Name, Dict, Value).

%%	r_data_frame_to_rows(+DataFrame, +Functor, -Rows) is det.
%
%	Translate a 2-dimensional R dataframe into   a  list of compound
%	terms, each representing a  row.  The   functor  of  each row is
%	Functor.  For example:
%
%	```
%	?- r_data_frame_to_rows(mtcars, car, Rows).
%	Rows = [ car(21.0, 6, 160.0, 110, 3.9, 2.62, 16.46, 0, 1, 4, 4),
%	         ...
%	       ].
%	```

r_data_frame_to_rows(DataFrame, Functor, Rows) :-
	Cols <- DataFrame,
	length(Cols, Arity),
	term_cols(Cols, 1, Arity, Functor, Rows).

term_cols([], _, _, _, _).
term_cols([Col|Cols], I, Arity, Functor, Rows) :-
	maplist(term_col(I, Arity, Functor), Col, Rows),
	I2 is I+1,
	term_cols(Cols, I2, Arity, Functor, Rows).

term_col(1, Arity, Functor, Value, Term) :- !,
	functor(Term, Functor, Arity),
	arg(1, Term, Value).
term_col(I, _, _, Value, Term) :-
	arg(I, Term, Value).

%%	r_data_frame_from_dicts(+DataFrame, +Rows) is det.
%
%	Assign the R variable DataFrame the content   of Rows. Rows is a
%	list of dicts that must all have the  same set of keys. The keys
%	are used as column names.
%
%	@see dicts_to_same_keys/3 to align the set of keys for each dict

r_data_frame_from_dicts(DataFrame, Rows) :-
	must_be(atom, DataFrame),
	must_be(list, Rows),
	Rows = [Row1|_],
	dict_keys(Row1, Keys),
	dict_col_data(Keys, Rows, ColData),
	compound_name_arguments(Term, 'data.frame', ColData),
	DataFrame <- Term,
	colnames(DataFrame) <- Keys.

dict_col_data([], _, []).
dict_col_data([K|Keys], Rows, [ColI|ColR]) :-
	maplist(get_dict(K), Rows, ColI),
	dict_col_data(Keys, Rows, ColR).

%%	r_data_frame_from_rows(+DataFrame, +Rows) is det.
%
%	Assign the R variable DataFrame the content   of Rows. Rows is a
%	list of compound terms.

r_data_frame_from_rows(DataFrame, Rows) :-
	must_be(atom, DataFrame),
	must_be(list, Rows),
	Rows = [Row1|_],
	functor(Row1, _, NCols),
	col_data(1, NCols, Rows, ColData),
	append(ColData, [stringsAsFactors = 'FALSE'], ColDataOpts),
	compound_name_arguments(Term, 'data.frame', ColDataOpts),
	DataFrame <- Term.

col_data(I, NCols, Rows, [ColI|ColR]) :-
	I =< NCols, !,
	maplist(arg(I), Rows, ColI),
	I2 is I + 1,
	col_data(I2, NCols, Rows, ColR).
col_data(_, _, _, []).

%%	r_data_frame_colnames(+DataFrame, -ColNames:list(atom)) is det.
%
%	ColNames are the column names for DataFrame as a list of atoms.

r_data_frame_colnames(DataFrame, ColNames) :-
	ColNameStrings <- colnames(DataFrame),
	maplist(atom_string, ColNames, ColNameStrings).

%%	r_data_frame_rownames(+DataFrame, -RowNames:list(atom)) is det.
%
%	RowNames are the row names for DataFrame as a list of atoms.

r_data_frame_rownames(DataFrame, RowNames) :-
	RowNameStrings <- rownames(DataFrame),
	maplist(atom_string, RowNames, RowNameStrings).
