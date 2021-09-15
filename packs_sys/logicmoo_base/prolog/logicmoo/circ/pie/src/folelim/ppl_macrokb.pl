%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% Copyright (C) 2016 Christoph Wernhard
%%%%
%%%% This file is part of PIE.
%%%%
%%%% PIE is free software: you can redistribute it and/or modify
%%%% it under the terms of the GNU General Public License as published by
%%%% the Free Software Foundation, either version 3 of the License, or
%%%% (at your option) any later version.
%%%% 
%%%% PIE is distributed in the hope that it will be useful,
%%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%% GNU General Public License for more details.
%%%% 
%%%% You should have received a copy of the GNU General Public License
%%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(ppl_macrokb,
	  [ppl_file/3,
	   ppl_content/4,
	   ppl_printtime/1,
	   ppl_is_at_printtime/0,
	   ppl_debug_info/1,
	   mark_installed_macros/2]).

:- use_module(pplatex(pplatex)).
:- use_module(swilib(fromonto)).
:- use_module(swilib(options)).
:- use_module(swilib(pretty)).
:- use_module(swilib(info)).
:- use_module(swilib(err)).
:- use_module(logop_fol).
:- use_module(macrokb).
:- use_module(support_macrokb).

:- flag(ppl_printtime_context, _, 0).
:- flag(ppl_debug_pos, _, 0).
:- dynamic ppl_debug_source/1.

:- multifile user:ppl_list_predicate/1.

ppl_printtime(_).

ppl_is_at_printtime :-
	flag(ppl_printtime_context, N, N),
	N > 0.

ppl_debug_info(A) :-
	flag(ppl_debug_pos, Pos, Pos),
	( ppl_debug_source(Source) -> true ; Source = unknown ),
	format(atom(A), 'After position ~w in source ~q', [Pos, Source]).

/*
TODO:

- mark free symbols to support special syntax for them
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% LaTeX Output of MacroKB Files
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% ppl_file('/Users/ch/w/provers/folelim/exkb01.pl', [], '/Users/ch/tmp/xo.tex').
%%%% tptp_to_mac('SWB009+3', X), ppl_content(X, [rightcols=3,maxpos=50], '/Users/ch/tmp/oy1.tex').
%%%%
%%%% Options:
%%%%
%%%%    standalone_latex=Boolean
%%%%   
%%%%       true (default) or false - determines whether a standalone document
%%%%       (i.e. with header and footer) is generated.
%%%%
%%%%    document_header=LaTeXCode
%%%%    document_footer=LaTeXCode
%%%%
%%%%       LaTeXCode := Atom | hook(Call)
%%%%
%%%%       If supplied, specifies the LaTeX document header and footer,
%%%%       respectively. The header is the first part of the preamble,
%%%%       starting from \documentclass. The preamble is
%%%%       then followed by a setup section (see pplkb_setup/2) and
%%%%       optional code specified by before_begin_document. The
%%%%       footer is the very end of the document, usually
%%%%       closing with \end_document. If the value is specified as
%%%%       atom, it will be written. If it is specified in the form
%%%%       hook(Call), then Call is called and its output is inserted.
%%%%       Only considered in standalone_latex mode.
%%%%
%%%%    before_begin_document=Call
%%%%
%%%%       If supplied, the output produced by calling Call is
%%%%       in standalone_latex mode inserted into the header immediately
%%%%       before \begin{document}
%%%%
%%%% Options are passed to pplatex. Possibly useful here:
%%%%  tabular control for printing larger formulas, e.g.
%%%%        rightcols=3, maxpos=50
%%%%  camel=true
%%%%  style=brief
%%%%
%%%% Note: macros in formulas are identified as such only after installing
%%%% them (e.g. by mac_install_defs/1). However, the default syntactic
%%%% representation of them is currently not different from non-macros, such
%%%% that there is not much difference, whether they macros are identified as
%%%% such.  (\pplmacro and \ppldefmacro might be used to configure this).
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ppl_file(File, Options, Outfile) :-	
	file_content(File, Content),
	ppl_content(Content, File, Options, Outfile).

ppl_content(Content, Source, Options, Outfile) :-
	from_options(standalone_latex=Standalone, Options, true),
	( Standalone = true ->
	  pplkb_header(Header),
	  pplkb_setup(Setup),
	  pplkb_footer(Footer),
	  from_options(before_begin_document=UserSetup, Options, true),
	  from_options(document_header=Header1, Options, Header),
	  from_options(document_footer=Footer1, Options, Footer)
	; true
	),
	info(10, 'Writing ~w', [Outfile]),
	flag(ppl_printtime_context, _, 1),
	retractall( ppl_debug_source(_) ),
	assert( ppl_debug_source(Source) ),
	catch(onto_file( ( ( Standalone = true ->
			     ( Header1 = hook(HeaderCall) ->
			       call(HeaderCall)
			     ; write(Header1)
			     ),
			     pplkb_setup(Setup),
			     write(Setup),
			     call(UserSetup),
			     pplkb_begin_document(BeginDocument),
			     write(BeginDocument)
			   ; true
			   ),
			   ( member(Item, Content),
			     ppl_content_item(Item, Source,
					      [format=latex|Options]),
			     fail
			   ; true
			   ),
			   ( Standalone = true ->
			     ( Footer1 = hook(FooterCall) ->
			       call(FooterCall)
			     ; write(Footer1)
			     )
			   ; true
			   )
			 ),
			 Outfile ),
	      E,
	      ( flag(ppl_printtime_context, _, 0),
		throw(E)
	      )),
	flag(ppl_printtime_context, _, 0).

ppl_content_item(term(Pos, (L::F), VarNames), Source, Options) :-
	!,
	flag(ppl_debug_pos, _, Pos),
	format('%~n% Statement at position ~w~n%~n', [Pos]),
	ppl_stmt((L::F), Source, [varnames=VarNames|Options]).
ppl_content_item((L::F), Source, Options) :-
	!,
	format('%~n% Statement~n%~n'),
	ppl_stmt((L::F), Source, Options).
ppl_content_item(doc(Pos, Lines), _, _Options) :-
	!,
	flag(ppl_debug_pos, _, Pos),
	format('%~n% Doc at position ~w~n%~n', [Pos]),
	append_doc_lines(Lines, L),
	evaluate_prolog_inserts(L, L1),
	( member(C, L1),
	  put_code(C),
	  fail
	; true
	).
ppl_content_item(term(Pos, Term, VarNames), _, Options) :-
	flag(ppl_debug_pos, _, Pos),
	( ( from_options(show_code=true, Options, false)
	  ; ppl_list_term(Term)
	  ) ->
	  copy_term(Term-VarNames, Term1-VarNames1),
	  numbervars_from_varnames(VarNames1),
	  Verbatim = verbatim,
 	  format('{\\small~n\\begin{~w}~n', [Verbatim]),
 	  pp_clause(Term1),
 	  format('\\end{~w}}~n', [Verbatim])
	; true
	),
	( Term = (:- ppl_printtime(Call)) ->
	  ( call(Call),
	    fail
	  ; true
	  )
	; true
	).

append_doc_lines([L|Ls], L1) :-
	( L = [0' ,0'*,0' |L2] -> true ; L2 = L ),
	append(L2, [0'\n| L3], L1),
	append_doc_lines(Ls, L3).
append_doc_lines([], []).

ppl_list_term((H :- _)) :-
        !,
        functor(H, F, N),
	once(ppl_list_predicate(F/N)).
ppl_list_term(H) :-
	functor(H, F, N),
	once(ppl_list_predicate(F/N)).

file_content(File, TermsAndDocs) :-
	from_file( read_terms(Terms), File ),
	from_file( read_doc_out(Docs), File ),
	append(Docs, Terms, TD1),
	map_add_firstarg(TD1, TD2),
	keysort(TD2, TD3),
	map_val(TD3, TermsAndDocs).

read_terms([term(Pos,Term,VarNames)|Ts]) :-
	read_term(current_input, Term,
		  [variable_names(VarNames), term_position(SPos)]),
	Term \== end_of_file,
	!,
	stream_position_data(char_count, SPos, Pos),
	read_terms(Ts).
read_terms([]).

read_doc_out(Ds) :-
	stream_property(current_input, position(SPos)),
	read_line_to_codes(current_input, L),
	L \= end_of_file,
	!,
	( L = [0'/,0'*,0'*|L1] ->
	  stream_position_data(char_count, SPos, Pos),
	  ( append(L3, [0'*,0'/], L1) ->
	    Ds = [doc(Pos, [L3])|Ds1],
	    read_doc_out(Ds1)
	  ; ( L1 = [] -> Ls2 = Ls ; Ls2 = [L1|Ls] ),
	    Ds = [doc(Pos,Ls2)|Ds1],
	    read_doc_in(Ls, Ds1)
	  )
	; read_doc_out(Ds)
	).
read_doc_out([]).

read_doc_in(Ls, Ds) :-
	read_line_to_codes(current_input, L),
	L \= end_of_file,
	!,
	( sublist([0'*,0'/], L) ->
	  Ls = [],
	  read_doc_out(Ds)
	; Ls = [L|Ls1],
	  read_doc_in(Ls1, Ds)
	).
read_doc_in([], []).

sublist(X, Y) :-
	append(X, _, X1),
	sublist_1(Y, X1),
	!.

sublist_1(X, X) :-
	!.
sublist_1([_|X], Y) :-
	sublist_1(X, Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

codes_latex_argument(C, C1, C2) :-
	cla1(C, 0, C, C1, C2).

cla1([0'{|C], N, CTX, [0'{|C1], C2) :-
	!,
	N1 is N+1,
	cla1(C, N1, CTX, C1, C2).
cla1([0'}|C], 0, _, [], C) :-
	!.
cla1([0'}|C], N, CTX, [0'}|C1], C2) :-
	!,
	N1 is N-1,
	cla1(C, N1, CTX, C1, C2).
cla1([C|C1], N, CTX, [C|C2], C3) :-
	cla1(C1, N, CTX, C2, C3).
cla1([] ,_, CTX, _, _) :-
	atom_codes(A, CTX),
	err('Runaway \\prolog argument in ~w', [A]).

evaluate_prolog_inserts([0'\\,0'p,0'r,0'o,0'l,0'o,0'g,0'{|C], C1) :-
	!,
	codes_latex_argument(C, C2, C3),
	atom_codes(CallA, C2),
	read_term_from_atom(CallA, Call, [variable_names(VarNames)]),
	apply_var_names(VarNames),
	format(atom(Result), '~@', call(Call)),
	atom_codes(Result, C4),
	append(C4, C5, C1),
	evaluate_prolog_inserts(C3, C5).
evaluate_prolog_inserts([C|Cs], [C|Cs1]) :-
	evaluate_prolog_inserts(Cs, Cs1).
evaluate_prolog_inserts([], []).

apply_var_names([N='$VAR'(N)|Xs]) :-
	apply_var_names(Xs).
apply_var_names([]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ppl_stmt(Stmt, Source, Options) :-
	from_options(varnames=Varnames, Options, []),
	canonicalize_def(Stmt, Source, Type, Varnames,
			 _Pat, _PatCode, DispPat,
			 Form, _, ParamMap, _),
	copy_term(t(DispPat,Varnames,ParamMap,Form),
		  t(DispPat1,Varnames1,ParamMap1,Form1)),
	mark_installed_macros(Form1, Form2),
	apply_var_var_mappings(ParamMap1),
	numbervars_from_varnames(Varnames1),
	numbervars_from_param_map(ParamMap1),
	numbervars(DispPat1-Form2, 0, _),
	( Form2 = (E ::- Body) -> true
	; E = Form2
	),
	format('\\pplkbBefore~n'),
	write_index_label(DispPat1, Type, Options),
	write_def_label(DispPat1, Type, Options),
	format('$'),
	pp_form('$defmacro'(DispPat1), Options),
	format('$'),
	%
	% ppl_pretty_type(Type, Type1, _),
	% format('\\pplkbDefType{~w}~n', [Type1]),
	format('\\pplkbBetween~n'),
	format('$'),
	( var(Body) -> Finally='.' ; Finally=',' ),
	pp_form(E, [finally=Finally|Options] ),
	format('$'),
	format('\\pplkbAfter~n'),
	( var(Body) -> true
	; format('\\pplkbBodyBefore~n'),
	  format('$'),
	  ppl_body(Body, Options),
	  format('$'),
	  format('\\pplkbBodyAfter~n')
	).

write_index_label(Pat, _Type, Options) :-
	format(atom(Label), '~@', [write_term(Pat,[numbervars(true)])]),
	convert_to_index_label(Label, Label1),
	format('\\index{~w@$~@$}',
 	       [Label1,	write_form('$defmacro'(Pat), Options)]).

write_def_label(Pat, _Type, Options) :-
	( memberchk(deflabels=true, Options) ->
	  pattern_def_label(Pat, Label),
	  format('\\refstepcounter{def}\\label{~w}%~n', [Label]),
	  ( memberchk(deflabel_fmt=Fmt, Options) -> true
	  ; Fmt = '\\ref{~w}. '
	  ),
	  format(Fmt, [Label])
	; true
	).

clean_atom_as_def_label(A, A1) :-
	atom_chars(A, A2),
	map_carl(A2, A3),
	atom_chars(A1, A3).

map_carl([X|Xs], [X1|Xs1]) :-
	( char_type(X, ascii),
	  char_type(X, alnum) ->
	  X1 = X
	; X1 = ':'
	),
	map_carl(Xs, Xs1).
map_carl([], []).

pattern_def_label(Pat, Label) :-
	( atom(Pat) ->
	  clean_atom_as_def_label(Pat, Label1),
	  format(atom(Label), 'def:~w', [Label1])
	; compound(Pat) ->
	  functor(Pat, F, N),
	  clean_atom_as_def_label(F, Label1),
	  format(atom(Label), 'def:~w:~w', [Label1, N])
	; format(atom(Label1), '~w', [Pat]),
	  clean_atom_as_def_label(Label1, Label2),
	  format(atom(Label), 'def:~w', [Label2])
	).
	
apply_var_var_mappings([]).
apply_var_var_mappings([X-Y|XYs]) :-
	var(X),
	var(Y),
	!,
	X=Y,
	apply_var_var_mappings(XYs).
apply_var_var_mappings([_|XYs]) :-
	apply_var_var_mappings(XYs).

numbervars_from_param_map([]).
numbervars_from_param_map([P-X|PXs]) :-
	var(X),
	atom(P),
	atom_chars(P, [C|Cs]),
	char_type(C1, to_upper(C)),
	!,
	atom_chars(P1, [C1|Cs]),
	X = '$VAR'(P1),
	numbervars_from_param_map(PXs).
numbervars_from_param_map([_|PXs]) :-
	numbervars_from_param_map(PXs).

numbervars_from_varnames([]).
numbervars_from_varnames([N=X|XNs]) :-
	var(X),
	!,
	X = '$VAR'(N),
	numbervars_from_varnames(XNs).
numbervars_from_varnames([_|XNs]) :-
	numbervars_from_varnames(XNs).

mark_installed_macros(T, T) :-
	var(T),
	!.
mark_installed_macros(T, '$macro'(T1)) :-
	mac_has_def(T),
	!,
	mark_installed_macros_subterms(T, T1).
mark_installed_macros(T, T1) :-
	mark_installed_macros_subterms(T, T1).

mark_installed_macros_subterms(T, T) :-
	atomic(T),
	!.
mark_installed_macros_subterms(T, T1) :-
	T =.. [F|Ts],
	map_mark_installed_macros(Ts, Ts1),
	T1 =.. [F|Ts1].

map_mark_installed_macros([X|Xs], [X1|Xs1]) :-
	mark_installed_macros(X, X1),
	map_mark_installed_macros(Xs, Xs1).
map_mark_installed_macros([], []).

ppl_body(X, Options) :-
	format('~n\\begin{array}{l}'),
	ppl_body_1(X, Options),
	format('~n\\end{array}').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ppl_body_1((G, Gs), Options) :-
	!,
	ppl_item(G, Options),
	format(',\\\\~n'),
	ppl_body_1(Gs, Options).
ppl_body_1(G, Options) :-
	ppl_item(G, Options),
	format('.~n').

ppl_item(G, Options) :-
	ppl_pl(G, Options),
	!.
ppl_item(G, _) :-
	functor(G, F, _),
	atom_length(F, 1),
	char_type(F, graph),
	F \= (=),
	!,
	format('\\mathit{[\\ldots unformattable\\ Prolog\\ code]}').
% 	format(atom(G1), '~@', [pp(G, Options)]),
% 	esc_latex(G1, G2),
% 	format('\\mathtt{~w}', [G2]).
%	format('~n\\begin{verbatim}~w\\end{verbatim}~n', [G2]).
ppl_item(G, _) :-
	format(atom(G1), '~w', G),
	esc_latex(G1, G2),
	format('\\mathrm{~w}', [G2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

esc_latex(X, Y) :-
	atom_chars(X, Cs),
	esc_latex_1(Cs, Cs1),
	atom_chars(Y, Cs1).

esc_latex_1([X|Xs], ['\\',X|Xs1]) :-
	latex_esc_char(X),
	!,
	esc_latex_1(Xs, Xs1).
esc_latex_1([X|Xs], Xs1) :-
	latex_special_char(X, Xs1, Xs2),
	!,
	esc_latex_1(Xs, Xs2).
esc_latex_1([X|Xs], [X|Xs1]) :-
	esc_latex_1(Xs, Xs1).
esc_latex_1([], []).

% latex_special_char('\\', ['\\',t,e,x,t,b,a,c,k,s,l,a,s,h|X], X).
latex_special_char('\\', ['\\',b,a,c,k,s,l,a,s,h|X], X).
latex_special_char('~', ['\\',t,i,l,d,e,'{','\\',' ','}'|X], X). 

latex_esc_char('%').
latex_esc_char('&').
latex_esc_char('|').
latex_esc_char('_').
latex_esc_char('{').
latex_esc_char('}').
latex_esc_char('#').
latex_esc_char('$').
latex_esc_char('^').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

convert_to_index_label(W, W1) :-
	atom_chars(W, Xs),
	map_il_equivalent(Xs, Xs1),
	atom_chars(W1, Xs1).

map_il_equivalent([X|Xs], Xs1) :-
	il_equivalent(X, X1),
	append(X1, Xs2, Xs1),
	map_il_equivalent(Xs, Xs2).
map_il_equivalent([], []).

il_equivalent(C, [C]) :-
	char_type(C, ascii),
	char_type(C, alnum),
	!.
il_equivalent(C, [' ']) :-
	char_type(C, white),
	!.
% il_equivalent('!', ['"','!']) :- !.
% il_equivalent('@', ['"','@']) :- !.
% il_equivalent('|', ['"','|']) :- !.
% il_equivalent('"', ['"','"']) :- !.
il_equivalent('!', ['-']) :- !.
il_equivalent('@', ['-']) :- !.
il_equivalent('|', ['-']) :- !.
il_equivalent('"', ['-']) :- !.
il_equivalent('ä', [a,e]) :-	!.
il_equivalent('ö', [o,e]) :-	!.
il_equivalent('ü', [u,e]) :-	!.
il_equivalent('Ä', ['A',e]) :-	!.
il_equivalent('Ö', ['O',e]) :-	!.
il_equivalent('Ü', ['U',e]) :-	!.
il_equivalent('ß', [s,s]) :-	!.
il_equivalent('é', [e]) :-	!.
il_equivalent('á', [a]) :-	!.
il_equivalent('ñ', [n]) :-	!.
il_equivalent('ã', [a]) :-	!.
il_equivalent(X, [X]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

map_add_firstarg([X|Xs], [K-X|Xs1]) :-
	arg(1, X, K),
	map_add_firstarg(Xs, Xs1).
map_add_firstarg([], []).

map_val([_-X|Xs], [X|Xs1]) :-
	map_val(Xs, Xs1).
map_val([], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pplkb_header(
'\\documentclass[a4paper]{article}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{imakeidx}
\\usepackage[hidelinks]{hyperref}
%% A screen friendly geometry:	    
\\usepackage[paper=a5paper,scale=0.9]{geometry}
').	    

pplkb_setup(
'%% PPL Setup
	   
\\newcommand{\\assign}{\\mathrel{\\mathop:}=}
\\newcommand{\\concat}{\\mathrel{+\\!+}}
	    
\\newcommand{\\f}[1]{\\mathsf{#1}}
\\newcommand{\\true}{\\top}
\\newcommand{\\false}{\\bot}
\\newcommand{\\imp}{\\rightarrow}
\\newcommand{\\revimp}{\\leftarrow}
\\newcommand{\\equi}{\\leftrightarrow}
\\newcommand{\\entails}{\\models}	    
\\newcommand{\\eqdef}{\\; 
\\raisebox{-0.1ex}[0mm]{$ \\stackrel{\\raisebox{-0.2ex}{\\tiny 
\\textnormal{def}}}{=} $}\\; }
\\newcommand{\\iffdef}{\\n{iff}_{\\mbox{\\scriptsize \\textnormal{def}}}}

\\newcommand{\\pplmacro}[1]{\\mathit{#1}}
\\newcommand{\\ppldefmacro}[1]{\\mathit{#1}}
\\newcommand{\\pplparam}[1]{\\mathit{#1}}
\\newcommand{\\pplparamidx}[2]{\\mathit{#1}_{#2}}
\\newcommand{\\pplparamplain}[1]{#1}
\\newcommand{\\pplparamplainidx}[2]{#1_{#2}}
\\newcommand{\\pplparamsup}[2]{\\mathit{#1}^{#2}}
\\newcommand{\\pplparamsupidx}[3]{\\mathit{#1}^{#2}_{#3}}
\\newcommand{\\pplparamplainsup}[2]{#1^{#2}}
\\newcommand{\\pplparamplainsupidx}[3]{#1^{#2}_{#3}}
\\newcommand{\\pplparamnum}[1]{\\mathit{X}_{#1}}

%%	    
%% We use @startsection just to obtain reduced vertical spacing above
%% macro headers which are immediately after other headers, e.g. of sections
%%	    
\\makeatletter%
\\newcounter{entry}%
\\newcommand{\\entrymark}[1]{}%
\\newcommand\\entryhead{%
\\@startsection{entry}{10}{\\z@}{12pt plus 2pt minus 2pt}{0pt}{}}%
\\makeatother
	    
\\newcommand{\\pplkbBefore}
{\\entryhead*{}%
\\setlength{\\arraycolsep}{0pt}%
\\pagebreak[0]%
\\begin{samepage}%
\\noindent%
\\rule[0.5pt]{\\textwidth}{2pt}\\\\%
\\noindent}

% \\newcommand{\\pplkbDefType}[1]{\\hspace{\\fill}{{[}#1{]}\\\\}}

\\newcommand{\\pplkbBetween}
{\\setlength{\\arraycolsep}{3pt}%
\\\\\\rule[3pt]{\\textwidth}{1pt}%
\\par\\nopagebreak\\noindent Defined as\\begin{center}}

\\newcommand{\\pplkbAfter}{\\end{center}\\end{samepage}\\noindent}

\\newcommand{\\pplkbBodyBefore}{\\par\\noindent where\\begin{center}}
\\newcommand{\\pplkbBodyAfter}{\\end{center}}

\\newcommand{\\pplkbFreePredicates}[1]{\\f{free\\_predicates}(#1)}
% \\newcommand{\\pplkbRenameFreeOccurrences}[3]{\\f{rename\\_free\\_occurrences}(#1,#2,#3)}

\\newcommand{\\pplIsValid}[1]{\\noindent This formula is valid: $#1$\\par}
\\newcommand{\\pplIsNotValid}[1]{\\noindent This formula is not valid: $#1$\\par}	    
\\newcommand{\\pplFailedToValidate}[1]{\\noindent Failed to validate this formula: $#1$\\par}

\\newcounter{def}
	    
\\makeindex
').

pplkb_begin_document('
\\begin{document}
').

pplkb_footer('\\printindex
\\end{document}
').

