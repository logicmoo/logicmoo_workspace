/*  Part of CHR (Constraint Handling Rules)

    Author:        Tom Schrijvers and Jan Wielemaker
    E-mail:        Tom.Schrijvers@cs.kuleuven.be
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2004-2015, K.U. Leuven
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

%% SWI begin
:- module(chr,
	  [ op(1180, xfx, ==>),
	    op(1180, xfx, <=>),
	    op(1150, fx, constraints),
	    op(1150, fx, chr_constraint),
	    op(1150, fx, chr_preprocessor),
	    op(1150, fx, handler),
	    op(1150, fx, rules),
	    op(1100, xfx, \),
	    op(1200, xfx, @),
	    op(1190, xfx, pragma),
	    op( 500, yfx, #),
	    op(1150, fx, chr_type),
	    op(1150, fx, chr_declaration),
	    op(1130, xfx, --->),
	    op(1150, fx, (?)),
	    chr_show_store/1,		% +Module
	    find_chr_constraint/1,	% +Pattern
	    chr_trace/0,
	    chr_notrace/0,
	    chr_leash/1			% +Ports
	  ]).
:- use_module(library(dialect), [expects_dialect/1]).
:- expects_dialect(swi).

:- set_prolog_flag(generate_debug_info, false).

:- multifile
	debug_ask_continue/1,
	preprocess/2.

:- multifile user:file_search_path/2.
:- dynamic   user:file_search_path/2.
:- dynamic   chr_translated_program/1.

user:file_search_path(chr, library(chr)).

:- load_files([ chr(chr_translate),
		chr(chr_runtime),
		chr(chr_messages),
		chr(chr_hashtable_store),
		chr(chr_compiler_errors)
	      ],
	      [ if(not_loaded),
		silent(true)
	      ]).

:- use_module(library(lists), [member/2]).
%% SWI end

%% SICStus begin
%% :- module(chr,[
%%	chr_trace/0,
%%	chr_notrace/0,
%%	chr_leash/0,
%%	chr_flag/3,
%%	chr_show_store/1
%%	]).
%%
%% :- op(1180, xfx, ==>),
%%	op(1180, xfx, <=>),
%%	op(1150, fx, constraints),
%%	op(1150, fx, handler),
%%	op(1150, fx, rules),
%%	op(1100, xfx, \),
%%	op(1200, xfx, @),
%%	op(1190, xfx, pragma),
%%	op( 500, yfx, #),
%%	op(1150, fx, chr_type),
%%	op(1130, xfx, --->),
%%	op(1150, fx, (?)).
%%
%% :- multifile user:file_search_path/2.
%% :- dynamic   chr_translated_program/1.
%%
%% user:file_search_path(chr, library(chr)).
%%
%%
%% :- use_module('chr/chr_translate').
%% :- use_module('chr/chr_runtime').
%% :- use_module('chr/chr_hashtable_store').
%% :- use_module('chr/hprolog').
%% SICStus end

:- multifile chr:'$chr_module'/1.

:- dynamic chr_term/3.		% File, Term

:- dynamic chr_pp/2.		% File, Term

%	chr_expandable(+Term)
%
%	Succeeds if Term is a  rule  that   must  be  handled by the CHR
%	compiler. Ideally CHR definitions should be between
%
%		:- constraints ...
%		...
%		:- end_constraints.
%
%	As they are not we have to   use  some heuristics. We assume any
%	file is a CHR after we've seen :- constraints ...

chr_expandable((:- constraints _)).
chr_expandable((constraints _)).
chr_expandable((:- chr_constraint _)).
chr_expandable((:- chr_type _)).
chr_expandable((chr_type _)).
chr_expandable((:- chr_declaration _)).
chr_expandable(option(_, _)).
chr_expandable((:- chr_option(_, _))).
chr_expandable((handler _)).
chr_expandable((rules _)).
chr_expandable((_ <=> _)).
chr_expandable((_ @ _)).
chr_expandable((_ ==> _)).
chr_expandable((_ pragma _)).

%	chr_expand(+Term, -Expansion)
%
%	Extract CHR declarations and rules from the file and run the
%	CHR compiler when reaching end-of-file.

%% SWI begin
extra_declarations([ (:- use_module(chr(chr_runtime))),
		     (:- style_check(-discontiguous)),
		     (:- style_check(-singleton)),
		     (:- style_check(-no_effect)),
		     (:- set_prolog_flag(generate_debug_info, false))
		   | Tail
		   ], Tail).
%% SWI end

%% SICStus begin
%% extra_declarations([(:-use_module(chr(chr_runtime)))
%%		     , (:- use_module(chr(hprolog),[term_variables/2,term_variables/3]))
%%		     , (:-use_module(chr(hpattvars)))
%%		     | Tail], Tail).
%% SICStus end

chr_expand(Term, []) :-
	chr_expandable(Term), !,
	prolog_load_context(source,Source),
	prolog_load_context(source,File),
	prolog_load_context(term_position,Pos),
	stream_position_data(line_count,Pos,SourceLocation),
	add_pragma_to_chr_rule(Term,source_location(File:SourceLocation),NTerm),
	assert(chr_term(Source, SourceLocation, NTerm)).
chr_expand(Term, []) :-
	Term = (:- chr_preprocessor Preprocessor), !,
	prolog_load_context(source,File),
	assert(chr_pp(File, Preprocessor)).
chr_expand(end_of_file, FinalProgram) :-
	extra_declarations(FinalProgram,Program),
	prolog_load_context(source,File),
	findall(T, retract(chr_term(File,_Line,T)), CHR0),
	CHR0 \== [],
	prolog_load_context(module, Module),
	add_debug_decl(CHR0, CHR1),
	add_optimise_decl(CHR1, CHR2),
	call_preprocess(CHR2, CHR3),
	CHR4 = [ (:- module(Module, [])) | CHR3 ],
	findall(P, retract(chr_pp(File, P)), Preprocessors),
	( Preprocessors = [] ->
		CHR4 = CHR
	; Preprocessors = [Preprocessor] ->
		chr_compiler_errors:chr_info(preprocessor,'\tPreprocessing with ~w.\n',[Preprocessor]),
		call_chr_preprocessor(Preprocessor,CHR4,CHR)
	;
		chr_compiler_errors:print_chr_error(error(syntax(Preprocessors),'Too many preprocessors! Only one is allowed!\n',[])),
		fail
	),
	catch(call_chr_translate(File,
			   [ (:- module(Module, []))
			   | CHR
			   ],
			   Program0),
		chr_error(Error),
		(	chr_compiler_errors:print_chr_error(Error),
			fail
		)
	),
	delete_header(Program0, Program).


delete_header([(:- module(_,_))|T0], T) :- !,
	delete_header(T0, T).
delete_header(L, L).

add_debug_decl(CHR, CHR) :-
	member(option(Name, _), CHR), Name == debug, !.
add_debug_decl(CHR, CHR) :-
	member((:- chr_option(Name, _)), CHR), Name == debug, !.
add_debug_decl(CHR, [(:- chr_option(debug, Debug))|CHR]) :-
	(   chr_current_prolog_flag(generate_debug_info, true)
	->  Debug = on
	;   Debug = off
	).

%% SWI begin
chr_current_prolog_flag(Flag,Val) :- current_prolog_flag(Flag,Val).
%% SWI end

add_optimise_decl(CHR, CHR) :-
	\+(\+(memberchk((:- chr_option(optimize, _)), CHR))), !.
add_optimise_decl(CHR, [(:- chr_option(optimize, full))|CHR]) :-
	chr_current_prolog_flag(optimize, full), !.
add_optimise_decl(CHR, CHR).

%%	call_preprocess(+CHR0, -CHR) is det.
%
%	Call user chr:preprocess(CHR0, CHR).

call_preprocess(CHR0, CHR) :-
	preprocess(CHR0, CHR), !.
call_preprocess(CHR, CHR).

%	call_chr_translate(+File, +In, -Out)
%
%	The entire chr_translate/2 translation may fail, in which case we'd
%	better issue a warning  rather  than   simply  ignoring  the CHR
%	declarations.

call_chr_translate(File, In, _Out) :-
	( chr_translate_line_info(In, File, Out0) ->
	    nb_setval(chr_translated_program,Out0),
	    fail
	).
call_chr_translate(_, _In, Out) :-
	nb_current(chr_translated_program,Out), !,
	nb_delete(chr_translated_program).

call_chr_translate(File, _, []) :-
	print_message(error, chr(compilation_failed(File))).

call_chr_preprocessor(Preprocessor,CHR,_NCHR) :-
	( call(Preprocessor,CHR,CHR0) ->
		nb_setval(chr_preprocessed_program,CHR0),
		fail
	).
call_chr_preprocessor(_,_,NCHR)	:-
	nb_current(chr_preprocessed_program,NCHR), !,
	nb_delete(chr_preprocessed_program).
call_chr_preprocessor(Preprocessor,_,_) :-
	chr_compiler_errors:print_chr_error(error(preprocessor,'Preprocessor `~w\' failed!\n',[Preprocessor])).

%% SWI begin

		 /*******************************
		 *      SYNCHRONISE TRACER	*
		 *******************************/

:- multifile
	user:message_hook/3,
	chr:debug_event/2,
	chr:debug_interact/3.
:- dynamic
	user:message_hook/3.

user:message_hook(trace_mode(OnOff), _, _) :-
	(   OnOff == on
	->  chr_trace
	;   chr_notrace
	),
	fail.				% backtrack to other handlers

:- public
	debug_event/2,
	debug_interact/3.

%%	debug_event(+State, +Event)
%
%	Hook into the CHR debugger.  At this moment we will discard CHR
%	events if we are in a Prolog `skip' and we ignore the

debug_event(_State, _Event) :-
	tracing,			% are we tracing?
	prolog_skip_level(Skip, Skip),
	Skip \== very_deep,
	prolog_current_frame(Me),
	prolog_frame_attribute(Me, level, Level),
	Level > Skip, !.

%%	debug_interact(+Event, +Depth, -Command)
%
%	Hook into the CHR debugger to display Event and ask for the next
%	command to execute. This  definition   causes  the normal Prolog
%	debugger to be used for the standard ports.

debug_interact(Event, _Depth, creep) :-
	prolog_event(Event),
	tracing, !.

prolog_event(call(_)).
prolog_event(exit(_)).
prolog_event(fail(_)).

%%	debug_ask_continue(-Command) is semidet.
%
%	Hook to ask for a CHR debug   continuation. Must bind Command to
%	one of =creep=, =skip=, =ancestors=, =nodebug=, =abort=, =fail=,
%	=break=, =help= or =exit=.


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(chr(CHR)) -->
	chr_message(CHR).

:- multifile
        check:trivial_fail_goal/1.

check:trivial_fail_goal(_:Goal) :-
        functor(Goal, Name, _),
        sub_atom(Name, 0, _, _, '$chr_store_constants_').

		 /*******************************
		 *	 TOPLEVEL PRINTING	*
		 *******************************/

:- create_prolog_flag(chr_toplevel_show_store, true, []).

:- residual_goals(chr_residuals).

%%	chr_residuals// is det.
%
%	Find the CHR constraints from the   store.  These are accessible
%	through the nondet predicate   current_chr_constraint/1. Doing a
%	findall/4 however would loose the  bindings. We therefore rolled
%	findallv/4,  which  exploits  non-backtrackable  assignment  and
%	realises a copy of the template  without disturbing the bindings
%	using this strangely looking construct.   Note that the bindings
%	created by the unifications are in New,  which is newer then the
%	latest choicepoint and therefore the bindings are not trailed.
%
%	  ==
%	  duplicate_term(Templ, New),
%	  New = Templ
%	  ==

chr_residuals(Residuals, Tail) :-
	chr_current_prolog_flag(chr_toplevel_show_store,true),
	nb_current(chr_global, _), !,
	Goal = _:_,
	findallv(Goal, current_chr_constraint(Goal), Residuals, Tail).
chr_residuals(Residuals, Residuals).

:- meta_predicate
	findallv(?, 0, ?, ?).

findallv(Templ, Goal, List, Tail) :-
	List2 = [x|_],
	State = state(List2),
	(   call(Goal),
	    arg(1, State, L),
	    duplicate_term(Templ, New),
	    New = Templ,
	    Cons = [New|_],
	    nb_linkarg(2, L, Cons),
	    nb_linkarg(1, State, Cons),
	    fail
	;   List2 = [x|List],
	    arg(1, State, Last),
	    arg(2, Last, Tail)
	).


		 /*******************************
		 *	   MUST BE LAST!	*
		 *******************************/

:- multifile system:term_expansion/2.
:- dynamic   system:term_expansion/2.

system:term_expansion(In, Out) :-
	\+ current_prolog_flag(xref, true),
	chr_expand(In, Out).
%% SWI end

%% SICStus begin
%
% :- dynamic
%	current_toplevel_show_store/1,
%	current_generate_debug_info/1,
%	current_optimize/1.
%
% current_toplevel_show_store(on).
%
% current_generate_debug_info(false).
%
% current_optimize(off).
%
% chr_current_prolog_flag(generate_debug_info, X) :-
%	chr_flag(generate_debug_info, X, X).
% chr_current_prolog_flag(optimize, X) :-
%	chr_flag(optimize, X, X).
%
% chr_flag(Flag, Old, New) :-
%	Goal = chr_flag(Flag,Old,New),
%	g must_be(Flag, oneof([toplevel_show_store,generate_debug_info,optimize]), Goal, 1),
%	chr_flag(Flag, Old, New, Goal).
%
% chr_flag(toplevel_show_store, Old, New, Goal) :-
%	clause(current_toplevel_show_store(Old), true, Ref),
%	(   New==Old -> true
%	;   must_be(New, oneof([on,off]), Goal, 3),
%	    erase(Ref),
%	    assertz(current_toplevel_show_store(New))
%	).
% chr_flag(generate_debug_info, Old, New, Goal) :-
%	clause(current_generate_debug_info(Old), true, Ref),
%	(   New==Old -> true
%	;   must_be(New, oneof([false,true]), Goal, 3),
%	    erase(Ref),
%	    assertz(current_generate_debug_info(New))
%	).
% chr_flag(optimize, Old, New, Goal) :-
%	clause(current_optimize(Old), true, Ref),
%	(   New==Old -> true
%	;   must_be(New, oneof([full,off]), Goal, 3),
%	    erase(Ref),
%	    assertz(current_optimize(New))
%	).
%
%
% all_stores_goal(Goal, CVAs) :-
%	chr_flag(toplevel_show_store, on, on), !,
%	findall(C-CVAs, find_chr_constraint(C), Pairs),
%	andify(Pairs, Goal, CVAs).
% all_stores_goal(true, _).
%
% andify([], true, _).
% andify([X-Vs|L], Conj, Vs) :- andify(L, X, Conj, Vs).
%
% andify([], X, X, _).
% andify([Y-Vs|L], X, (X,Conj), Vs) :- andify(L, Y, Conj, Vs).
%
% :- multifile user:term_expansion/6.
%
% user:term_expansion(In, _, Ids, Out, [], [chr|Ids]) :-
%	nonvar(In),
%	nonmember(chr, Ids),
%	chr_expand(In, Out), !.
%
%% SICStus end

%%% for SSS %%%

add_pragma_to_chr_rule((Name @ Rule), Pragma, Result) :- !,
	add_pragma_to_chr_rule(Rule,Pragma,NRule),
	Result = (Name @ NRule).
add_pragma_to_chr_rule((Rule pragma Pragmas), Pragma, Result) :- !,
	Result = (Rule pragma (Pragma,Pragmas)).
add_pragma_to_chr_rule((Head ==> Body), Pragma, Result) :- !,
	Result = (Head ==> Body pragma Pragma).
add_pragma_to_chr_rule((Head <=> Body), Pragma, Result) :- !,
	Result = (Head <=> Body pragma Pragma).
add_pragma_to_chr_rule(Term,_,Term).


		 /*******************************
		 *	  SANDBOX SUPPORT	*
		 *******************************/

:- multifile
	sandbox:safe_primitive/1.

% CHR uses a lot of global variables. We   don't  really mind as long as
% the user does not mess around  with   global  variable that may have a
% predefined meaning.

sandbox:safe_primitive(system:b_setval(V, _)) :-
	chr_var(V).
sandbox:safe_primitive(system:nb_linkval(V, _)) :-
	chr_var(V).
sandbox:safe_primitive(chr:debug_event(_,_)).
sandbox:safe_primitive(chr:debug_interact(_,_,_)).

chr_var(Name) :- sub_atom(Name, 0, _, _, '$chr').
chr_var(Name) :- sub_atom(Name, 0, _, _, 'chr').


		 /*******************************
		 *     SYNTAX HIGHLIGHTING	*
		 *******************************/

:- multifile
	prolog_colour:term_colours/2,
	prolog_colour:goal_colours/2.

%%	term_colours(+Term, -Colours)
%
%	Colourisation of a toplevel term as read from the file.

term_colours((_Name @ Rule), delimiter - [ identifier, RuleColours ]) :- !,
	term_colours(Rule, RuleColours).
term_colours((Rule pragma _Pragma), delimiter - [RuleColours,pragma]) :- !,
	term_colours(Rule, RuleColours).
term_colours((Head <=> Body), delimiter - [ HeadColours, BodyColours ]) :- !,
	chr_head(Head, HeadColours),
	chr_body(Body, BodyColours).
term_colours((Head ==> Body), delimiter - [ HeadColours, BodyColours ]) :- !,
	chr_head(Head, HeadColours),
	chr_body(Body, BodyColours).

chr_head(_C#_Id, delimiter - [ head, identifier ]) :- !.
chr_head((A \ B), delimiter - [ AC, BC ]) :- !,
	chr_head(A, AC),
	chr_head(B, BC).
chr_head((A, B), functor - [ AC, BC ]) :- !,
	chr_head(A, AC),
	chr_head(B, BC).
chr_head(_, head).

chr_body((Guard|Goal), delimiter - [ GuardColour, GoalColour ]) :- !,
	chr_body(Guard, GuardColour),
	chr_body(Goal, GoalColour).
chr_body(_, body).


%%	goal_colours(+Goal, -Colours)
%
%	Colouring of special goals.

goal_colours(constraints(Decls), deprecated-[DeclColours]) :-
	chr_constraint_colours(Decls, DeclColours).
goal_colours(chr_constraint(Decls), built_in-[DeclColours]) :-
	chr_constraint_colours(Decls, DeclColours).
goal_colours(chr_type(TypeDecl), built_in-[DeclColours]) :-
	chr_type_decl_colours(TypeDecl, DeclColours).
goal_colours(chr_option(Option,Value), built_in-[OpC,ValC]) :-
	chr_option_colours(Option, Value, OpC, ValC).

chr_constraint_colours(Var, instantiation_error(Var)) :-
	var(Var), !.
chr_constraint_colours((H,T), classify-[HeadColours,BodyColours]) :- !,
	chr_constraint_colours(H, HeadColours),
	chr_constraint_colours(T, BodyColours).
chr_constraint_colours(PI, Colours) :-
	pi_to_term(PI, Goal), !,
	Colours = predicate_indicator-[ goal(constraint(0), Goal),
					arity
				      ].
chr_constraint_colours(Goal, Colours) :-
	atom(Goal), !,
	Colours = goal(constraint(0), Goal).
chr_constraint_colours(Goal, Colours) :-
	compound(Goal), !,
	compound_name_arguments(Goal, _Name, Args),
	maplist(chr_argspec, Args, ArgColours),
	Colours = goal(constraint(0), Goal)-ArgColours.

chr_argspec(Term, mode(Mode)-[chr_type(Type)]) :-
	compound(Term),
	compound_name_arguments(Term, Mode, [Type]),
	chr_mode(Mode).

chr_mode(+).
chr_mode(?).
chr_mode(-).

pi_to_term(Name/Arity, Term) :-
	atom(Name), integer(Arity), Arity >= 0, !,
	functor(Term, Name, Arity).

chr_type_decl_colours((Type ---> Def), built_in-[chr_type(Type), DefColours]) :-
	chr_type_colours(Def, DefColours).
chr_type_decl_colours((Type == Alias), built_in-[chr_type(Type), chr_type(Alias)]).

chr_type_colours(Var, classify) :-
	var(Var), !.
chr_type_colours((A;B), control-[CA,CB]) :- !,
	chr_type_colours(A, CA),
	chr_type_colours(B, CB).
chr_type_colours(T, chr_type(T)).

chr_option_colours(Option, Value, identifier, ValCol) :-
	chr_option_range(Option, Values), !,
	(   nonvar(Value),
	    memberchk(Value, Values)
	->  ValCol = classify
	;   ValCol = error
	).
chr_option_colours(_, _, error, classify).

chr_option_range(check_guard_bindings, [on,off]).
chr_option_range(optimize, [off, full]).
chr_option_range(debug, [on, off]).

prolog_colour:term_colours(Term, Colours) :-
	term_colours(Term, Colours).
prolog_colour:goal_colours(Term, Colours) :-
	goal_colours(Term, Colours).
