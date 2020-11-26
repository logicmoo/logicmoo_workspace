/** <module> LPS Compatibility module

This  module  provides  compatibility  to   LPS  through  the  directive
expects_dialect/1:

	==
	:- expects_dialect(lps)
	==

@tbd this module meeds

	* Implement system predicates available in LPS we do not yet or
	do not wish to support in SWI-Prolog.  Export these predicates.

	* Provide lps_<name>(...) predicates for predicates that exist
	both in LPS and SWI-Prolog and define goal_expansion/2 rules to
	map calls to these predicates to the lps_<name> version.
	Export these predicates.

	* Alter the library search path, placing dialect/lps *before*
	the system libraries.

	* Allow for =|.lps|= extension as extension for Prolog files.
	If both a =|.pl|= and =|.lps|= is present, the =|.lps|= file
	is loaded if the current environment expects LPS.

@tbd	The dialect-compatibility packages are developed in a
	`demand-driven' fashion.  Please contribute to this package. Fill it in!
@author Douglas R. Miles
*/

:- module(lps, [pop_lps_dialect/0,push_lps_dialect/0,dialect_input_stream/1]).
% :- asserta(swish:is_a_module).


		 /*******************************
		 *	     EXPANSION		*
		 *******************************/

:- multifile
	user:goal_expansion/2,
	user:file_search_path/2,
	user:prolog_file_type/2,
	lps_gOAL_expansion/2.
	
:- dynamic
	user:goal_expansion/2,
	user:file_search_path/2,
	user:prolog_file_type/2.

% :- notrace(system:ensure_loaded(library(operators))).


lps_debug(Info):- ignore(notrace((debug(lps(dialect),'~N% ~p.',[Info])))).
% lps_debug(X):- format(user_error,'~N% LPS_DEBUG: ~q.~n',[X]),flush_output(user_error).

%%	lps_gOAL_expansion(+In, +Out)
%
%	goal_expansion rules to emulate LPS behaviour in SWI-Prolog. The
%	expansions  below  maintain  optimization    from   compilation.
%	Defining them as predicates would loose compilation.

lps_gOAL_expansion(expects_dialect(Dialect), Out):- 
   % in case it is used more than once
   lps == Dialect -> 
       Out = debug(lps(term_expansion),'~q.',[(expects_dialect(Dialect))])
     ; Out=pop_lps_dialect.
/*
lps_gOAL_expansion(eval_arith(Expr, Result),
	      Result is Expr).

lps_gOAL_expansion(if(Goal, Then),
	      (Goal *-> Then; true)).
lps_gOAL_expansion(if(Goal, Then, Else),
	      (Goal *-> Then; Else)).
lps_gOAL_expansion(style_check(Style),
	      lps_style_check(Style)).

*/

		 /*******************************
		 *	    LIBRARY SETUP	*
		 *******************************/

%	Pushes searching for  dialect/lps  in   front  of  every library
%	directory that contains such as sub-directory.

:-      
   exists_source(library(dialect/lps)) -> true;
   (prolog_load_context(directory, ThisDir),
   absolute_file_name('..', Dir,
          [ file_type(directory),
            access(read),
            relative_to(ThisDir),
            file_errors(fail)
          ]),
   asserta((user:file_search_path(library, Dir)))).
/*
:- prolog_load_context(directory, ThisDir),
   absolute_file_name('lps_autoload', Dir,
			       [ file_type(directory),
				 access(read),
                                 relative_to(ThisDir),
				 file_errors(fail)
			       ]),
      asserta((user:file_search_path(library, Dir) :-
	prolog_load_context(dialect, lps))).
*/
:- user:file_search_path(lps_library, Dir) -> true;
    (prolog_load_context(directory, ThisDir),
         absolute_file_name('../..', Dir,
			       [ file_type(directory),
				 access(read),
                                 relative_to(ThisDir),
				 file_errors(fail)
			       ]),
	    asserta((user:file_search_path(lps_library, Dir)))).



%%	push_lps_file_extension
%
%	Looks for .lps files before looking for .pl files if the current
%	dialect is =lps=.

push_lps_file_extension :-
	asserta((user:prolog_file_type(lps, prolog) :-
		    prolog_load_context(dialect, lps))).


:- push_lps_file_extension.


:- multifile
	prolog:message//1.

prolog:message(lps_unsupported(Goal)) -->
	[ 'LPS emulation (lps.pl): unsupported: ~p'-[Goal] ].


:- use_module(library(pengines),[pengine_self/1]). 

calc_dialect_module(OM):- pengine_self(OM),!.
calc_dialect_module(OM):- 
     '$current_typein_module'(TM), 
     prolog_load_context(module,Load),strip_module(_,Strip,_),
     context_module(Ctx),'$current_source_module'(SM),
     ((SM==Load,SM\==user)-> M = SM ;
     ((TM\==Load,TM\==user) -> M = TM ; (M = SM))),
     OM=Load,
     lps_debug([ti=TM,load=Load,strip=Strip,ctx=Ctx,sm=SM,lps=M,using=OM]).     


    :- volatile(tmp:module_dialect_lps/4).
:- thread_local(tmp:module_dialect_lps/4).


:- lps:export(lps:push_lps_dialect/0). 
:- system:import(lps:push_lps_dialect/0). 

:- system:module_transparent(lps:setup_dialect/0). 
:- system:module_transparent(lps:pop_lps_dialect/0).
:- system:module_transparent(lps:push_lps_dialect/0).
%:- system:module_transparent(lps:push_lps_dialect_now/2).

lps:setup_dialect:- 
    lps_debug(push_lps_dialect),lps_debug(ops),
    (push_lps_dialect->true;(trace,push_lps_dialect)),
    lps_debug(continue_lps_dialect),lps_debug(ops).

:- system:module_transparent(prolog_dialect:expects_dialect/1). 
%:- prolog_dialect:import(lps:push_lps_dialect/0). 



% :- prolog_dialect:asserta((())).
:- thread_local(interpreter:lps_program_module/1).


get_lps_alt_user_module(_User,LPS_USER):- interpreter:lps_program_module(LPS_USER),!.
get_lps_alt_user_module( user, db):-!.
get_lps_alt_user_module( User,LPS_USER):- is_lps_alt_user_module(User,LPS_USER),!.
%get_lps_alt_user_module(_User,LPS_USER):- interpreter:lps_program_module(LPS_USER),!.

% is_lps_alt_user_module(user,db):-!.
is_lps_alt_user_module(_User,Out):- gensym(lps, Out).

% is_lps_alt_user_module(db).


push_lps_dialect:-
   calc_dialect_module(M),
   push_lps_dialect_now(M, M).   
  
push_lps_dialect_now(User, User):-  
  User==user,
  get_lps_alt_user_module(User,LPS_USER),
  LPS_USER\==user,
  lps_debug(alt_module(User,LPS_USER)),
  '$set_source_module'(LPS_USER),!,
  push_lps_dialect_now(User, LPS_USER).


push_lps_dialect_now(Was, M):-
   notrace(interpreter:ensure_loaded(library('../engine/interpreter.P'))),
   notrace(user:use_module(library('../swish/term_expander.pl'))),
   notrace(lps_repl:ensure_loaded(library(lps_corner))),
   %notrace(system:ensure_loaded(library(broadcast))),
   interpreter:check_lps_program_module(M),
   multifile(M:actions/1),dynamic(M:actions/1),
   dialect_input_stream(StreamIn),
   style_check(-discontiguous), style_check(-singleton),
   push_operators(M:[
     op(900,fy,(M:not)), 
     op(1200,xfx,(M:then)),
     op(1185,fx,(M:if)),
     op(1190,xfx,(M:if)),
     op(1100,xfy,(M:else)), 
     op(1050,xfx,(M:terminates)),
     op(1050,xfx,(M:initiates)),
     op(1050,xfx,(M:updates)),
% Rejected    (      op(1050,fx,impossible), 
     op(1050,fx,(M:observe)),
     op(1050,fx,(M:false)),
     op(1050,fx,(M:initially)),
     op(1050,fx,(M:fluents)),
     op(1050,fx,(M:events)),
     op(1050,fx,(M:prolog_events)),
     op(1050,fx,(M:actions)),
     op(1050,fx,(M:unserializable)),
% notice ',' has priority 1000
     op(999,fx,(M:update)),
     op(999,fx,(M:initiate)),
     op(999,fx,(M:terminate)),
     op(997,xfx,(M:in)),
     op(995,xfx,(M:at)),
     op(995,xfx,(M:during)),
     op(995,xfx,(M:from)), 
     op(994,xfx,(M:to)), % from's priority higher
     op(1050,xfy,(M:(::))),

% lps.js syntax extras
     op(1200,xfx,(M:(<-))),
     op(1050,fx,(M:(<-))),
% -> is already defined as 1050, xfy, which will do given that lps.js does not support if-then-elses
     op(700,xfx,((M:(<=))))],Undo),
   %ignore(retract(tmp:module_dialect_lps(StreamIn,_,_,_))), 
   asserta(tmp:module_dialect_lps(StreamIn,Was,M,Undo)),!.

dialect_input_stream(StreamIn):- prolog_load_context(stream,StreamIn)->true;current_input(StreamIn).

pop_lps_dialect:-
    dialect_input_stream(StreamIn),
    retract(tmp:module_dialect_lps(StreamIn,Was,M,Undo)),!,
    pop_operators(Undo),
    lps_debug(pop_lps_dialect(StreamIn,M->Was)),
    %nop('$set_source_module'(Was)),!,
    lps_debug(ops).
pop_lps_dialect:-
    retract(tmp:module_dialect_lps(StreamIn,Was,M,Undo)),!,
    print_message(warning, format('~q', [warn_pop_lps_dialect_fallback(StreamIn,M->Was)])),
    %dumpST,
    %lps_debug(ops),
    pop_operators(Undo),    
    %nop('$set_source_module'(Was)),!,
    lps_debug(ops).
pop_lps_dialect:- 
   lps_debug(ops),
   print_message(warning, format('~q', [missing_pop_lps_dialect_fallback])).



user:goal_expansion(In, Out) :-
    prolog_load_context(dialect, lps),
    lps_gOAL_expansion(In, Out).



system:term_expansion(In, PosIn, Out, PosOut) :- 
  prolog_load_context(dialect, lps),
  In == (:- include(system('date_utils.pl'))), 
  PosIn=PosOut, 
  expects_dialect(swi),
  Out = [(:- expects_dialect(swi)),
         (:- include(system('date_utils.pl'))),
         (:- expects_dialect(lps))],!.

system:term_expansion(In, PosIn, Out, PosOut) :- In == end_of_file,
   prolog_load_context(dialect, lps),
   dialect_input_stream(StreamIn),
   tmp:module_dialect_lps(StreamIn,_,_,_),
   pop_lps_dialect,!,
   Out = In,
   PosIn = PosOut.
      
      



