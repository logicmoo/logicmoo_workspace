/** <module> PFC Compatibility module

This  module  provides  compatibility  to   PFC  through  the  directive
expects_dialect/1:

	==
	:- expects_dialect(pfc)
	==

@tbd this module meeds

	* Implement system predicates available in PFC we do not yet or
	do not wish to support in SWI-Prolog.  Export these predicates.

	* Provide pfc_<name>(...) predicates for predicates that exist
	both in PFC and SWI-Prolog and define goal_expansion/2 rules to
	map calls to these predicates to the pfc_<name> version.
	Export these predicates.

	* Alter the library search path, placing dialect/pfc *before*
	the system libraries.

	* Allow for =|.pfc|= extension as extension for Prolog files.
	If both a =|.pl|= and =|.pfc|= is present, the =|.pfc|= file
	is loaded if the current environment expects PFC.

@tbd	The dialect-compatibility packages are developed in a
	`demand-driven' fashion.  Please contribute to this package. Fill it in!
@author Douglas R. Miles
*/

:- module(pfc, [pop_pfc_dialect/0,push_pfc_dialect/0,dialect_input_stream/1, calc_load_module_pfc/1]).
% :- asserta(swish:is_a_module).


		 /*******************************
		 *	     EXPANSION		*
		 *******************************/

:- multifile
	user:goal_expansion/2,
	user:file_search_path/2,
	user:prolog_file_type/2,
	pfc_gOAL_expansion/2.
	
:- dynamic
	user:goal_expansion/2,
	user:file_search_path/2,
	user:prolog_file_type/2.

% :- notrace(system:ensure_loaded(library(operators))).


pfc_debug(Info):- ignore(notrace((debug(pfc(dialect),'~N% ~p.',[Info])))).
% pfc_debug(X):- format(user_error,'~N% PFC_DEBUG: ~q.~n',[X]),flush_output(user_error).

%%	pfc_gOAL_expansion(+In, +Out)
%
%	goal_expansion rules to emulate PFC behaviour in SWI-Prolog. The
%	expansions  below  maintain  optimization    from   compilation.
%	Defining them as predicates would loose compilation.

pfc_gOAL_expansion(expects_dialect(Dialect), Out):- 
   % in case it is used more than once
   pfc == Dialect -> 
       Out = debug(pfc(term_expansion),'~q.',[(expects_dialect(Dialect))])
     ; Out=pop_pfc_dialect.
/*
pfc_gOAL_expansion(eval_arith(Expr, Result),
	      Result is Expr).

pfc_gOAL_expansion(if(Goal, Then),
	      (Goal *-> Then; true)).
pfc_gOAL_expansion(if(Goal, Then, Else),
	      (Goal *-> Then; Else)).
pfc_gOAL_expansion(style_check(Style),
	      pfc_style_check(Style)).

*/

		 /*******************************
		 *	    LIBRARY SETUP	*
		 *******************************/

%	Pushes searching for  dialect/pfc  in   front  of  every library
%	directory that contains such as sub-directory.

:-      
   exists_source(library(dialect/pfc)) -> true;
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
   absolute_file_name('pfc_autoload', Dir,
			       [ file_type(directory),
				 access(read),
                                 relative_to(ThisDir),
				 file_errors(fail)
			       ]),
      asserta((user:file_search_path(library, Dir) :-
	prolog_load_context(dialect, pfc))).
*/
:- user:file_search_path(pfc_library, Dir) -> true;
    (prolog_load_context(directory, ThisDir),
         absolute_file_name('../..', Dir,
			       [ file_type(directory),
				 access(read),
                                 relative_to(ThisDir),
				 file_errors(fail)
			       ]),
	    asserta((user:file_search_path(pfc_library, Dir)))).



%%	push_pfc_file_extension
%
%	Looks for .pfc files before looking for .pl files if the current
%	dialect is =pfc=.

push_pfc_file_extension :-
	asserta((user:prolog_file_type(pfc, prolog) :-
		    prolog_load_context(dialect, pfc))).


:- push_pfc_file_extension.


:- multifile
	prolog:message//1.

prolog:message(pfc_unsupported(Goal)) -->
	[ 'PFC emulation (pfc.pl): unsupported: ~p'-[Goal] ].


:- use_module(library(pengines),[pengine_self/1]). 

calc_load_module_pfc(OM):- pengine_self(OM),!.
calc_load_module_pfc(OM):- 
     '$current_typein_module'(TM), 
     prolog_load_context(module,Load),strip_module(_,Strip,_),
     context_module(Ctx),'$current_source_module'(SM),
     ((SM==Load,SM\==user)-> M = SM ;
     ((TM\==Load,TM\==user) -> M = TM ; (M = SM))),
     OM=Load,
     pfc_debug([ti=TM,load=Load,strip=Strip,ctx=Ctx,sm=SM,pfc=M,using=OM]),!.     

calc_load_module_pfc(M):- 
    (member(Call,[
     prolog_load_context(module,M),
     pengine_self(M),
     '$current_source_module'(M),
     '$current_typein_module'(M),
     pfc:pfc_program_module(M),
     strip_module(_,M,_),
     context_module(M),
     source_location(M,_)]),
    call(Call),
    pfc_debug(calc_load_module_pfc(Call)),
    \+ likely_reserved_module(M)); pfc:must_pfc_program_module(M).

likely_reserved_module(M):- M=user; 
  module_property(M,P), member(P,[class(library),class(system),exported_operators([_|_]),exports([_|_])]).
  



    :- volatile(tmp:module_dialect_pfc/4).
:- thread_local(tmp:module_dialect_pfc/4).


:- pfc:export(pfc:push_pfc_dialect/0). 
:- system:import(pfc:push_pfc_dialect/0). 

:- system:module_transparent(pfc:setup_dialect/0). 
:- system:module_transparent(pfc:pop_pfc_dialect/0).
:- system:module_transparent(pfc:push_pfc_dialect/0).
%:- system:module_transparent(pfc:push_pfc_dialect_now/2).

pfc:setup_dialect:- 
    pfc_debug(push_pfc_dialect),pfc_debug(ops),
    (push_pfc_dialect->true;(trace,push_pfc_dialect)),
    pfc_debug(continue_pfc_dialect),pfc_debug(ops).

:- system:module_transparent(prolog_dialect:expects_dialect/1). 
%:- prolog_dialect:import(pfc:push_pfc_dialect/0). 



% :- prolog_dialect:asserta((())).
% :- thread_local(pfc:pfc_program_module/1).


% get_pfc_alt_user_module(_User,PFC_USER):- pfc:pfc_program_module(PFC_USER),!.
get_pfc_alt_user_module( user, baseKB):-!.
get_pfc_alt_user_module( User,PFC_USER):- is_pfc_alt_user_module(User,PFC_USER),!.
%get_pfc_alt_user_module(_User,PFC_USER):- pfc:pfc_program_module(PFC_USER),!.

% is_pfc_alt_user_module(user,baseKB):-!.
is_pfc_alt_user_module(_User,Out):- gensym(pfc, Out).

% is_pfc_alt_user_module(baseKB).


pfc_operators(M,[
              op(1200,xfx,(M:(<-))),
              op(1050,fx,(M:(<-))),  
              op(1200,xfx,(M:(==>))),
              op(1050,fx,(M:(==>))), 
              op(1200,xfx,(M:(<==>)))
]).

push_pfc_dialect:-
   calc_load_module_pfc(M),
   push_pfc_dialect_now(M, M).   
  
push_pfc_dialect_now(User, User):-  
  User==user,
  get_pfc_alt_user_module(User,PFC_USER),
  PFC_USER\==user,
  pfc_debug(alt_module(User,PFC_USER)),
  '$set_source_module'(PFC_USER),!,
  push_pfc_dialect_now(User, PFC_USER).


push_pfc_dialect_now(Was, M):-
   notrace(M:ensure_loaded(library(pfc_lib))),
   pfc:check_pfc_program_module(M),
   M:style_check(-discontiguous), M:style_check(-singleton),
   baseKB:define_pfc_into_module(M),
   dialect_input_stream(StreamIn),   
   pfc_operators(M, Ops),
   push_operators(M:Ops, Undo),
   %ignore(retract(tmp:module_dialect_pfc(StreamIn,_,_,_))), 
   asserta(tmp:module_dialect_pfc(StreamIn,Was,M,Undo)),!.

dialect_input_stream(StreamIn):- prolog_load_context(stream,StreamIn)->true;current_input(StreamIn).

pop_pfc_dialect:-
    dialect_input_stream(StreamIn),
    retract(tmp:module_dialect_pfc(StreamIn,Was,M,Undo)),!,
    pop_operators(Undo),
    pfc_debug(pop_pfc_dialect(StreamIn,M->Was)),
    %nop('$set_source_module'(Was)),!,
    pfc_debug(ops).

pop_pfc_dialect:-
    retract(tmp:module_dialect_pfc(StreamIn,Was,M,Undo)),!,
    print_message(warning, format('~q', [warn_pop_pfc_dialect_fallback(StreamIn,M->Was)])),
    %dumpST,
    %pfc_debug(ops),
    pop_operators(Undo),    
    %nop('$set_source_module'(Was)),!,
    pfc_debug(ops).
pop_pfc_dialect:- 
   pfc_debug(ops),
   print_message(warning, format('~q', [missing_pop_pfc_dialect_fallback])).


                 /*******************************
                 *         SYNTAX HOOKS         *
                 *******************************/

:- multifile
    prolog:alternate_syntax/4.


prolog:alternate_syntax(pfc, Module,
                        pfc:push_pfc_operators(Module),
                        pfc:pop_pfc_operators).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Note that we could generalise this to deal with all included files.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

push_pfc_operators :-
    '$set_source_module'(M, M),
    push_pfc_operators(M).

push_pfc_operators(Module) :-
    pfc_operators(Module, Ops),
    push_operators(Module:Ops).

pop_pfc_operators :-
    pop_operators.


user:goal_expansion(In, Out) :-
    prolog_load_context(dialect, pfc),
    pfc_gOAL_expansion(In, Out).



system:term_expansion(In, PosIn, Out, PosOut) :- 
  prolog_load_context(dialect, pfc),
  In == (:- include(system('date_utils.pl'))), 
  PosIn=PosOut, 
  expects_dialect(swi),
  Out = [(:- expects_dialect(swi)),
         (:- include(system('date_utils.pl'))),
         (:- expects_dialect(pfc))],!.

system:term_expansion(In, PosIn, Out, PosOut) :- In == end_of_file,
   prolog_load_context(dialect, pfc),
   dialect_input_stream(StreamIn),
   tmp:module_dialect_pfc(StreamIn,_,_,_),
   pop_pfc_dialect,!,
   Out = In,
   PosIn = PosOut.
      
      


