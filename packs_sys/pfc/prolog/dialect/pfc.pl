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

:- module(pfc, [pfc_pop_dialect/2, pfc_expects_dialect/1, pfc_debug/1, pfc_expects_dialect/4,dialect_input_stream_pfc/1]).
% :- asserta(swish:is_a_module).


		 /*******************************
		 *	     EXPANSION		*
		 *******************************/

:- multifile
	user:goal_expansion/2,
	user:file_search_path/2,
	user:prolog_file_type/2,
	pfc_dialect_expansion/2.
	
:- dynamic
	user:goal_expansion/2,
	user:file_search_path/2,
	user:prolog_file_type/2.

% :- notrace(system:ensure_loaded(library(operators))).


% pfc_debug(Info):- ignore(notrace((debug(pfc(dialect),'~N% ~p.',[Info])))).
pfc_debug(_):-!.
pfc_debug(I):- ignore(notrace(pfc_debug0(I))).
pfc_debug0(state):-!,prolog_load_context(dialect,D),
  G=tmp:module_dialect_pfc(_,_,_,M,_),predicate_property(G,number_of_clauses(NC)),
  pfc_debug(prolog_load_context(dialect,D,NC)),
  forall((G,current_op(X,fy,(M:'-'))), pfc_debug(current_op(X,fy,(M:'-'))+G)),
  current_op(X,fy,-),
  pfc_debug(current_op(X,fy,-)),!.

pfc_debug0(X):- format(user_error,'~N% PFC_DEBUG: ~q.~n',[X]),flush_output(user_error).
  

%%	pfc_dialect_expansion(+In, +Out)
%
%	goal_expansion rules to emulate PFC behaviour in SWI-Prolog. The
%	expansions  below  maintain  optimization    from   compilation.
%	Defining them as predicates would loose compilation.


pfc_dialect_expansion(expects_dialect(Dialect), Out):-
 (prolog_load_context(dialect, pfc); Dialect \== pfc ; true)-> Out = pfc_expects_dialect(Dialect).

pfc_expects_dialect(Dialect):-  
  prolog_load_context(module, M),  
  notrace(
  pfc:(prolog_load_context(dialect, Was),
  dialect_input_stream_pfc(Stream),
  pfc_debug(pfc_expects_dialect(Dialect,Stream,Was,M)))),
  pfc_expects_dialect(Dialect,Stream,Was,M),
  pfc_debug(state).

  
  
  %prolog_load_context(dialect, Was)
%  ((Was==pfc, Dialect\==pfc)-> pfc_pop_dialect ; true),
%  expects_dialect(Dialect).

/*
 % current_prolog_flag(emulated_dialect, pfc) 
 dumpST,
 wdmsg(expects_dialect(Dialect)),
   fail,
   % in case it is used more than once   
   pfc == Dialect -> 
       Out = debug(pfc(term_expansion),'~q.',[(expects_dialect(Dialect))])
     ; Out=pfc_pop_dialect.
*/
/*
pfc_dialect_expansion(eval_arith(Expr, Result),
	      Result is Expr).

pfc_dialect_expansion(if(Goal, Then),
	      (Goal *-> Then; true)).
pfc_dialect_expansion(if(Goal, Then, Else),
	      (Goal *-> Then; Else)).
pfc_dialect_expansion(style_check(Style),
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
    \+ likely_reserved_module(M)).
calc_load_module_pfc(M):- M==baseKB. % pfc:pfc_program_module(M).

likely_reserved_module(M):- M=user; 
  module_property(M,P), member(P,[class(library),class(system),exported_operators([_|_]),exports([_|_])]).




    :- volatile(tmp:module_dialect_pfc/5).
:- thread_local(tmp:module_dialect_pfc/5).


:- system:module_transparent(prolog_dialect:expects_dialect/1). 
:- system:module_transparent(pfc:setup_dialect/0). 
pfc:setup_dialect:- pfc_expects_dialect(pfc).
   

pfc_operators(M,[
%op(300,fy,(M:'-')),
%op(1200,xfx,(M:('=-=>'))),              
op(500,fx, M: ('~')),
op(1050,xfx,M: ('==>')),
op(1050,xfx,M: ('<==>')),
op(1050,xfx,M: ('<-')),
op(1100,fx,M: ('==>')),
op(1150,xfx,M: ('::::'))]).

other_dialect(Dialect):- Dialect\==pfc.

:- system:module_transparent(pfc:pfc_expects_dialect/4).
:- system:import(pfc:pfc_expects_dialect/4).
pfc_expects_dialect(SWI,Stream,_,M):- other_dialect(SWI),!,pfc_pop_dialect(Stream,M), expects_dialect(SWI).
%pfc_expects_dialect(SWI,_,Bin,_):- other_dialect(SWI),other_dialect(Bin),!, expects_dialect(SWI).
pfc_expects_dialect(WAS,_,WAS,_):- !. % expects_dialect(WAS).
pfc_expects_dialect(Next,Stream,Was,M):- tmp:module_dialect_pfc(Next,Stream,Was,M,_Undo), !.
pfc_expects_dialect(Next,StreamNow,Was,M):- tmp:module_dialect_pfc(Next,StreamBefore,Was,M,_Undo),
   StreamNow \== StreamBefore,!,
   retract(tmp:module_dialect_pfc(Next,StreamBefore,Was,M,Undo)),
   asserta(tmp:module_dialect_pfc(Next,StreamNow,Was,M,Undo)),!.

:- system:module_transparent(pfc:pfc_expects_dialect/1).
:- system:import(pfc:pfc_expects_dialect/1).
pfc_expects_dialect(pfc,Stream,Was,M):-
   %notrace(M:ensure_loaded(library(pfc_lib))),
   M:use_module(library(dialect/pfc)),
   ((current_prolog_flag(pfc_version,1.8);clause(pfcVersion(1.8),true)) -> M:ensure_loaded(library('../t/vlibs/pfc_1_8_full'));
      (M:use_module(library(pfc_lib)),M:set_fileAssertMt(M))),
   % dynamic(Was:'=-=>'/2),
   pfc_operators(M, Ops),
   push_operators(M:Ops, Undo),
   %ignore(retract(tmp:module_dialect_pfc(Dialect,Stream,_,_,_))), 
   asserta(tmp:module_dialect_pfc(pfc,Stream,Was,M,Undo)),!.
  

dialect_input_stream_pfc(Stream):- prolog_load_context(stream,Stream)->true;current_input(Stream).

:- system:module_transparent(pfc:pfc_pop_dialect/2).
:- system:import(pfc:pfc_pop_dialect/2).
pfc_pop_dialect(Stream,M):-
    dialect_input_stream_pfc(Stream),
    retract(tmp:module_dialect_pfc(pfc,Stream,Was,M,Undo)),!,
    pfc_debug(pop_pfc_dialect1(Stream,M->Was)),
    pop_operators(Undo),    
    %nop('$set_source_module'(Was)),!,
    pfc_debug(state).

pfc_pop_dialect(Stream,M):-
    retract(tmp:module_dialect_pfc(pfc,Stream,Was,M,Undo)),!,
    print_message(warning, format('~q', [warn_pop_pfc_dialect_fallback(Stream,M->Was)])),
    pfc_debug(pop_pfc_dialect2(Stream,M->Was)),
    pop_operators(Undo),    
    %nop('$set_source_module'(Was)),!,
    pfc_debug(state).
pfc_pop_dialect(Stream,M):- 
   pfc_debug(print_message(warning, format('~q', [missing_pop_pfc_dialect_fallback(Stream,M)]))),
   pfc_debug(state).


                 /*******************************
                 *         SYNTAX HOOKS         *
                 *******************************/

:- multifile
    prolog:alternate_syntax/4.


prolog:alternate_syntax(pfc, M,
                        pfc:push_pfc_operators(M),
                        pfc:pop_pfc_operators(M)).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Note that we could generalise this to deal with all included files.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

push_pfc_operators(M) :-
    pfc_operators(M, Ops),
    push_operators(M:Ops).

pop_pfc_operators(_) :-
    pop_operators.


user:goal_expansion(In, Out) :-    
    pfc_dialect_expansion(In, Out).



system:term_expansion(In, PosIn, Out, PosOut) :- notrace(In == end_of_file),
   prolog_load_context(dialect, pfc),
   dialect_input_stream_pfc(Stream),
   prolog_load_context(module, M),
   pfc_pop_dialect(Stream,M),!,
   Out = In,
   PosIn = PosOut.
      
      


