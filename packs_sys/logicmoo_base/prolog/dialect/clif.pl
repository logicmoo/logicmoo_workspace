/** <module> CLIF Compatibility module

This  module  provides  compatibility  to   CLIF  through  the  directive
expects_dialect/1:

	==
	:- expects_dialect(clif)
	==

@tbd this module meeds

	* Implement system predicates available in CLIF we do not yet or
	do not wish to support in SWI-Prolog.  Export these predicates.

	* Provide clif_<name>(...) predicates for predicates that exist
	both in CLIF and SWI-Prolog and define goal_expansion/2 rules to
	map calls to these predicates to the clif_<name> version.
	Export these predicates.

	* Alter the library search path, placing dialect/clif *before*
	the system libraries.

	* Allow for =|.clif|= extension as extension for Prolog files.
	If both a =|.pl|= and =|.clif|= is present, the =|.clif|= file
	is loaded if the current environment expects CLIF.

@tbd	The dialect-compatibility packages are developed in a
	`demand-driven' fashion.  Please contribute to this package. Fill it in!
@author Douglas R. Miles

*/

:- module(clif_dialect, [clif_pop_dialect/2, clif_expects_dialect/1, expecting_clif_dialect/0, 
   clif_debug/1, clif_expects_dialect/4,dialect_input_name_clif/1]).
% :- asserta(swish:is_a_module).

:- autoload(library(operators)).


		 /*******************************
		 *	     EXPANSION		*
		 *******************************/

:- multifile
	user:goal_expansion/2,
	user:file_search_path/2,
	user:prolog_file_type/2.
	% clif_dialect_ge/2.
	
:- dynamic
	user:goal_expansion/2,
	user:file_search_path/2,
	user:prolog_file_type/2.

% :- notrace(system:ensure_loaded(library(operators))).


% clif_debug(Info):- ignore(notrace((debug(clif(dialect),'~N% ~p.',[Info])))).
clif_debug(_):-!.
clif_debug(I):- ignore(notrace(clif_debug0(I))).

clif_debug0(state):- !, clif_state.
clif_debug0(X):- format(user_error,'~N% CLIF_DEBUG: ~p.~n',[X]),flush_output(user_error).

show_all_debug_clif(G):- ignore(((G *-> clif_debug0(G);clif_debug0(failed(G))),fail)).


clif_state:-!,
  show_all_debug_clif(prolog_load_context(dialect,_)),
  G = cliftmp:module_dialect_clif(_,_,_,M,_),
  OP = current_op(_,fy,(M:'-')),
  show_all_debug_clif((G,OP)),
  show_all_debug_clif(predicate_property(G,number_of_clauses(_))),
  M = user, show_all_debug_clif((OP)).

:- export(clif_dialect:clif_state/0).
:- system:import(clif_dialect:clif_state/0).



%%	clif_dialect_ge(+In, +Out)
%
%	goal_expansion rules to emulate CLIF behaviour in SWI-Prolog. The
%	expansions  below  maintain  optimization    from   compilation.
%	Defining them as predicates would loose compilation.

/*

clif_dialect_ge(eval_arith(Expr, Result),
	      Result is Expr).

clif_dialect_ge(if(Goal, Then),
	      (Goal *-> Then; true)).
clif_dialect_ge(if(Goal, Then, Else),
	      (Goal *-> Then; Else)).
clif_dialect_ge(style_check(Style),
	      clif_style_check(Style)).
*/
clif_dialect_ge(expects_dialect(Dialect), clif_expects_dialect(Dialect)):- !.
clif_dialect_ge(I,O):- clif_dialect_te(( :- I),( :- O)).

:- module_transparent(clif_dialect_te/2).
clif_dialect_te(I, O):- kif_process_expansion(I,O),!.
clif_dialect_te(I,O):- \+ compound(I),!,as_pfc_expansion(I,O).
clif_dialect_te('=>'(A,B),clif(O)):- I='implies'(A,B), ((fail,as_pfc_expansion(I,O)) -> true ; O=I),!.
clif_dialect_te(I,O):- as_pfc_expansion(I,O).

as_pfc_expansion(I,O):-  
 setup_call_cleanup(current_prolog_flag(emulated_dialect,Was),
  (set_prolog_flag(emulated_dialect,pfc),
   pfc_lib:base_clause_expansion(I,O)),set_prolog_flag(emulated_dialect,Was)).

:- system:module_transparent(clif_dialect:clif_expects_dialect/1).
:- system:import(clif_dialect:clif_expects_dialect/1).
clif_expects_dialect(Dialect):- current_prolog_flag(emulated_dialect,Was),Was==Dialect,!.
clif_expects_dialect(Dialect):-  
 prolog_load_context(module, M),  
 notrace((clif_dialect:(
  prolog_load_context(dialect, Was),
  dialect_input_name_clif(SourceFile),
  clif_debug(clif_expects_dialect(Dialect,SourceFile,Was,M))))),
  clif_expects_dialect(Dialect,SourceFile,Was,M),
  clif_debug(state).

expecting_clif_dialect:- 
 notrace((
  \+ current_prolog_flag(loading_pfc_dialect,true),
  prolog_load_context(dialect, clif),
  prolog_load_context(module, M),
  dialect_input_name_clif(SourceFile),
  cliftmp:module_dialect_clif(clif,SourceFile,_,M,_Undo))).


  %prolog_load_context(dialect, Was)
%  ((Was==clif, Dialect\==clif)-> clif_pop_dialect ; true),
%  expects_dialect(Dialect).

/*
 % current_prolog_flag(emulated_dialect, clif) 
 dumpST,
 wdmsg(expects_dialect(Dialect)),
   fail,
   % in case it is used more than once   
   clif == Dialect -> 
       Out = debug(clif(term_expansion),'~q.',[(expects_dialect(Dialect))])
     ; Out=clif_pop_dialect.
*/

		 /*******************************
		 *	    LIBRARY SETUP	*
		 *******************************/

%	Pushes searching for  dialect/clif  in   front  of  every library
%	directory that contains such as sub-directory.

:-      
   exists_source(library(dialect/clif)) -> true;
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
   absolute_file_name('clif_autoload', Dir,
			       [ file_type(directory),
				 access(read),
                                 relative_to(ThisDir),
				 file_errors(fail)
			       ]),
      asserta((user:file_search_path(library, Dir) :-
	expecting_clif_dialect)).
*/
:- user:file_search_path(clif_library, Dir) -> true;
    ignore((prolog_load_context(directory, ThisDir),
         absolute_file_name('../..', Dir,
			       [ file_type(directory),
				 access(read),
                                 relative_to(ThisDir),
				 file_errors(fail)
			       ]),
	    asserta((user:file_search_path(clif_library, Dir))))).



%%	push_clif_file_extension
%
%	Looks for .clif files before looking for .pl files if the current
%	dialect is =clif=.

push_clif_file_extension :-
	asserta((user:prolog_file_type(clif, prolog) :- expecting_clif_dialect)).


:- push_clif_file_extension.


:- multifile
	prolog:message//1.

prolog:message(clif_unsupported(Goal)) -->
	[ 'CLIF emulation (clif.pl): unsupported: ~p'-[Goal] ].


:- use_module(library(pengines),[pengine_self/1]). 

calc_load_module_clif(OM):- pengine_self(OM),!.
calc_load_module_clif(OM):- 
     '$current_typein_module'(TM), 
     prolog_load_context(module,Load),strip_module(_,Strip,_),
     context_module(Ctx),'$current_source_module'(SM),
     ((SM==Load,SM\==user)-> M = SM ;
     ((TM\==Load,TM\==user) -> M = TM ; (M = SM))),
     OM=Load,
     clif_debug([ti=TM,load=Load,strip=Strip,ctx=Ctx,sm=SM,clif=M,using=OM]),!.     

calc_load_module_clif(M):- 
    (member(Call,[
     prolog_load_context(module,M),
     pengine_self(M),
     '$current_source_module'(M),
     '$current_typein_module'(M),
     strip_module(_,M,_),
     context_module(M),
     source_location(M,_)]),
    call(Call),
    clif_debug(calc_load_module_clif(Call)),
    \+ likely_reserved_module(M)).
calc_load_module_clif(M):- M==baseKB. % clif_dialect:clif_program_module(M).

likely_reserved_module(M):- M=user; 
  module_property(M,P), member(P,[class(library),class(system),exported_operators([_|_]),exports([_|_])]).




    :- volatile(cliftmp:module_dialect_clif/5).
:- thread_local(cliftmp:module_dialect_clif/5).


:- system:module_transparent(prolog_dialect:expects_dialect/1). 
:- system:module_transparent(clif:setup_dialect/0). 
clif:setup_dialect:- clif_expects_dialect(clif).
   

clif_operators(M,[ 
 op(1199,fx,M:('==>')),
 op(1190,xfx,M:('::::')),
 op(1180,xfx,M:('==>')),
 op(1170,xfx,M:('<==>')),
 op(1160,xfx,M:('<-')),
 op(1150,xfx,M:('=>')),
 op(1140,xfx,M:('<=')),
 op(1130,xfx,M:('<=>')),
 op(1120,xfx,M:('<->')),
 op(600,yfx,M:('&')),
 op(600,yfx,M:('v')),
 op(350,xfx,M:('xor')),
 op(300,fx,M:('-')),
 op(300,fx,M:('~'))]).

other_dialect(Dialect):- Dialect\==clif.

:- system:module_transparent(clif_dialect:clif_expects_dialect/4).
:- system:import(clif_dialect:clif_expects_dialect/4).
clif_expects_dialect(SWI,SourceFile,_,M):- other_dialect(SWI),!,clif_pop_dialect(SourceFile,M), expects_dialect(SWI).
%clif_expects_dialect(SWI,_,Bin,_):- other_dialect(SWI),other_dialect(Bin),!, expects_dialect(SWI).
%clif_expects_dialect(WAS,_,WAS,_):- WAS==clif, 
%clif_expects_dialect(WAS,_,WAS,_):- !. % expects_dialect(WAS).
%clif_expects_dialect(Next,SourceFile,Was,M):- cliftmp:module_dialect_clif(Next,SourceFile,Was,M,_Undo), !.
clif_expects_dialect(Next,StreamNow,Was,M):- 
   cliftmp:module_dialect_clif(Next,StreamBefore,Was,M,_Undo),
   StreamNow \== StreamBefore,!,
   retract(cliftmp:module_dialect_clif(Next,StreamBefore,Was,M,Undo)),   
   asserta(cliftmp:module_dialect_clif(Next,StreamNow,Was,M,Undo)),!.
clif_expects_dialect(clif,SourceFile,Was,M):-
   %notrace(M:ensure_loaded(library(clif_lib))),
   %ignore(retract(cliftmp:module_dialect_clif(Dialect,SourceFile,_,_,_))), 
   M:use_module(library(dialect/clif)),  
   M:use_module(library(logicmoo_clif)),
   clif_operators(M, Ops),
   push_operators(M:Ops, Undo),
   asserta(cliftmp:module_dialect_clif(clif,SourceFile,Was,M,Undo)),!.


dialect_input_name_clif(SourceFile):-   
  prolog_load_context(source,SourceFile)->true; source_location(SourceFile) -> true ; SourceFile = user_input.

:- system:module_transparent(clif_dialect:clif_pop_dialect/2).
:- system:import(clif_dialect:clif_pop_dialect/2).
clif_pop_dialect(SourceFile,M):-
    retract(cliftmp:module_dialect_clif(clif,SourceFile,Was,M,Undo)),!,
    %print_message(warning, format('~q', [warn_pop_clif_dialect_fallback(SourceFile,M->Was)])),
    clif_debug(pop_clif_dialect2(SourceFile,M->Was)),
    pop_operators(Undo),    
    %nop('$set_source_module'(Was)),!,
    clif_debug(state).
clif_pop_dialect(SourceFile,M):- 
   clif_debug(print_message(warning, format('~q', [missing_pop_clif_dialect_fallback(SourceFile,M)]))),
   clif_debug(state).


                 /*******************************
                 *         SYNTAX HOOKS         *
                 *******************************/

:- multifile
    prolog:alternate_syntax/4.


prolog:alternate_syntax(clif, M,
                        clif_dialect:push_clif_operators(M),
                        clif_dialect:pop_clif_operators(M)).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Note that we could generalise this to deal with all included files.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

push_clif_operators(M) :-
    clif_operators(M, Ops),
    push_operators(M:Ops).

pop_clif_operators(_) :-
    pop_operators.

expanding_at_file(In):- prolog_load_context(term,TermIn), strip_module(In,_,A),strip_module(TermIn,_,B),!,A=@=B.

:- multifile(system:goal_expansion/2).
:- module_transparent(system:goal_expansion/2).
:- system:import(clif_dialect_ge/2).
:- system:import(expanding_at_file/2).
system:goal_expansion(In, Out) :- notrace(expecting_clif_dialect),
  clif_dialect_ge(In, Out), In\=@=Out.

system:term_expansion(In, P, Out, PO) :- 
 notrace(expecting_clif_dialect),
 In\==end_of_file,In\==begin_of_file,
 %(tracing->break;true),
 expanding_at_file(In),
 nonvar(P),
 clif_dialect_te(In, Out), In\=@=Out,
 P = PO.

term_expansion_clif_eof(M):-   
   expecting_clif_dialect,
   prolog_load_context(file, SourceFile),
   cliftmp:module_dialect_clif(clif,SourceFile,_,M,_Undo),
   clif_pop_dialect(SourceFile,M),!.

:- multifile(system:term_expansion/2).
:- module_transparent(system:term_expansion/2).
:- system:import(term_expansion_clif_eof/1).

system:term_expansion(MIn, _Out):- 
   notrace(expecting_clif_dialect),
   notrace(strip_module(MIn,MM,In)),
   notrace(In == end_of_file),
   (MIn==In->prolog_load_context(module, M);MM=M),
   term_expansion_clif_eof(M),
   fail.

