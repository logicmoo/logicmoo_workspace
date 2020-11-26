/*************************************************************************

         name: godis-cellphone-text
      version: 
  description: GoDiS cellphone application specification file, text, windows
       author: Anna Olsson and Jessica Villing
 
*************************************************************************/

:-ensure_loaded(app_search_paths).

/*========================================================================
   Select datatypes

   Speficies a list of datatypes to be loaded. Each item DataType in the
   list corresponds to a file DataType.pl in the search path.
========================================================================*/

selected_datatypes([string, move, atom, integer, bool, record, set, stack,
stackset, queue, oqueue, pair, assocset, godis_datatypes]).


/*========================================================================
   Select modules

   Each module spec has the form Predicate:FileName, where Predicate is the
   unary predicate used to call the module, and FileName.pl is the a file
   in the search path containing the module specification
========================================================================*/

selected_modules([ input : input_textscore,
		   interpret : interpret_simple,
%		   interpret : interpret_empty,
		   update : update,
		   select : select,
		   generate : generate_simple,
%		   generate : generate_empty,
		   output : output_simpletext
		 ]).

% dme_module/1 - spefifies which modules have unlimited access to TIS

dme_modules([ update, select ]). 


/*========================================================================
   Select resources

   Speficies a list of resources to be loaded. Each item
   ResourceFile in the list corresponds to a  a file ResourceFile.pl
   in the search path. The file defines a resource object with the same
   name as the file.
========================================================================*/

selected_resources( [
		     lexicon_cellphone_english,
%		     lexicon_cellphone_svenska,
		     domain_cellphone,
		     device_cellphone,
		     database_cellphone
		    ] ).

selected_macro_file( godis_macros ).

/*========================================================================
   operations to execute on TIS reset
========================================================================*/

reset_operations( [ set( program_state, run),
		    set( language, Lang ),
		    set( lexicon, $$dash2underscore(lexicon-Domain-Lang) ),
		    set( devices,
			 record([ cellphone = device_cellphone ]) ),
		    set( domain, $$dash2underscore(domain-Domain) ),
		    push(/private/agenda,greet),
%		    push(/private/agenda,do(vcr_top)),
%		    push( /shared/actions, vcr_top ) ]):-
		    push(/private/agenda,do(top)),
		    push( /shared/actions, top ) ]):-
	flag( language, Lang ),
	flag( domain, Domain ).


/*========================================================================
   Set flags
========================================================================*/

:- setflag(show_rules,yes).
:- setflag(show_state,all).

/*========================================================================
   Run
========================================================================*/

quiet:-
	setflag(show_rules,no),
	setflag(show_state,no).
verb:-
	setflag(show_rules,yes),
	setflag(show_state,all).


run :- run(cellphone-english).

% reload update rules
rur :-
	update:ensure_loaded(library(update_rules)).

rsr :-
	select:ensure_loaded(library(selection_rules)).

:- assert(hide_path('UNLIKELYPAHNAME')).


