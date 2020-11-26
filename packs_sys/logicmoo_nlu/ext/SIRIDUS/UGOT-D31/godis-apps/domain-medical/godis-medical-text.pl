/*************************************************************************

         name: godis-medical-text
      version: 
  description: Specification file for GoDiS-AOD medical application
       author: Staffan Larsson
 
*************************************************************************/


/*========================================================================
   Select datatypes

   Speficies a list of datatypes to be loaded. Each item DataType in the
   list corresponds to a file DataType.pl in the search path.
========================================================================*/

selected_datatypes([string, move, atom, integer, real, bool, record,
set, stack, stackset, queue, oqueue, pair, godis_datatypes]).


/*========================================================================
   Select modules

   Each module spec has the form Predicate:FileName, where Predicate is the
   unary predicate used to call the module, and FileName.pl is the a file
   in the search path containing the module specification
========================================================================*/

selected_modules([ input : input_textscore,
		   %input:input_simpletext,
		   interpret: interpret_simple,
		   %interpret: interpret_empty,
		   update : update,
		   select : select,
		   generate : generate_simple,
		   %generate : generate_empty,
		   output:output_simpletext
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
		     device_medical,
		     lexicon_medical_english,
		     domain_medical
		    ] ).

selected_macro_file( godis_macros ).

batch_files([]).

/*========================================================================
   operations to execute on TIS reset
========================================================================*/

reset_operations( [ set( program_state, run),
		    set( language, Lang ),
		    set( lexicon, $$dash2underscore(lexicon-Domain-Lang) ),
		    set( devices,
			 record([medical=device_medical]) ),
		    set( domain, $$dash2underscore(domain-Domain) ),
		    push(/private/agenda,greet),
		    push(/private/agenda,raise(X^symptom(X))),
		    push(/private/agenda,respond(X^disease(X))),
%		    push(/shared/issues, X^info(X) ),
		    push(/shared/issues, confirmed_by_tests ),
		    push(/shared/issues, confirmed_by_interview ),
		    push(/shared/issues,X^disease(X)),
%		    push( /shared/actions, top ),
		    score := 1.0 % default
		  ]):-
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

run( Domain-Language ):-
	setflag(domain, Domain),
	setflag(language, Language),
	start_trindikit.


run :- run(vcr-english).

% reload update rules
rur :-
	update:ensure_loaded(library(update_rules)).

rsr :-
	select:ensure_loaded(library(selection_rules)).

:- assert(hide_path('UNLIKELYPAHNAME')).

