/*************************************************************************

         name: godis-icecream-text
      version: 
  description: GoDiS Icecream application specification file, text, windows
       author: Staffan Larsson
 
*************************************************************************/



/*========================================================================
   Select datatypes

   Speficies a list of datatypes to be loaded. Each item DataType in the
   list corresponds to a file DataType.pl in the search path.
========================================================================*/

selected_datatypes([string, move, atom, integer, bool, record, set, stack,
stackset, queue, oqueue, pair, assocset]).


/*========================================================================
   Select modules

   Each module spec has the form Predicate:FileName, where Predicate is the
   unary predicate used to call the module, and FileName.pl is the a file
   in the search path containing the module specification
========================================================================*/

selected_modules([ input : input_simpletext,
		   interpret : interpret_empty,
		   update : update,
		   select : select,
		   generate : generate_empty,
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
		     domain_icecream
		    ] ).

selected_macro_file( godis_macros ).

/*========================================================================
   operations to execute on TIS reset
========================================================================*/

reset_operations( [ set( program_state, run),
		    set( language, Lang ),
		    set( lexicon, $$dash2underscore(lexicon-Domain-Lang) ),
		    set( domain, $$dash2underscore(domain-Domain) ),
		    push(/private/agenda,greet),
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

go( Domain-Language ):-
	setflag(domain, Domain),
	setflag(language, Language),
	start_trindikit.


go :- go(vcr-english).

% reload update rules
rur :-
	update:ensure_loaded(library(update_rules)).

rsr :-
	select:ensure_loaded(library(selection_rules)).

:- assert(hide_path('UNLIKELYPAHNAME')).




%%% APPLY CHANGES BELOW %%%

% Select appropriate godis library/libraries
%:- assertz(user:library_directory(godis('godis-aod'))).
:- assertz(user:library_directory('/users/gslt/sl/godis/dist/prolog/godis/godis-nd')).
:- assertz(user:library_directory('/users/gslt/sl/godis/dist/prolog/godis/general')).

% Set the application home path
:- assertz(user:file_search_path(home,'/users/gslt/sl/godis/godis-apps/domain-icecream')).
:- assertz(user:library_directory(home(''))).
% Add any subdirectories to the application home directory below
:- assertz(user:library_directory(home('Resources'))).

% GODIS spec filename here; initialize system
%:- init( 'godis-vcr-text' ).

