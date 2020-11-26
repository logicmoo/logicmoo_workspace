
 
/*************************************************************************

         name: imdis.pl 
      version: 
  description: Imdis specification file
       author: Staffan Larsson
 
*************************************************************************/


/*========================================================================
     File Search Paths
========================================================================*/

%:- assertz(user:file_search_path(home, '$HOME')).

:- assertz(user:file_search_path(home, '/users/ling/sl')).

% search path for trindikit infrastructure, datatypes, and modules

:- assertz(user:library_directory(home('Jobb/TRINDI/TRINDIKIT/OLD/trindikit1.2'))).
:- assertz(user:library_directory(home('Jobb/TRINDI/TRINDIKIT/OLD/trindikit1.2/Datatypes'))).
:- assertz(user:library_directory(home('Jobb/TRINDI/TRINDIKIT/OLD/trindikit1.2/Modules'))).
:- assertz(user:library_directory(home('Jobb/TRINDI/IMDiS'))).

/*========================================================================
     Misc.
========================================================================*/

:- op( 850, xfx, '::' ).

:- ensure_loaded( library(lists) ).

/*========================================================================
   Select datatypes

   Speficies a list of datatypes to be loaded. Each item DataType in the
   list corresponds to a file DataType.pl in the search path.
========================================================================*/

selected_datatypes([standard, record, set, stack, stackset, assocset, imdis_datatypes]).


/*========================================================================
   Select modules

   Each module spec has the form Predicate:FileName, where Predicate is the
   unary predicate used to call the module, and FileName.pl is the a file
   in the search path containing the module specification
========================================================================*/

selected_modules([ control: control,
		   input : input_simpletext,
		   interpret : interpret_simple1,
		   update : update,
		   select : select,
		   generate : generate_simple1,
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

selected_resources( [lexicon_homecentre_english, domain_homecentre] ).


/*========================================================================
   Select macro file

   MacroFile.pl is the a file in the search path containing the macro
   definitions
   
========================================================================*/

selected_macro_file( imdis_macros ).
 
/*========================================================================
   Select type of resource interface: datatypes or predicates
========================================================================*/

% resource_interface( datatypes ).

/*========================================================================
   operations to execute on TIS reset
========================================================================*/

reset_operations( [ set(program_state, run),
		    set( lexicon, lexicon-Domain-Lang ),
		    set( domain, domain-Domain ),
		    pushRec(private^agenda,raise(X^(task(X)))),
		    pushRec(private^agenda,greet)]):-
	flag( language, Lang ),
	flag( domain, Domain ).


/*========================================================================
   Load the TRINDIKIT, including selected modules and datatypes

   assumes the following:
   
   - the trindikit.pl file is available in the search path, in a directory
     which contains the complete and unaltered trindikit
     
   - selected_modules/1 and selected_datatypes/1 are defined in module user
   
   - all selected modules and datatypes are in the search path
   
   - for each datatype D, there is a file D.pl containing the datatype
     definition, and a file print_D.pl defining print_D/1, in the search path
     
   - for any modules requiring algorithm specifications, these specs are
     in the search path and have the name specified in the module definition

   - there is (may be) a file user_flags.pl in the search path speficying additional
     flags
     
   - there is a file resource_interfaces.pl in the search path, which imports
     resources and defines resource interfaces as typed objects or
     simple predicates

   - there is a (possibly empty) file macros.pl in the search path
   
========================================================================*/

:- ensure_loaded( library(trindikit) ).

/*========================================================================
   Set flags
========================================================================*/

:- setflag(version,standalone).
:- setflag(show_rules,yes).
:- setflag(show_state,no).



/*========================================================================
   Run
========================================================================*/



run :-
	setflag( domain, homecentre ),
	setflag( language, english ),
	reset,
	control.

