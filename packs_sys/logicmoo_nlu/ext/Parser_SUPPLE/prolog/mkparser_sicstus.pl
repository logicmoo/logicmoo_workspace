
/* ------------------------------------------------------------------------
 > FILENAME:	mkparser_sicstus
 > PURPOSE:	create and dump saved state ready to process command
 >              line when restored 
 > AUTHORS:	Kevin Humphreys, Hamish Cunningham
 > NOTES:	
 ------------------------------------------------------------------------ */

cvsid_mkparser_sicstus("$Id: mkparser_sicstus.pl 9628 2008-05-09 14:50:14Z ian_roberts $").


% COMMAND LINE OPTIONS

/*
parse [-v]                be verbose
      [-d]                 show debugging info
      [-level n]          parse up to level n grammar & don't do semantics
      [-m markup_file]    write NE markup to markup_file
      [-p parse_file]     write best parses to parse_file in tree form
      [-b parse_file]     write best parses to parse_file in bracketed form
      [-c chart_file]     write chart to chart_file
      [-o output_file]    write gdm output to output_file instead of stdout
      file

NOTE: -n reserved for mkparser script (it triggers assertion of
      non_interactive/0, which triggers saving of state and halting).

*/

:- dynamic verbose/0, chart/3, grammar_file/1, filter_grammar/1,
	output_file/1, chart_file/1, markup_file/1, 
	best_parse_file/1, bracketed_parses/0.

:- prolog_flag(redefine_warnings,_,off).
:- prolog_flag(single_var_warnings,_,off).

% compile either sicstus3_utils or sicstus4_utils, depending on SICStus major
% version number.  The 'version' prolog_flag on SICStus starts 'SICStus
% major.minor.patch', we extract the major version and condition on that.
:- (prolog_flag(version, V),
    atom_concat('SICStus ', Vtail, V),
    atom_codes(Vtail, [Major|_]),
    number_codes(Major, [MajorVersion]),
    MajorVersion =< 3,
    compile('sicstus3_utils.pl'))
   ; compile('sicstus4_utils.pl').

:- dynamic non_interactive/0.
?- ((prolog_flag(argv,A,A), member('-n', A), assert(non_interactive)) 
   % replace compile with consult if interactive
   ; prolog_flag(compiling,_,debugcode)).


%:- use_module(library(gauge)), prolog_flag(compiling,_,profiledcode).

:- compile('compile_grammar.pl').
:- compile('parse_file.pl').
:- compile('supple.pl').
:- compile('supple_io.pl').
:- compile('best_parse.pl').
:- compile('semantics.pl').

% load and compile default set of grammars
:- consult('grammar/load.pl').

% top-level control
parse :-
    prolog_flag(argv,Args,Args),
    on_exception(Error,parse(Args),(write(Error),nl)),
    flush_output, halt(0).


% compile any grammars on cmd line
?- non_interactive, prolog_flag(argv,Args,Args), parse(Args).


:- prolog_flag(redefine_warnings,_,on).


% save current state, ready to restart with parse/0.
?- non_interactive, save_program('supple.sicstus', parse), flush_output, halt(0).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% $Log$
% Revision 1.1  2005/12/05 16:32:02  ian_roberts
% SUPPLE - the Sheffield University Prolog Parser for Language Engineering.
%
% SUPPLE replaces the old Buchart plugin, which should now be considered
% deprecated.
%
% Revision 1.1.1.1  2005/10/06 10:02:05  saggion
% Supple Project
%
% Revision 1.1.1.1  2004/11/25 08:17:49  saggion
% Buchart-Gate Integration
%
% Revision 1.17  1998/04/04 22:19:16  kwh
% syntax error
%
% Revision 1.16  1998/04/04 22:01:27  kwh
% exit on error
%
% Revision 1.15  1998/03/25 18:51:46  kwh
% affixes passed into chart
%
% Revision 1.14  1998/02/20 16:07:33  kwh
% sicstus3 changes
%
% Revision 1.13  1997/11/28 14:57:04  kwh
% use single load file to specify grammars
%
% Revision 1.12  1997/09/30 17:24:43  kwh
% merge buchart_cascade changes back in
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

