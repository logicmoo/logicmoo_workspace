
/* ------------------------------------------------------------------------
 > FILENAME:	mkparser_swi
 > PURPOSE:	top level loader for SWI-Prolog
 > AUTHORS:	Kevin Humphreys, Hamish Cunningham
 > NOTES:	
 ------------------------------------------------------------------------ */

cvsid_mkparser_swi("$Id: mkparser_swi.pl 7085 2005-12-05 16:32:03Z ian_roberts $").


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
*/

:- dynamic verbose/0, chart/3, grammar_file/1, filter_grammar/1,
	output_file/1, chart_file/1, markup_file/1, 
	best_parse_file/1, bracketed_parses/0.

:- style_check(-singleton).

:- consult('swi_utils.pl').


:- consult('compile_grammar.pl').
:- consult('parse_file.pl').
:- consult('supple.pl').
:- consult('supple_io.pl').
:- consult('best_parse.pl').
:- consult('semantics.pl').

% load and compile default set of grammars
:- consult('grammar/load.pl').

% top-level control
parse :- unix(argv([_swi,_x,_code|Args])), parse(Args), halt.


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
% Revision 1.9  1997/12/17 11:57:55  huyck
% Makefile changes to incorporate swi.
%
% Revision 1.8  1997/11/28  14:57:06  kwh
% use single load file to specify grammars
%
% Revision 1.7  1997/11/18 18:43:38  kwh
% run aircraft grammar at end of ne
%
% Revision 1.6  1997/10/13 11:24:11  kwh
% load grammars here instead of from Makefile
%
% Revision 1.5  1997/09/30 17:24:45  kwh
% merge buchart_cascade changes back in
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

