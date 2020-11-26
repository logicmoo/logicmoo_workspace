
/* ------------------------------------------------------------------------
 > FILENAME:	mkparser_plcafe
 > PURPOSE:	Compile the Prolog parser to Java using PrologCafe
 > AUTHORS:	Mark A. Greenwood
 > NOTES:	
 ------------------------------------------------------------------------ */

:- assert('$package_name'('shef.nlp.supple.prolog.cafe')).

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

:- compile('dynamic.pl').

:- compile('plcafe_utils.pl').
:- compile('update_name_match.pl').
:- compile('compile_grammar.pl').
:- compile('plcafe_supple_io.pl').
:- compile('parse_file.pl').
:- compile('supple.pl').
:- compile('best_parse.pl').
:- compile('semantics.pl').

parse(Out,Hash,Args) :-
	current_engine(E),
	java_method(E, setDynamicHash(Hash), _),
	java_method(E, setCurrentOutput(Out), _),
	parse(Args).
