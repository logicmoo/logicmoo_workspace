
:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- use_module('$REGULUS/PrologLib/prolog_json_compact').
:- use_module('$REGULUS/PrologLib/prolog_xml_compact').
:- use_module('$REGULUS/PrologLib/utilities').

test_json_prolog_xml(JSONStrAtom) :-
	atom_codes(JSONStrAtom, JSONStr),
	prolog_json(Prolog, JSONStr),
	prolog_xml(Prolog, XMLString),
	format('~N~nJSON:~n~n"~s"~n', [JSONStr]),
	format('~N~nXML:~n~n"~s"~n', [XMLString]),
	format('~N~nProlog:~n~n', []),
	prettyprint(Prolog).


	
