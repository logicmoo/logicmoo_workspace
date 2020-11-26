
% prolog_xml.pl

:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(prolog_xml,
	  [prolog_xml/2,
	   safe_prolog_xml/2]
	 ).

%------------------------------------------------------------------------------------

:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).
:- use_module(library(xml)).

%------------------------------------------------------------------------------------

/*

prolog_xml(?Prolog, ?XMLString)

Convert Prolog term into XML format, or vice versa. Useful for passing data between Prolog
and non-Prolog processes.

Example:

| ?- Term = foo(a,b,[c, d, 6, p=q, some_vars(F, F, G)]), prolog_xml(Term, XML), format('~N~s~n~n', [XML]), prolog_xml(Term1, XML), format('~N~q~n~n', [Term1]), fail.

<term>
 <functor>foo</functor>
 <atom>a</atom>
 <atom>b</atom>
 <list>
  <atom>c</atom>
  <atom>d</atom>
  <number>6</number>
  <term>
   <functor>=</functor>
   <atom>p</atom>
   <atom>q</atom>
  </term>
  <term>
   <functor>some_vars</functor>
   <var>_514</var>
   <var>_514</var>
   <var>_554</var>
  </term>
 </list>
</term>

foo(a,b,[c,d,6,p=q,some_vars(_9078,_9078,_9108)])

*/

safe_prolog_xml(Prolog, XMLString) :-
	on_exception(
	Exception, 
	prolog_xml(Prolog, XMLString),
	handle_exception_in_prolog_xml(Exception)
    ),
	!.

handle_exception_in_prolog_xml(Exception) :-
	format('~N~n*** Exception in call to prolog_xml/2 ***~n~n', []),
	print_message(error, Exception),
	fail.

%------------------------------------------------------------------------------------

prolog_xml(Prolog, XMLString) :-
	nonvar(Prolog),
	prolog_to_xml_term(Prolog, XMLTerm),
	WrappedXMLTerm = xml([], XMLTerm),
	(   xml_parse(XMLString, WrappedXMLTerm) ->
	    true
	;
	    otherwise ->
	    format('~N*** Error: unable to convert to XML: ~n~q~n', [WrappedXMLTerm]),
	    fail
	),  
	!.
prolog_xml(Prolog, XMLString) :-
	is_prolog_string(XMLString),
	(   xml_parse(XMLString, XMLParseResult) ->
	    true
	;
	    otherwise ->
	    format('~N*** Error: unable to parse XML: ~n~s~n', [XMLString]),
	    fail
	),
	XMLParseResult = xml(_Attributes, XMLBody),
	(   XMLBody = [SingleXMLTerm] ->
	    xml_term_to_prolog(SingleXMLTerm, Prolog)
	;
	    is_list(XMLBody) ->
	    xml_term_list_to_prolog_list(XMLTerm, Prolog)
	;
	    otherwise ->
	    xml_term_to_prolog(XMLTerm, Prolog)
	),
	!.

%------------------------------------------------------------------------------------

/*

document ::= xml(attributes, content) { well-formed document } |
malformed(attributes, content) { malformed document }

attributes ::= [] | [name=chardata | attributes]

content ::= [] | [cterm | content]

cterm ::= pcdata(char-data) { text } |
comment(char-data) { an XML comment } |
namespace(URI,prefix,element) { a Namespace } |
element(tag, attributes, content) { <tag>..</tag> encloses content or <tag /> if empty } |
instructions(name,chardata) { A PI <? name char-data ?> } |
cdata(char-data) { <![CDATA[char-data]]> } |
doctype(tag,doctype-id) { DTD <!DOCTYPE .. > } |
unparsed(char-data) { text that hasn't been parsed } |
out_of_context(tag) { tag is not closed }

tag ::= atom { naming an element }

name ::= atom { not naming an element }

URI ::= atom { giving the URI of a namespace }

char-data ::= code-list

doctype-id ::= public(char-data,chardata) |
public(char-data,dtdliterals) |
system(char-data) |
system(char-data,dtdliterals) |
local |
local,dtd-literals

dtd-literals ::= [] |
[dtd_literal(chardata) | dtd-literals]
*/

prolog_to_xml_term(Var, element(var, [], [pcdata(Codes)])) :-
	var(Var),
	format_to_atom('~w', [Var], Atom),
	atom_codes(Atom, Codes),
	!.
prolog_to_xml_term(Atom, element(atom, [], [pcdata(Codes)])) :-
	atom(Atom),
	atom_codes(Atom, Codes),
	!.
prolog_to_xml_term(N, element(number, [], [pcdata(Codes)])) :-
	number(N),
	number_codes(N, Codes),
	!.
% This can cause problems with non-string lists of numbers
%prolog_to_xml_term(String, element(string, [], [pcdata(String)])) :-
%	is_prolog_string(String),
%	!.
prolog_to_xml_term(List, element(list, [], XMLList)) :-
	is_list(List),
	prolog_list_to_xml_term_list(List, XMLList),
	!.
prolog_to_xml_term(Term, element(term, [], [XMLFunctor | XMLArgs])) :-
	compound(Term),
	Term =.. [Functor | Args],
	atom_codes(Functor, FunctorCodes),
	XMLFunctor = element(functor, [], [pcdata(FunctorCodes)]),
	prolog_list_to_xml_term_list(Args, XMLArgs),
	!.
prolog_to_xml_term(Prolog, XMLTerm) :-
	format('~N*** Error: bad call: ~w~n', [prolog_to_xml_term(Prolog, XMLTerm)]),
	fail.

prolog_list_to_xml_term_list([], []).
prolog_list_to_xml_term_list([F | R], [F1 | R1]) :-
	prolog_to_xml_term(F, F1),
	!,
	prolog_list_to_xml_term_list(R, R1).
prolog_list_to_xml_term_list(Args, XMLArgs) :-
	format('~N*** Error: bad call: ~w~n', [prolog_list_to_xml_term_list(Args, XMLArgs)]),
	fail.

%------------------------------------------------------------------------------------

xml_term_to_prolog(XMLTerm, Prolog) :-
	xml_term_to_prolog(XMLTerm, Prolog, []-_VarAssocFinal).

xml_term_list_to_prolog_list(XMLTerm, Prolog) :-
	xml_term_list_to_prolog_list(XMLTerm, Prolog, []-_VarAssocFinal).

xml_term_to_prolog(element(var, [], [pcdata(Codes)]), Var, VarAssocIn-VarAssocOut) :-
	atom_codes(VarName, Codes),
	(   member(VarName-Var, VarAssocIn) ->
	    VarAssocOut = VarAssocIn
	;
	    otherwise ->
	    VarAssocOut = [VarName-Var | VarAssocIn]
	),
	!.
xml_term_to_prolog(element(atom, [], [pcdata(Codes)]), Atom, VarAssocIn-VarAssocIn) :-
	atom_codes(Atom, Codes),
	!.
xml_term_to_prolog(element(string, [], [pcdata(String)]), String, VarAssocIn-VarAssocIn) :-
	!.
xml_term_to_prolog(element(number, [], [pcdata(Codes)]), N, VarAssocIn-VarAssocIn) :-
	number_codes(N, Codes),
	!.
xml_term_to_prolog(element(list, [], XMLList), List, VarAssocIn-VarAssocOut) :-
	xml_term_list_to_prolog_list(XMLList, List, VarAssocIn-VarAssocOut),
	!.
xml_term_to_prolog(element(term, [], [XMLFunctor | XMLList]), Term, VarAssocIn-VarAssocOut) :-
	XMLFunctor = element(functor, [], [pcdata(FunctorCodes)]),
	atom_codes(Functor, FunctorCodes),
	xml_term_list_to_prolog_list(XMLList, List, VarAssocIn-VarAssocOut),
	Term =.. [Functor | List],
	!.
xml_term_to_prolog(XMLTerm, Prolog, VarAssoc) :-
	format('~N*** Error: bad call: ~w~n', [xml_term_to_prolog(XMLTerm, Prolog, VarAssoc)]),
	fail.

xml_term_list_to_prolog_list([], [], VarAssocIn-VarAssocIn) :-
	!.
xml_term_list_to_prolog_list([F | R], [F1 | R1], VarAssocIn-VarAssocOut) :-
	xml_term_to_prolog(F, F1, VarAssocIn-VarAssocNext),
	!,
	xml_term_list_to_prolog_list(R, R1, VarAssocNext-VarAssocOut).
xml_term_list_to_prolog_list(XMLList, List, VarAssoc) :-
	format('~N*** Error: bad call: ~w~n', [xml_term_list_to_prolog_list(XMLList, List, VarAssoc)]),
	fail.
