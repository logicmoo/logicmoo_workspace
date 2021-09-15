%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% Copyright (C) 2019 Christoph Wernhard
%%%%
%%%% This file is part of PIE.
%%%%
%%%% PIE is free software: you can redistribute it and/or modify
%%%% it under the terms of the GNU General Public License as published by
%%%% the Free Software Foundation, either version 3 of the License, or
%%%% (at your option) any later version.
%%%% 
%%%% PIE is distributed in the hope that it will be useful,
%%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%% GNU General Public License for more details.
%%%% 
%%%% You should have received a copy of the GNU General Public License
%%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(prooftrans, [prover9_proofs/2,
		       p9p_sgraph/2]).

:- use_module(prover9io).
:- use_module(library(sgml)).
:- use_module(swilib(err)).
:- use_module(folelim(logop_fol)).
:- use_module(folelim(prettysymbols_fol)).
:- use_module(swilib(tempfiles)).
:- use_module(swilib(fromonto)).

%%%% 
%%%% Returns a Prolog representation of Prover9 proofs. Takes the xml files as
%%%% input, as obtained e.g. with:
%%%% 
%%%%  $ prooftrans xml renumber xml -f prover9_output >tmp.xml
%%%%  $ prooftrans xml expand renumber xml -f prover9_output >tmp.xml
%%%% 
%%%% <PROOF>   := proof(LIST OF <CLAUSE>)
%%%% <CLAUSE>  := clause(<ID>,
%%%%                     <TYPE>,
%%%%                     LIST OF <FORMULA> | LIST OF <LITERAL>,
%%%%                     LIST OF <ID>)   % parents
%%%% <ID>      := <NUMBER>
%%%% <FORMULA> := NF format, quantified, with atoms as variables
%%%% <LITERAL> := NF format, with Prolog variables
%%%%
prover9_proofs(XMLFile, Proofs) :-
	prooftrans_dtd(DTD),
	load_structure(XMLFile, X, [dtd(DTD), dialect(xml), space(remove)]),
	memberchk(element(proofs, _, E), X),
	findall(P, ( member(E1, E),
		     E1 = element(proof, _, _),
		     convert_p9_proof(E1, P) ),
		Proofs).

convert_p9_proof( element(proof, _, E), proof(Cs) ) :-
	findall(C, ( member(E1, E),
		     E1 = element(clause, _, _),
		     convert_p9_clause(E1, C) ),
		Cs1),
	align_vars(Cs1, _, Cs).

align_vars([C|Cs], Vs, [C1|Cs1]) :-
	copy_term(C, C1),
	term_variables(C1, Vs1),
	set_vars(Vs1, Vs),
	align_vars(Cs, Vs, Cs1).
align_vars([], _, []).

set_vars([V|Vs], [V|Vs1]) :-
	set_vars(Vs, Vs1).
set_vars([], _).
	
convert_p9_clause( element(clause, A, E), clause(Id, Type, Lits, ParentIds) ) :-
	( memberchk(id=Id1, A) ->
	  atom_number(Id1, Id)
	; err('Clause without id ~q', [element(clause, A, E)] )
	),
	( memberchk(type=Type, A) -> true ; Type=std ),
	findall(L, ( member(E1, E),
		     E1 = element(literal, _, [L]) ),
		RawLits ),
	concat_atom(RawLits, ' | ', RawClause),
	( Type = assumption ->
	  prover9_fol_read_from_chars(RawClause, Form),
	  c9n(Form, Form1),
	  logform_clean_vars_to_symbols(Form1, Form2),
	  ( ground(Form2) ->
	    logform_vars_to_pretty_symbols(Form2, Form3)
	  ; Form3 = Form2
	  ),
	  Lits = [Form3]
	; prover9_read_from_chars(RawClause, Form),
	  c9n(Form, Form1),
	  disjunction_to_list(Form1, Lits)
	),
	findall(Par, ( member(E1, E),
		       E1 = element(justification, _, E2),
		       member(E3, E2),
		       E3 = element(_, AJ, _),
		       member(parents=Par, AJ)
		     ),
		Pars),
	concat_atom(Pars, ' ', Pars1),
	concat_atom(Pars2, ' ', Pars1),
	findall(Par1, ( member(Par2, Pars2),
			atom_number(Par2, Par1) ),
		Pars3),
	sort(Pars3, ParentIds).
		
c9n(all:(X:F), all(X, G)) :- !, c9n(F, G).
c9n(exists:(X:F), ex(X, G)) :- !, c9n(F, G).
c9n('&'(F1,F2), (G1,G2)) :- !, c9n(F1,G1), c9n(F2,G2).
c9n('|'(F1,F2), (G1;G2)) :- !, c9n(F1,G1), c9n(F2,G2).
c9n('->'(F1,F2), (G1 -> G2)) :- !, c9n(F1,G1), c9n(F2,G2).
c9n('<-'(F1,F2), (G1 <- G2)) :- !, c9n(F1,G1), c9n(F2,G2).
c9n('<->'(F1,F2), (G1 <-> G2)) :- !, c9n(F1,G1), c9n(F2,G2).
c9n('-'(F), ~G) :- !, c9n(F,G).
c9n('!='(A,B), ~(A=B)) :- !.
c9n('$T', true) :- !.
c9n('$F', false) :- !.
c9n(F, F).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% 
%%%% The output can be used with graphs.pl (e.g. warshall)
%%%% 
p9p_sgraph(proof(Clauses), SGraph) :-
	findall(Id-Ids, member(clause(Id, _, _, Ids), Clauses), SGraph).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic prooftrans_dtd_1/1.

prooftrans_dtd(DTD) :-
	prooftrans_dtd_1(DTD),
	!.
prooftrans_dtd(DTD) :-
	init_prooftrans_dtd,
	prooftrans_dtd_1(DTD).

init_prooftrans_dtd :-
	retractall( prooftrans_dtd_1(_) ),
	tempfile(prooftrans, dtd, DTDFile),
	proof3_dtd_content(DTDContent),
	info(10, 'Initializing prooftrans DTD'),
	onto_file(write(DTDContent), DTDFile),
	new_dtd(proof, DTD),
	load_dtd(DTD, DTDFile),
	delete_file(DTDFile),
	assert( prooftrans_dtd_1(DTD) ).

%%%% 
%%%% From https://www.cs.unm.edu/~mccune/mace4/manual/2009-11A/proof3.dtd.
%%%% Comments stripped. Seemingly needed to read the xml file without
%%%% annoying (but harmless) error message.
%%%%
proof3_dtd_content(
'<?xml version="1.0" encoding="ISO-8859-1"?>

<!ELEMENT proofs (source?,heading?,proof*)>
  <!ATTLIST proofs number_of_proofs CDATA #IMPLIED>
  <!ELEMENT source       (#PCDATA)>
  <!ELEMENT heading       (#PCDATA)>
  <!ELEMENT proof (comments?,clause+)>
    <!ATTLIST proof    number CDATA #IMPLIED
                       length CDATA #IMPLIED
                       max_count CDATA #IMPLIED
                       seconds CDATA #IMPLIED>
  
      <!ELEMENT comments       (#PCDATA)>
      <!ELEMENT clause (literal+,attribute*,justification?)>
        <!ATTLIST clause    id CDATA #IMPLIED type CDATA #IMPLIED>
          <!ELEMENT literal       (#PCDATA)>
          <!ELEMENT attribute     (#PCDATA)>
      
          <!ELEMENT justification (j1,j2*)>
            <!ATTLIST justification jstring CDATA #REQUIRED>
              <!ELEMENT j1 EMPTY>
                <!ATTLIST j1 rule CDATA #REQUIRED parents CDATA #IMPLIED>
              <!ELEMENT j2 EMPTY>
                <!ATTLIST j2 rule CDATA #REQUIRED parents CDATA #IMPLIED>').

