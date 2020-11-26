/*
* Copyright (C) 2002, 2007 Christoph Wernhard
* 
* This program is free software; you can redistribute it and/or modify it
* under the terms of the GNU General Public License as published by the Free
* Software Foundation; either version 2 of the License, or (at your option)
* any later version.
* 
* This program is distributed in the hope that it will be useful, but WITHOUT
* ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
* FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
* more details.
* 
* You should have received a copy of the GNU General Public License along with
* this program; if not, see <http://www.gnu.org/licenses/>.
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% Mimic (PLRDF)
%%%%
%%%% A Prolog Syntax for RDF that mimics the original XML Syntax.
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(mimic, [ mimic_file_triples/2,
		   mimic_file_triples/3,
		   mimic_terms_triples/3,
		   triples_to_mimic/3,
		   triples_to_mimic_facts/3,
		   pp_mimic_terms/1,
		   pp_mimic_term/1 ]).
%%%% 
%%%% *** TODO: allow nonempty conjunctions of P=V as terms (i.e. whithout
%%%% class or subject.						  
%%%% *** Check that the empty namespace works - items start with _ 
%%%%

:- use_module('swilib/fromonto').
:- use_module('swilib/err').
:- use_module('rdf_convert').
:- use_module('rdf_writer').
:- use_module(library(ordsets)).
:- use_module(library(gensym)).

%%%% 
%%%% Triples are in "canonic" form (i.e. internal KB form).
%%%% 

/*

Mimic Syntax Description
========================


MimicTermSequence ::= SEQUENCE(MimicTerm)

           Within Prolog, SEQUENCE means a list. As file format SEQUENCE
	   means a list of statements, each terminated by '.'.

MimicTerm  ::= NamespaceTerm | DescriptionTerm | FactTerm | CommentTerm

CommentTerm ::= comment(Atom)

NamespaceTerm ::= namespace(Prefix, Namespace)

           Prefix and Namespace are atoms.

FactTerm ::= fact(FactSubject, Property, FactObject)
	   
DescriptionTerm ::=
           [Subject][:Class][+= CONJUNCTION(PropObj)]

	   At least one of Subject or Class must be present.

PropVal ::= RDFSComment | Property=ObjectTerm

FactSubject ::= Subject | blank(Id)

FactObject ::= Object | blank(Id)

ObjectTerm ::= CONJUNCTION(DescriptionTerm)

Subject, Class and Property are atoms that start with a namespace
prefix, separated from the name by '_'.

RDFSComment is an atom and used as rdfs_comment property.

The prefixes used in a DeclarationTerm must be declared by a
preceeding NamespaceTerm. The effective uri is obtained by
concatenating the Namespace associated with the Prefix and the name.
The empty atom is permitted as a namespace. The namespace
'http://www.infraengine.com/literal#' is used to indicate literals. An uri
starting with this namespace is converted to a literal with the rest
of the uri as content.

If there are several declarations of the same prefix, it is
unspecified, which one is used.

The prefixes rdf and rdfs are predefined for
'http://www.w3.org/1999/02/22-rdf-syntax-ns#' and
'http://www.w3.org/2000/01/rdf-schema#'.

If no subject is present in a declaration term, that corresponds to an
anonymous (blank) subject.

Facts can contain explicit blank(Id) terms as subject or object. Id is
an arbitrary atom. Its "scope" is the MimicTermSequence.

CommentTerms are ignored in semantics processing.

TODO: Specify "semantics", i.e. which triples are generated. This is
straightforward.


Printing/Editing Hints
======================

In addition to the standard Prolog operators, two further declarations
are used to improve readability:
:- op(500, fx, :).
:- op(1200, xfx, +=).

In the "new Prolog mode" for Emacs (0.1.34), the variable
prolog-head-delimiter (which is declared as a constant there for no
good reason) can be modified to include '+='.


Rationales
==========

':'/2 is commonly used for type declaration. '+=' associates, that the
"information about a subject" (i.e. set of facts with it in subject
role) is incremented by an RDF description. There can be several
descriptions about the same subject.


An Example
==========

BEGIN_OF_FILE

namespace(l, 'http://www.infraengine.com/literal#').
namespace(w3h, 'http://www.w3.org/Home/').
namespace(w3s, 'http://www.w3.org/staffId/').
namespace(mailto, 'mailto:').
namespace(s, 'http://www.description.org/schema/').

w3h_Lassila +=
	'An example',
	s_Creator=(w3s_85740:s_Person +=
		      s_name=('l_Ora Lassila', 'l_Ora'),
		      s_email='mailto_lassila@w3.org').
END_OF_FILE

This file generates the following triples:

  rdf( 'http://www.w3.org/Home/Lassila',
       rdfs_comment,
       literal('An example')),
  rdf( 'http://www.w3.org/Home/Lassila',
       'http://www.description.org/schema/Creator',
       'http://www.w3.org/staffId/85740'),
  rdf( 'http://www.w3.org/staffId/85740',
       rdf_type,
       'http://www.description.org/schema/Person'),
  rdf( 'http://www.w3.org/staffId/85740',
       'http://www.description.org/schema/name',
       literal('Ora Lassila')),
  rdf( 'http://www.w3.org/staffId/85740',
       'http://www.description.org/schema/name',
       literal('Ora')),
  rdf( 'http://www.w3.org/staffId/85740',
       'http://www.description.org/schema/email',
       'mailto:lassila@w3.org')

*/

%%%% 
%%%% SWI-BUG: read_term seems not to consider module local operators, so we
%%%% have to declare them global here.
%%%% Also should ensure that there are NO UNUSED operators active. 
%%%% Clashes seem however rare, since we use the namespace prefixes
%%%% (except for comments).
%%%%

:- op(500, fx, user:(:)).
:- op(1200, xfx, user:(+=)).

mimic_file_triples(File, Triples) :-
	file_terms(File, Terms),
	mimic_terms_triples(Terms, [], Triples).

mimic_file_triples(File, Triples, Namespaces) :-
	file_terms(File, Terms),
	mimic_terms_triples(Terms, [namespaces(Namespaces)], Triples).

file_terms(File, Terms) :-
	from_file( read_terms(Terms), File ).

read_terms([T|Ts]) :-
	read_term(T, [module(mimicops)]),
	T \== end_of_file,
	!,
	read_terms(Ts).
read_terms([]).

mimic_terms_triples(Terms, Options, Triples) :-
	( memberchk(input_namespaces(NAs), Options) ->
	  map_transpose(NAs, Ns)
	; Ns = [rdf-'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
		rdfs-'http://www.w3.org/2000/01/rdf-schema#']
	),
	( memberchk(mimic_fix_blanks, Options) ->
	  map_mimic_fix_blanks(Terms, Terms1)
	; Terms1 = Terms
	),
	( memberchk(namespaces(Namespaces), Options) ->
	  findall(N-A, ( member(namespace(A, N), Terms),
			 \+ N = '',
			 \+ literal_namespace(N)
		       ),
		  Namespaces)
        ; true
        ),
	findall(Triple,	mimic_terms_triple(Terms1, Ns, Triple), Triples).

map_transpose([X-Y|Xs], [Y-X|Xs1]) :-
	map_transpose(Xs, Xs1).
map_transpose([], []).
		    
mimic_terms_triple([comment(_)|Ts], Ns, Triple) :-
	!,
	mimic_terms_triple(Ts, Ns, Triple).
mimic_terms_triple([namespace(P, N)|Ts], Ns, Triple) :-
	!,
	mimic_terms_triple(Ts, [P-N|Ns], Triple).
mimic_terms_triple([fact(S, P, O)|Ts], Ns, Triple) :-
	!,
	( fact_triple(S, P, O, Ns, Triple)
	; mimic_terms_triple(Ts, Ns, Triple)
	).
mimic_terms_triple([T|Ts], Ns, Triple) :-	
	( description_triple(T, Ns, Triple)
	; mimic_terms_triple(Ts, Ns, Triple)
	).

fact_triple(S, P, O, Ns, rdf(S1, P1, O1)) :-
	standardize(S, Ns, S1),
	standardize(P, Ns, P1),
	standardize(O, Ns, O1).

standardize(Item, N, Item1) :-
	atom(Item),
	!,
	( sub_atom(Item, B, _, _, '_'), B > 0 ->
	  true
	; err('Missing mimic namespace prefix: ~q.', [Item])
	),
	sub_atom(Item, 0, B, _, Prefix),
	( memberchk(Prefix-Namespace, N) ->
	  true
	; err('Undeclared mimic namespace prefix: ~q.', [Prefix])
	),
	B1 is B + 1,
	sub_atom(Item, B1, _, 0, Name),
	( literal_namespace(Namespace) ->
	  Item1 = literal(Name)
	; atom_concat(Namespace, Name, Item2),
	  canonicalize_item(Item2, Item1)
	).
standardize(Item, _, Item).

gen_blank(blank(Id)) :-
	gensym(g, Id).

subject(S:_, N, S1) :-
	!,
	standardize(S, N, S1).
subject(:_, _, S) :-
	!,
	gen_blank(S).
subject((SC += _), N, S) :-
	!,
	subject(SC, N, S).
subject(S, N, S1) :-
	atom(S),
	!,
	standardize(S, N, S1).
subject(T, _, _) :-
	err('Bad mimic term: ~q.', [T]).
	
class(_:C, N, C1) :-
	!,
	standardize(C, N, C1).
class(:C, N, C1) :-
	!,
	standardize(C, N, C1).
class((SC += _), N, C) :-
	!,
	class(SC, N, C).

description_triple(Term, N, Triple) :-
	subject(Term, N, S),
	s_triple(Term, S, N, Triple).

s_triple(T, S, N, rdf(S, rdf_type, C)) :-
	class(T, N, C).
s_triple((_ += KVs), S, N, Triple) :-
	po_triple(KVs, S, N, Triple).

po_triple((X , Y), S, N, Triple) :-
	!,
	( po_triple(X, S, N, Triple)
	; po_triple(Y, S, N, Triple)
	).
po_triple(P=O, S, N, Triple) :-
	!,
	standardize(P, N, P1),
	o_triple(O, S, P1, N, Triple).
po_triple(O, S, _, rdf(S, rdfs_comment, literal(O))) :-
	atom(O),
	!.
po_triple(PO, _, _, _) :-
	err('Bad mimic property specifier: ~q.', [PO]).

o_triple((X, Y), S, P, N, Triple) :-
	!,
	( o_triple(X, S, P, N, Triple)
	; o_triple(Y, S, P, N, Triple)
	).
o_triple(O, S, P, N, Triple) :-
	subject(O, N, O1),
	( Triple = rdf(S, P, O1)
	; s_triple(O, O1, N, Triple)
	).

literal_namespace('http://www.infraengine.com/literal#').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

triples_to_mimic(Triples, NAs, Mimic) :-
	catch( ( triples_to_mimic_descs(Triples, NAs, Mimic1),
		 ( Mimic1 = [] ->
		   Mimic = [comment('No facts')]
		 ; Mimic = Mimic1
		 )
               ),
	       err_serializability(_, _),
	       ( triples_to_mimic_facts(Triples, NAs, Mimic1),
		 Mimic =
	         [comment('Only serializable as fact terms.')|
		  Mimic1]
	       )		 
	     ).

triples_to_mimic_descs(Triples, NAs, Mimic) :-
	aux_check_blanks(Triples),
	fix_prefixes(Triples, NAs, NAs1, Triples1),
	namespace_declarations(NAs1, Mimic, Descs),
	aux_group(Triples1, ResDescs, BlankDescs),
	map_desce(ResDescs, BlankDescs, BlankDescs1, Es),
	map_blank_desce(BlankDescs1, Es, Descs).

triples_to_mimic_facts(Triples, NAs, Mimic) :-
	fix_prefixes(Triples, NAs, NAs1, Triples1),
	namespace_declarations(NAs1, Mimic, Facts),
	map_mfact(Triples1, Facts).

map_mfact([rdf(S,P,O)|Xs], [fact(S,P,O)|Xs1]) :-
	map_mfact(Xs, Xs1).
map_mfact([], []).
	
namespace_declarations(['http://www.w3.org/1999/02/22-rdf-syntax-ns#'-rdf|NAs],
		       ANs, Rest) :-
	!,
	namespace_declarations(NAs, ANs, Rest).

namespace_declarations(['http://www.w3.org/2000/01/rdf-schema#'-rdfs|NAs],
		       ANs, Rest) :-
	!,
	namespace_declarations(NAs, ANs, Rest).
namespace_declarations([N-A|NAs], [namespace(A,N)|ANs], Rest) :-
	namespace_declarations(NAs, ANs, Rest).
namespace_declarations([], Rest, Rest).

fix_prefixes(Triples, NAs, NAs1, Triples1) :-
	sort(NAs, NAs2),
	reverse(NAs2, NAs3),
	once(( find_name('lit', LA), \+ memberchk(_-LA, NAs3) )),
	once(( find_name('abs', AA), \+ memberchk(_-AA, NAs3) )),
	map_fp_1(Triples, LA, AA, NAs3, [], NAs1, Triples1).

map_fp_1([T|Ts], LA, AA, NAs, NAs1, NAs2, [T1|Ts1]) :-
	fp_1(T, LA, AA, NAs, NAs1, NAs3, T1),
	map_fp_1(Ts, LA, AA, NAs, NAs3, NAs2, Ts1).
map_fp_1([], _, _, _, NAs1, NAs1, []).

fp_1( rdf(S, P, O), LA, AA, NAs, NAs1, NAs2, rdf(S1, P1, O1)) :-
	fp_2(S, LA, AA, NAs, NAs1, NAs3, S1),
	fp_2(P, LA, AA, NAs, NAs3, NAs4, P1),
	( P1 = rdfs_comment, O = literal(O1) ->
          NAs2 = NAs4
	; fp_2(O, LA, AA, NAs, NAs4, NAs2, O1)
	).

fp_2( literal(Lit), LA, _, _, NAs1, NAs2, Item1 ) :-
	!,
	ensure_atom(Lit, Lit1), %% *** ?
	literal_namespace(LN),
	concat_atom([LA, '_', Lit1], Item1),
	ord_add_element(NAs1, LN-LA, NAs2).
fp_2( blank(Id), _, _, _, NAs, NAs, blank(Id) ) :-
	!.
fp_2( Item, _, AA, NAs, NAs1, NAs2, Item1 ) :-
	atom(Item),
	!,
	public_item(Item, Item2),
	add_predefineds(NAs, NAs3),
	( member(N-A, NAs3), sub_atom(Item2, 0, L, _, N), L > 0 ->
	  sub_atom(Item2, L, _, 0, Item3),
	  concat_atom([A, '_', Item3], Item1),
	  ord_add_element(NAs1, N-A, NAs2)
	; concat_atom([AA, '_', Item2], Item1),
	  ord_add_element(NAs1, ''-AA, NAs2)	    
	).

add_predefineds(Ns, ['http://www.w3.org/1999/02/22-rdf-syntax-ns#'-rdf,
		     'http://www.w3.org/2000/01/rdf-schema#'-rdfs|Ns]).
	 
ensure_atom(X, X) :-
	atom(X),
	!.
ensure_atom(X, X1) :-
	term_to_atom(X, X1).

find_name(Name, Name).
find_name(Name, Name1) :-
	gen_new_name_n(Name, 1, Name1).

gen_new_name_n(Name, N, Name1) :-
	concat_atom([Name, N], Name1).
gen_new_name_n(Name, N, Name1) :-
	N1 is N + 1,
	gen_new_name_n(Name, N1, Name1).

map_blank_desce([D|Ds], Es, [E|Es1]) :-
	desce(D, Ds, Ds1, E),
	map_blank_desce(Ds1, Es, Es1).
map_blank_desce([], Es, Es).

map_desce([D|Ds], Bs, Bs1, [E|Es]) :-
	desce(D, Bs, Bs2, E),
	map_desce(Ds, Bs2, Bs1, Es).
map_desce([], Bs, Bs, []).

map_poe([PO], Bs, Bs1, E) :-
	!,
	poe(PO, Bs, Bs1, E).
map_poe([PO|POs], Bs, Bs1, (E,Es)) :-
	poe(PO, Bs, Bs2, E),
	map_poe(POs, Bs2, Bs1, Es).

desce(S-POs, Bs, Bs1, E) :-
	( select( rdf_type-C, POs, POs1) ->
	  true
	; POs1 = POs
	),
	( S = blank(_) ->
	  ( var(C) -> SC = (:rdfs_Resource) ; SC = (:C) )
	; ( var(C) -> SC = S ; SC = S:C )
	),	
	( POs1 = [] ->
	  E = SC,
	  Bs1 = Bs
	; E = (SC += POEs),
	  setof(P-Os, setof(O, member(P-O, POs1), Os), POGroups),
	  ( select(rdfs_comment-Comments, POGroups, POGroups1) ->
	    POGroups2 = [rdfs_comment-Comments|POGroups1]
	  ; POGroups2 = POGroups
	  ),
	  map_poe(POGroups2, Bs, Bs1, POEs)
	).

poe(rdfs_comment-Os, Bs, Bs1, OEs) :-
	!,
	map_oe(Os, Bs, Bs1, OEs).
poe(P-Os, Bs, Bs1, P=OEs) :-
	map_oe(Os, Bs, Bs1, OEs).

map_oe([O], Bs, Bs1, OE) :-
	!,
	oe(O, Bs, Bs1, OE).
map_oe([O|Os], Bs, Bs1, (OE, OEs)) :-
	oe(O, Bs, Bs2, OE),
	map_oe(Os, Bs2, Bs1, OEs).

oe(O, Bs, Bs1, OE) :-
        ( O = blank(_) ->
	  select_blank(O, Bs, Bs2, D),
	  desce(D, Bs2, Bs1, OE)
        ; OE = O,
	  Bs1 = Bs
	).

select_blank(B, BDs, BDs1, B-POs) :-
	select(B-POs, BDs, BDs1),
	!.
select_blank(B, _, _, _) :-
	err_serializability(
	    'Not serializable since ~q violates a blank constraint.', [B]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pp_mimic_terms(Terms) :-
	( member(Term, Terms),
	  ( Term \= namespace(_, _), Term \= fact(_, _, _) -> nl ; true ),
	  pp_mimic_term(Term),
	  writeln('.'),
	  fail
	; true
	).
	
tab_width(8).

indent(N) :-
	N > 0,
	!,
	write(' '),
	N1 is N - 1,
	indent(N1).
indent(_).

print_length(Term, L) :-
	sformat(S, '~q', [Term]),
	string_length(S, L).

pp_mimic_term( comment(Comment) ) :-
	!,
	format('~q', [comment(Comment)]).
pp_mimic_term( namespace(A, N) ) :-
	!,
	format('~q', [namespace(A, N)]).
pp_mimic_term( fact(S, P, O) ) :-
	!,
	format('~q', [fact(S, P, O)]).
pp_mimic_term( D ) :-
	pp_d( D, 0 ).


pp_d((SC += POs), W) :-
        !,
        ( W > 0 ->
	  write('('),
	  W1 is W + 1
        ; W1 = W
	),
	writeq(SC),
	write(' += '),
	tab_width(T),
	W2 is W1 + T,
	pp_pos(POs, W2),
	( W > 0 ->
	  write(')')
	; true
	).
pp_d(SC, _) :-
	writeq(SC).


pp_pos((POs, POs1), W) :-
	!,
	pp_pos(POs, W),
	write(','),
	pp_pos(POs1, W).
pp_pos(P=O, W) :-
	!,
	nl,
	indent(W),
	writeq(P),
	write(' = '),
	print_length(P, L),
	W1 is W + L + 3,
	pp_os(O, W1).
pp_pos(Comment, W) :-
	nl,
	indent(W),
	writeq(Comment).

pp_os((O, O1), W) :-
	!,
	write('('),
	W1 is W + 1,
	pp_multiple_os((O, O1), W1),
	write(')').
pp_os(O, W) :-
	pp_o(O, W).
	  
pp_multiple_os((O, O1), W) :-
	!,
	pp_multiple_os(O, W),
	format(',~n'),
	indent(W),
	pp_multiple_os(O1, W).
pp_multiple_os(O, W) :-
	pp_o(O, W).
	
pp_o(O, W) :-
	pp_d(O, W).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Fix Blanks
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mimic_fix_blanks((:C += PO), (S:C += PO1)) :-
        !,
	gen_blank(S),
	map_mimic_fix_blanks_prop(PO, PO1).
mimic_fix_blanks((:C), (S:C)) :-
        !,
	gen_blank(S).
mimic_fix_blanks(X, X).

map_mimic_fix_blanks_prop(P=V, P=V1) :-
	!,
	mimic_fix_blanks(V, V1).	
map_mimic_fix_blanks_prop((P=V, PVs), (P=V1, PVs1)) :-
	!,
	mimic_fix_blanks(V, V1),
	map_mimic_fix_blanks_prop(PVs, PVs1).

map_mimic_fix_blanks([X|Xs], [X1|Xs1]) :-
	mimic_fix_blanks(X, X1),
	map_mimic_fix_blanks(Xs, Xs1).
map_mimic_fix_blanks([], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

