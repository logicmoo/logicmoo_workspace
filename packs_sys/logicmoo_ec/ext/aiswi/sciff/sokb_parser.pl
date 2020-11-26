:-module(sokb_parser,
	[translate_sokb/2,
	 translate_sokb/3,
	 commas_to_list/2]).


:-use_module(library(lists)).

:-use_module(parser_utils).
%:- ensure_loaded(solver).
:- use_module(solver).

translate_sokb(Origin,Destination):-
    translate_sokb(Origin,Destination,write).
translate_sokb(Origin,Destination,Mode):-
	parse_sokb(Origin,OriginalSOKB),
	dynamics(OriginalSOKB,Dynamic),
	rewrite_sokb(OriginalSOKB,RewrittenSOKB),
	append(Dynamic,RewrittenSOKB,DynamicRewrittenSOKB),
	write_sokb_to_file(Destination,DynamicRewrittenSOKB,Mode).

write_sokb_to_file(FileName,SOKB,Mode):-
	open(FileName,Mode,Stream),
	% If the user has not declared the 'fdet' predicate, declare it as dynamic
	% so SCIFF won't fail when invoking it
	(member(query([dynamic_predicate(fdet/1)]),SOKB) 
        -> true
        ;  write(Stream,':- dynamic(fdet/1).'), nl(Stream)
    ),
	write_sokb_to_stream(SOKB,Stream),
	close(Stream).

write_sokb_to_stream([],_).
write_sokb_to_stream([query([dynamic_predicate(Signature)])|MoreSOKB],Stream):-
	!,
	write(Stream,':- dynamic('),write(Stream,Signature),
	write(Stream,').'),
	nl(Stream),
	nl(Stream),
	write_sokb_to_stream(MoreSOKB,Stream).
write_sokb_to_stream([query([Atom])|MoreSOKB],Stream):-
	!,
	write(Stream,':- '),write(Stream,Atom),write(Stream,'.'),
	nl(Stream),
	nl(Stream),
	write_sokb_to_stream(MoreSOKB,Stream).
write_sokb_to_stream([query([Atom|MoreAtoms])|MoreSOKB],Stream):-
	write(Stream,':- '),write(Stream,Atom),write(Stream,','),
	nl(Stream),
	write_tab_list(Stream,MoreAtoms),
	write(Stream,'.'),
	nl(Stream),
	nl(Stream),
	write_sokb_to_stream(MoreSOKB,Stream).
write_sokb_to_stream([clause([Atom|MoreAtoms])|MoreSOKB],Stream):- !, % SWI does not optimize this cut
	write(Stream,Atom),
	write(Stream,' :- '),
	nl(Stream),
	write_tab_list(Stream,MoreAtoms),
	write(Stream,'.'),
	nl(Stream),
	nl(Stream),
	write_sokb_to_stream(MoreSOKB,Stream).
write_sokb_to_stream([fact([Atom])|MoreSOKB],Stream):-
	write(Stream,Atom),write(Stream,'.'),
	nl(Stream),
	nl(Stream),
	write_sokb_to_stream(MoreSOKB,Stream).

write_tab_list(_,[]).
write_tab_list(Stream,[Atom]):-
	!,
	indent(Stream),
	write(Stream,Atom).
write_tab_list(Stream,[Atom|MoreAtoms]):-
	indent(Stream),
	write(Stream,Atom),
	write(Stream,','),
	nl(Stream),
	write_tab_list(Stream,MoreAtoms).

indent(Stream):-
	tab_number(N),
	write_spaces_to_stream(Stream,N).

tab_number(4).

write_spaces_to_stream(_,0):-
	!.
write_spaces_to_stream(Stream,N):-
	write(Stream,' '),
	N1 is N-1,
	write_spaces_to_stream(Stream,N1).

read_terms_from_file(FileName,Terms):-
	open(FileName,read,Stream),
	read_terms_from_stream(Stream,Terms),
	close(Stream).

read_terms_from_stream(Stream,Terms):-
	read_term(Stream,Term,[variable_names(Names)]),
	(Term=end_of_file ->
	    Terms=[];
	    Terms=[t(Term,Names)|Terms1],
	    read_terms_from_stream(Stream,Terms1)).

parse_sokb(FileName,SOKB):-
	read_terms_from_file(FileName,Terms),
	parse_terms(Terms,SOKB).

parse_terms([],[]).
parse_terms([t((:-Query1),Vars)|MoreTerms],[query(Query,Vars)|MoreSOKB]):-
	!,
	commas_to_list(Query1,Query),
	parse_terms(MoreTerms,MoreSOKB).
parse_terms([t((Head:-Body1),Vars)|MoreTerms],
	    [clause([Head|Body],Vars)|MoreSOKB]):-
	!,
	commas_to_list(Body1,Body),
	parse_terms(MoreTerms,MoreSOKB).
parse_terms([t(Term1,Vars)|MoreTerms],[fact(Term,Vars)|MoreSOKB]):-
	commas_to_list(Term1,Term),
	parse_terms(MoreTerms,MoreSOKB).

commas_to_list((A,B),[A|C]):-
	!,
	commas_to_list(B,C).
commas_to_list(A,[A]).

rewrite_sokb([],[]).
rewrite_sokb([Clause|MoreClauses],[NewClause|MoreNewClauses]):-
	rewrite_clause(Clause,NewClause),
	rewrite_sokb(MoreClauses,MoreNewClauses).

rewrite_clause(Clause,NewClause):-
	Clause=..[Type,AtomList,Variables],
	rewrite_atom_list(AtomList,Variables,NewAtomList,[],OccurList),
	quantifying_atoms(OccurList,Quantifying),
	get_quantified_atom_list(Type,NewAtomList,Quantifying,
				 QuantifiedAtomList),
	NewClause=..[Type,QuantifiedAtomList].

rewrite_atom_list([],_,[],OccurList,OccurList).
rewrite_atom_list([Atom|MoreAtoms],Variables,[NewAtom|MoreNewAtoms],
		  OldOccurList,NewOccurList):-
	rewrite_atom(Atom,Variables,NewAtom,OldOccurList,IntOccurList),
	rewrite_atom_list(MoreAtoms,Variables,MoreNewAtoms,
			  IntOccurList,NewOccurList).

rewrite_atom(Atom,Variables,NewAtom,OldOccurList,NewOccurList):-
	functor(Atom,Functor,Arity),
	Atom=..[Functor|Arguments],
	rewrite_arg_list(Arguments,NewArguments,Functor/Arity,
			 Variables,OldOccurList,NewOccurList),
	NewAtom1=..[Functor|NewArguments],
	(is_constraint_functor(Functor)->
	NewAtom=clp_constraint(NewAtom1);
	NewAtom=NewAtom1).

rewrite_arg_list([],[],_,_,OccurList,OccurList).
rewrite_arg_list([Arg|MoreArgs],[NewArg|MoreNewArgs],Functor,
		 Variables,OldOccurList,NewOccurList):-
	rewrite_arg(Arg,NewArg,Functor,Variables,
		    OldOccurList,IntOccurList),
	rewrite_arg_list(MoreArgs,MoreNewArgs,Functor,
			 Variables,IntOccurList,NewOccurList).

rewrite_arg(Arg,NewArg,Functor,Variables,OldOccurList,NewOccurList):-
	compound(Arg),
	!,
	Arg=..[Fun|Args],
	rewrite_arg_list(Args,NewArgs,Functor,Variables,
			 OldOccurList,NewOccurList),
	NewArg=..[Fun|NewArgs].
rewrite_arg(Arg,VarName,Functor,Variables,OldOccurList,NewOccurList):-
	var(Arg),
	!,
	get_var_name(Variables,Arg,VarName),
	update_occur_list(OldOccurList,VarName,Functor,NewOccurList).
rewrite_arg(Arg,Arg,_,_,OccurList,OccurList).

get_var_name([],_,'_').
get_var_name([VarName=Var1|_],Var,VarName):-
	Var1==Var,
	!.
get_var_name([_|MoreVarNames],Var,VarName):-
	get_var_name(MoreVarNames,Var,VarName).

update_occur_list([],VarName,Functor,[occur(VarName,[Functor])]).
update_occur_list([occur(VarName,OldList)|MoreOccurs],
		  VarName,Functor,[occur(VarName,NewList)|MoreOccurs]):-
	!,
	update_occurs(OldList,Functor,NewList).
update_occur_list([Occur|MoreOccurs],VarName,Functor,
		  [Occur|MoreNewOccurs]):-
	update_occur_list(MoreOccurs,VarName,Functor,MoreNewOccurs).

update_occurs([],Functor,[Functor]).
update_occurs([Functor|Tail],Functor,[Functor|Tail]):-
	!.
update_occurs([Head|Tail],Functor,[Head|NewTail]):-
	update_occurs(Tail,Functor,NewTail).

quantifying_atoms([],[]).
quantifying_atoms([occur(VarName,OccurList)|MoreOccurs],
		  [quant(VarName,forallf)|MoreQuantifyingAtoms]):-
	quant_forall(OccurList),
	!,
	quantifying_atoms(MoreOccurs,MoreQuantifyingAtoms).
quantifying_atoms([occur(VarName,OccurList)|MoreOccurs],
		  [quant(VarName,existsf)|MoreQuantifyingAtoms]):-
	quant_exists(OccurList),
	!,
	quantifying_atoms(MoreOccurs,MoreQuantifyingAtoms).
quantifying_atoms([_|MoreOccurs],QuantifyingAtoms):-
	quantifying_atoms(MoreOccurs,QuantifyingAtoms).

quant_forall([Occur|MoreOccurs]):-
	forall_abducible(Occur),
	quant_forall1(MoreOccurs).

quant_forall1([]).
quant_forall1([Occur|MoreOccurs]):-
	forall_abducible(Occur),
	!,
	quant_forall1(MoreOccurs).
quant_forall1([Occur|MoreOccurs]):-
	Occur=F/_,
	is_constraint_functor(F),
	quant_forall1(MoreOccurs).

quant_exists([Occur|_]):-
	exists_abducible(Occur),
	!.
quant_exists([_|MoreOccurs]):-
	quant_exists(MoreOccurs).


forall_abducible(en/2).
forall_abducible(noten/2).

exists_abducible(e/2).
exists_abducible(en/2).
exists_abducible(abd/2).


get_quantified_atom_list(clause,[Atom|MoreAtoms],Quantifying,
			 [Atom|MoreQuantifiedAtoms]):-
	append(Quantifying,MoreAtoms,MoreQuantifiedAtoms).
get_quantified_atom_list(query,Atoms,Quantifying,QuantifiedAtoms):-
	append(Quantifying,Atoms,QuantifiedAtoms).
get_quantified_atom_list(fact,Atoms,_,Atoms).


get_signatures([],[]).
get_signatures([query(_,_)|MoreClauses],Signatures):-
	get_signatures(MoreClauses,Signatures).
get_signatures([clause([Head|_],_)|MoreClauses],
	       [Signature|MoreSignatures]):- !,  % SWI does not optimize this cut
	signature(Head,Signature),
	get_signatures(MoreClauses,MoreSignatures).
get_signatures([fact([Head|_],_)|MoreClauses],
	       [Signature|MoreSignatures]):-
	signature(Head,Signature),
	get_signatures(MoreClauses,MoreSignatures).


signature(Term,Functor/L):-
	Term=..[Functor|Arguments],
	length(Arguments,L).
	
dynamic_queries([],[]).
dynamic_queries([Signature|MoreSignatures],
		[query([dynamic_predicate(Signature)])|MoreQueries]):-
	dynamic_queries(MoreSignatures,MoreQueries).
	       

dynamics(SOKB,Dynamics):-
	get_signatures(SOKB,Signatures),
	remove_duplicates(Signatures,SignatureSet),
	dynamic_queries(SignatureSet,Dynamics).


remove_duplicates([],[]).
remove_duplicates([H|T],L):-
	member(H,T),
	!,
	remove_duplicates(T,L).
remove_duplicates([H|T1],[H|T2]):-
	remove_duplicates(T1,T2).


