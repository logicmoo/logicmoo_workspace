/*
 *	file:		in.pl
 *	version:	1.5
 *	date:		November 6, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains predicates for input and output of order files
 *
 *	history:
 *	891102	uh	Added this comment
 *				
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */

storeOrder :-
	cont(moduleName,M),
	M \== '$noName',
	!,
	cont(orderName,Ord),
	(Ord == noorder ->
		storeOrder(M)
	;
		fileNameExt(M,Ord,File),
		storeOrder(File)
	).
storeOrder :-
	error("no name associated with current specification",[],storeOrder),
	fail.


storeOrder(To) :-
	nonvar(To) ->
		fileNameExt(To,'ord',File),
		tell(File),
		try(storeOrdering),
		told,
		write('Order written to '),
		write(File).

storeOrder(ModName,OrdName) :-
	fileNameExt(ModName,OrdName,To),
	storeOrder(To).

storeOrdering :-
		tps_current_ordering(Ord),
		transformed_Order(ExternalOrder,Ord),
		write('order('),
		write(ExternalOrder),
		(	cont(moduleName,M)
		;	M = '$noName'),
		(M \== '$noName' ->
			write(' for '),
			writeq(M)
		;
			true
		),
		write(')'),
		cont(orderInterfaceExp,Exp) ->
		(Exp \== noInterface ->
			write(' using '),
			writeq(Exp)
		;
			true
		),
		write('.'),
		nl,
		((Ord == kns ; Ord == neqkns) ->
			storeOrderKns
		;
			(Ord = poly(_N) ->
				storeOrderPoly
			;
				true
			)
		).


storeOrderKns :-
	kns_gt(Op1,Op2),
	writeq((greater([[Op1,Op2]]))),
	write('.'),
	nl,
	fail.
storeOrderKns :-
	kns_eq(Op1,Op2),
	writeq((equal([[Op1,Op2]]))),
	write('.'),
	nl,
	fail.
storeOrderKns :-
	kns_status(Op,St),
	writeq((status([Op : St]))),
	write('.'),
	nl,
	fail.
storeOrderKns.


storeOrderPoly :-
	pol_get_current(pol_state(_,IList,_,_)),
	member(pol_op_interpretation(OP,Arity,Interpretation),IList),
	pol_varlist(Arity,VL),
	sPrint("setInterpretation(%% : ",['$writeq'(OP),'$list'(VL,',','(',')')]),
	pol_write_interpretation(Interpretation),
	write(').'),
	nl,
	fail.
storeOrderPoly.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


loadOrder :-
	cont(moduleName,M),
	M \== '$noName',
	!,
	cont(orderName,Ord),
	(Ord == noorder ->
		loadOrder(M)
	;
		fileNameExt(M,Ord,File),
		loadOrder(File)
	).
loadOrder :-
	error("no name associated with current specification",[],loadOrder),
	fail.


loadOrder(From) :-
	nonvar(From) ->
		fileNameExt(From,'ord',File),
		baseName(File,BaseName),
		sPrint("[loading % ...]",[BaseName]),
		nl,
                tmpFileName(TF),
		tell(TF),
		undoUponFail(enrich('',File,noorder,Error),Error),
		told,
		concAtomNames('rm ',TF,Cmd),
		unix(shell(Cmd)).
loadOrder(_From) :-
	told,
	fail.


loadOrder(ModName,OrdName) :-
	fileNameExt(ModName,OrdName,From),
	loadOrder(From).
