/*
 *	file:		io.pl
 *	version:	1.5
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains additional IO-related predicates.
 *
 *	history:
 *	891010	js	Added this comment
 *	900123	uh	Changed definition of 
 *			fileNameExt/3
 *			If the ordering "noorder" is given, the filename
 *			has no extension
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */

rFrom(F,T):- see(F), read(T), seen.
wTo(F,T):- tell(F), write(T), write('.'),nl,told.

% Old clause: fileNameExt(_,noorder,noorder)
% New clause (changed 23.01.90 uh):
fileNameExt(N,noorder,N) :- !.
fileNameExt(user,_,user) :- !.
fileNameExt(N,E,F):-
	(cont1(currentDir,D);D=''),
	name(D,SD),
	name(N,SN),
	"/"=[Slash],
	"."=[P],
	name(E,SE),
	((D='';SN=[Slash|_]) -> SDN = SN ; append(SD,[Slash|SN],SDN)),
	append(SDN,[P|SE],SF),
	name(F,SF),
	!.


readFrom(From,N,Terms):-
	(	From=user
	;
		fileExists(From)
	),
	!,
	(From=user, var(N) ->
		write('

Please, type in specification elements terminated by <endOfFile> :'),
		nl,
		nl,
		inTermsUser(Terms)
	;
		see(From),
		(	inTerms(N,Terms,success)
		;	
			seen,
			!,
			fail
		),
		seen
	).

readFrom(From,Terms):-
	readFrom(From,_All,Terms).


fileExists(F):-
	exists(F).
fileExists(F):-
	error("no file of name %",[F],exists),
	fail.


% syntax errors result in E = error
inTerms(N,[],success):-
	nonvar(N),
	N=<0,
	!.
inTerms(N,Terms,E):-
        (	read(Term),
		!,
		E1=success
	;
		E1=error,
		Term=error
	),
        (Term\==end_of_file ->
		(var(N) -> M=N;M is N-1),
		(Term=(H:-B) ->
			assertz((H:-B)),
			inTerms(M,Terms,E2)
		;
			(Term=[_|_] ->
				inList(N, Term, Terms, E2)
			;
				(	expandTerm(Term,ETerm),
					!
				;
					ETerm=Term
				),
		        	inTerms(M,Terms1,E2),
		        	Terms=[ETerm|Terms1]
			)
		)
	;       
		Terms=[],
		(nonvar(N) ->
			E2=error
		;	E2=success
		)
	),
        (E1=E2 ->
		E=E1
	;
		E=error
	).


inList(N, _, [], success):-
	nonvar(N),
	N=<0,
	!.
inList(N, [], [], success):-
	var(N),
	!.
inList(_, [], [], error):-
	!.
inList(N, [(H:-B)|R], Terms, E) :-
	assertz((H:-B)),
	!,
	decrIfNonVar(N, M),
	inList(M, R, Terms, E).
inList(N, [T|R], [TExp|Rest], E) :-
	expandTerm(T,TExp),
	decrIfNonVar(N, M),
	inList(M, R, Rest, E).


decrIfNonVar(N, N) :-
	var(N),
	!.
decrIfNonVar(N, M) :-
	M is N - 1.



% syntax errors are ignored
inTermsUser(Terms):-
	repeat,
	read(Term),
	!,
	(Term\==end_of_file->
		(	expandTerm(Term,ETerm),
			!
		;
			ETerm=Term
		),
		inTermsUser(Terms1),
		Terms=[ETerm|Terms1]
	;
		Terms=[]
	).



readS(X):-
	read(X),
	(scriptTo(F)->
		telling(T),
		tell(F),
		write(X),
		nl,
		tell(T)
	 ;
		true
	).



writeS(X):-
	write(X),
	(scriptTo(F)->
		telling(T),
		tell(F),
		write(X),
		tell(T)
	 ;
		true
	).

nlS:-
	nl,
	(scriptTo(F)->
		telling(T),
		tell(F),
		nl,
		tell(T)
	 ;
		true
	).

writeqS(X):-
	writeq(X),
	(scriptTo(F)->
		telling(T),
		tell(F),
		writeq(X),
		tell(T)
	 ;
		true
	).


writeDec(N,M,C):-
	digits([],N,Digs),
	length(Digs,M1),
	(M1 >= M ->
		write(N)
	;	Diff is M-M1,
		genList(Diff,C,Filling),
		writeList(Filling,''),
		write(N)
	),
	!.


digits([],0,[0]):-!.
digits(Dig,0,Dig):-!.
digits(Dig,N,Dig1):-
	!,
	Digit is N mod 10,
	N1 is N//10,
	digits([Digit|Dig],N1,Dig1).

	
write3Dec(N):-
	writeS('  '),
	writeS(N).

write3Dec(N):-
	N>=100,
	!,
	writeS(N).
write3Dec(N):-
	N>=10,
	!,
	writeS(' '),
	writeS(N).
write3Dec(N):-
	writeS('  '),
	writeS(N).




script(F):-
	fileNameExt(F,scr,SF),
	(	retract(scriptTo(_))
	;
		true
	),
	writeS('scripting to '),writeS(SF),writeS('.'),
	asserta(scriptTo(SF)).

noScript:-
	(	retract(scriptTo(F))
	;
		true
	).



promptFor(T,Type,ErrorAction):-
	repeat,
	read(T),
	(apply(Type,[T]) ->
		true
	;	write('???'),
		nl,
		ErrorAction,
		nl,
		write('Please, try again: '),
		fail
	),
	!.


prompt1(Requests,Answers,A):-
	prompt1(Requests,Answers,member(A,Answers),A).


prompt1(Requests,Answers,Condition,A):-
	nl,
	writeList(Requests,'
'),
	repeat,
	nl,
	write('   Please answer with '),
	writeList(Answers,'. or '),
	write('. (Type A. to abort)'),
	write(' >'),
	read(A),
	(var(A) ->
		abortFromCompletion
	;	true),
	(	call(Condition)
	;
		write('???'),
		fail
	),
	!.

abortFromCompletion:-
		(cont1(indProve,true) ->
			indProve:==false, 
			sPrint("

Proof aborted.
Type ``load('$beforeIndProve').'' to restore state before proof attempt.
",[])
		;	sPrint("

Completion aborted.
Type ``undo.''    to restore state before completion.
Type ``cResume.'' to resume completion.
",[])),

		disable_q_option,
		abort.




writeList([],_):-
	!.
writeList([X],_):-
	print(X),
	!.
writeList([F,S|Xs],Sep):-
	print(F),
	write(Sep),
	writeList([S|Xs],Sep),
	!.
writeList(X,_):-
	print(X). 


tab:-
	put(9).


sPrint([Char|String],[P|Ps]):-
	name('%',[Char]),
	print(P),
	sPrint(String,Ps),
	!.
sPrint([Char|String],Ps):-
	put(Char),
	sPrint(String,Ps),
	!.
sPrint([],_):-
	!.
sPrint(_,[]):-
	write(' *** sPrint : too few parameters'),
	nl.

writeAtoms([],_).
writeAtoms([A|As],N):-
	writeAtom(A,N),
	writeAtoms(As,N),
	!.

writeAtom(A,N):-
	fillAtomName(A,N,AN),
	write(AN),
	!.

fillAtomName(A,N,AN):-
	name(A,AS),
	length(AS,M),
	S is N-M,
	max(S,1,S1),
	name(' ',[BlankCode]),
	genList(S1,BlankCode,BlanksS),
	append(AS,BlanksS,ANS),
	name(AN,ANS),
	!.


	
error(Message,Args,F):-
	error:==true,
	append(" *** % : ",Message,SM),
	tell(user),
	sPrint(SM,[F|Args]),
	nl,
	told,
	!.


systemError(Message,Args):-
	error:==true,
	append(" *** system error : ",Message,SM),
	tell(user),
	sPrint(SM,Args),
	nl,
	told,
	!.
	

dirExists(D):-
	concAtomNames('test -d ',D,C),
	unix(system(C)).

instantiate(PVar,N1,N):-
                var(PVar),
		!,
		PVar = '$VAR'(N),
		N1 is N + 1.
instantiate(Term,N1,N) :-
		Term=..[_|LTerm],
		iter(instantiate,N,LTerm,[],N1).

printClause(Es1,Sep) :-
	make_list(Es1,Es),
	instantiate(Es,_,1),
	map(lambda([X,'$literal'(X)],true),Es,Cs),
	writeList(Cs,Sep).

make_list(X,[X]) :- var(X), !.
make_list([H|T],[H|T]) :- !.
make_list(T,[T]) :- !.

printCRule(Cond,L1,R1) :-
	make_list(L1,L2),
	make_list(R1,R2),
	user:map(lambda([Lit,Eq],
		    Lit1^(Lit= -(Lit1) ->
			Eq = Lit1
		    ;	Eq = -(Lit))), Cond,CEqs),
	instantiate([CEqs,L2,R2],_,1),
	(CEqs = [] ->
	    format('~8|~s~p~s~p',["(",'$clause'(L2),") --> ",'$clause'(R2)])
	;
	    format('~8|~p~s~p~s~p',['$clause_conjunction'(CEqs)," => (",'$clause'(L2),") --> ",'$clause'(R2)])
	).
