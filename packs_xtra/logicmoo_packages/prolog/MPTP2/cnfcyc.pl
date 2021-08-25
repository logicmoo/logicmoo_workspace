%%- -*-Mode: Prolog;-*--------------------------------------------------
%% specialised fast clausifier for the CyC large files
:- [utils].

cnf(! _A : B, B1):- !, cnf(B,B1).
cnf((A & B) => C, (A1 | D)):- !,
	cnfneglit(A,A1),
	cnf(B => C, D).

%% C is always literal here in CyC
cnf(B => C, (B1 | C)):- !, cnfneglit(B,B1).


cnf(~(A & B & C), (A1 | B1 | C1)):- !,
	cnfneglit(A,A1),
	cnfneglit(B,B1),
	cnfneglit(C,C1).

cnf(~(A & B), (A1 | B1)):- !,
	cnfneglit(A,A1),
	cnfneglit(B,B1).

cnf(A,A).

cnfneglit(~A,A):-!.
cnfneglit(A,~A).

cyccnf(InFile,OutFile):-
	declare_mptp_predicates,
	consult(InFile),
	tell(OutFile),
	repeat,
	(
	  fof(A,B,Fof),
	  cnf(Fof,Cnf),
	  numbervars(Cnf,0,_),
	  print(cnf(A,B,Cnf)),
	  write('.'),nl,
	  fail
	;
	  told
	).

%% this takes ca. 30s
%% ##TEST: :- cyccnf('CSR002+4.ax','00cyctstcnf').
