%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                           %%
%%      Version:  1.00   Date: 28/06/96   File: model.pl
%% Last Version:                          File:                              %%
%% Changes:                                                                  %%
%% 26/06/95 Created                                                          %%
%% 25/06/96 moved compatible from defaults.pl
%% 28/06/96 updated cnf,make_matrix, make_clause
%%                                                                           %%
%% Purpose:                                                                  %%
%%                                                                           %%
%% Author:  Torsten Schaub                                                   %%
%%                                                                           %%
%% Usage:   prolog model.pl                                                  %%
%%                                                                           %%
%%                                                                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- if(( ( \+ ((current_prolog_flag(logicmoo_include,Call),Call))) )).
:- module(xray_model,[]).
:- endif.

:- ensure_loaded(base).

:- was_dynamic(do_model_inits).

do_model_inits.
:- retractall(do_model_inits). 


%%% ----------------------------------------------------------------------
%%% model_initialization/2
%%%     generates an initial model

display_cmm(Type,Info):-wdmsg(Type:Info).

model_initialization(Matrix,cmm(Model,NewMatrix)) :-
	model_generation(matrix([],Matrix),NewMatrix,Model),
	display_cmm('INITIALIZATION',cmm(Model,NewMatrix)),
	!.

%%% Consistency checking.
%%%
%%% Add extra arguments to each goal so that information
%%% on what assumptions were made in the proof can be checked
%%% at each step/the end.
%%%
%%% I suppose Wff has to be replaced by cmm(Model,ModelStructure) ...
%%% [all this is a quick copy of add_proof_recording ...]
% ======================================================
% HEAD+(ModelIn,ModelOut)  BODY+(ModelIn,ModelOut)
% ======================================================
add_consistency_checking((Head :- Body),(Head :- Body)) :- \+ do_model_inits,!.

add_consistency_checking((Head :- Body),(Head1 :- Body1)) :-
        functor(Head,query,_) ->
                Head1 = Head,
		(do_model_inits -> conjoin(model_initialization(MM0),Body,Body2); Body=Body2),
		add_consistency_checking_args(Body2,MM0,MMOut,Body1);
        %true ->
		Head =.. L,
		append(L,[MMIn,MMOut],L1),
		Head1 =.. L1,
		add_consistency_checking_args(Body,MMIn,MMOut,Body1).

add_consistency_checking_args(Body,MMIn,MMOut,Body1) :-
        Body = (A , B) ->
                add_consistency_checking_args(A,MMIn,MMIn1,A1),
                add_consistency_checking_args(B,MMIn1,MMOut,B1),
                conjoin(A1,B1,Body1);
        Body = (A ; B) ->
                add_consistency_checking_args(A,MMIn,MMOut,A1),
                add_consistency_checking_args(B,MMIn,MMOut,B1),
                disjoin(A1,B1,Body1);
        Body =.. [prove,Goal|L] ->
                add_consistency_checking_args(Goal,MMIn,MMOut,Goal1), % ???
                Body1 =.. [prove,Goal1|L];
        Body = justification(X) ->
		Body1 = compatible(X,MMIn,MMOut);
        Body = fail ->
                Body1 = Body;
        builtin(Body) ->
                MMIn = MMOut,
                Body1 = Body;
        %true ->
                Body =.. L,
                append(L,[MMIn,MMOut],L1),
                Body1 =.. L1.

classical_clauses(WffI,WffO) :-
        WffI = (A , B) ->
                classical_clauses(A,A1),
                classical_clauses(B,B1),
		conjoin(A1,B1,WffO);
        WffI = (A ; B) ->
                classical_clauses(A,A1),
                classical_clauses(B,B1),
		disjoin(A1,B1,WffO);
        WffI = (A :- B) ->                        % ??? (special case query elim. TS Apr04)
	        WffO = true;
        builtin(WffI) ->
                WffO = true;
        %true ->
	        WffI = WffO.


query_clause(WffI,WffO) :-
        WffI = (A , B) ->
	        (query_clause(A,WffO);
                 query_clause(B,WffO));
        WffI = (A ; B) ->
                (query_clause(A,WffO);
                 query_clause(B,WffO));
        WffI = (A :- B) ->
	        (A = query ->
                      classical_clauses(B,WffO);
		 %true ->
                      fail);
        %true ->
	        fail.


add_model_structure(WffI,Q,C,WffO) :-
	WffI = (A , B) ->
                add_model_structure(A,Q,C,A1),
                add_model_structure(B,Q,C,B1),
		conjoin(A1,B1,WffO);
        WffI = (A ; B) ->
                add_model_structure(A,Q,C,A1),
                add_model_structure(B,Q,C,B1),
		disjoin(A1,B1,WffO);
        WffI = (A :- B) ->
                add_model_structure(B,Q,C,B1),
		WffO = (A :- B1);
	WffI = model_initialization(Var) ->
	        combine_clauses(Q,C,Matrix),
		WffO = model_initialization(Matrix,Var);
        %true ->
	        WffO = WffI.

%%% Runtime predicate for consistency checking
%%%

compatible(MatrixJ,cmm(Model,ModelMatrix),cmm(NewModel,NewModelMatrix)) :-
	satisfy_matrix(MatrixJ,Model,NewModel) ->
	        append_matrix(MatrixJ,ModelMatrix,NewModelMatrix),
%		display_cmm('CHECKING - satisfy':MatrixJ,cmm(NewModel,NewModelMatrix)),
		verbose('+ satisfiability check');
	adjoin_matrix(MatrixJ,ModelMatrix,ModelMatrix1) ->
%	        display_cmm('CHECKING - generation I':MatrixJ,cmm(Model,ModelMatrix)),
		model_generation(ModelMatrix1,NewModelMatrix,NewModel);
%		display_cmm('CHECKING - generation II':MatrixJ,cmm(NewModel,NewModelMatrix));
	%true ->
%		display_cmm('CHECKING - failure':MatrixJ,cmm(Model,ModelMatrix)),
		verbose('- compatible *failure*'),
		fail.

%%% ----------------------------------------------------------------------
%%% satisfy_matrix/3
%%%     checks whether a given model, ModelI, satisfies
%%%     a formula in matrix form, Matrix, or whether the model
%%%     can be extended to a model, ModelO
%%%

satisfy_matrix([],Model,Model).
satisfy_matrix([Clause],ModelI,ModelO) :-
	!,
	satisfy_clause(Clause,ModelI,ModelO).
satisfy_matrix([C1,C2],ModelI,ModelO) :-
	!,
	satisfy_clause(C1,ModelI,Model1),
	satisfy_clause(C2,Model1,ModelO).
satisfy_matrix([C1,C2,C3|M],ModelI,ModelO) :-
	satisfy_clause(C1,ModelI,Model1),
	satisfy_clause(C2,Model1,Model2),
	satisfy_clause(C3,Model2,Model3),
	satisfy_matrix(M,Model3,ModelO).

satisfy_clause([],_,_) :- 
	!,
	fail.
satisfy_clause([L],ModelI,ModelO) :-
	!,
	satisfy_literal(L,ModelI,ModelO).
satisfy_clause([L1,L2],ModelI,ModelO) :-
	satisfy_literal(L1,ModelI,ModelO);
	satisfy_literal(L2,ModelI,ModelO).
satisfy_clause([L1,L2,L3|C],ModelI,ModelO) :-
	satisfy_literal(L1,ModelI,ModelO);
	satisfy_literal(L2,ModelI,ModelO);
	satisfy_literal(L3,ModelI,ModelO);
	satisfy_clause(C,ModelI,ModelO).

satisfy_literal(L,ModelI,ModelO) :-
        identical_member(L,ModelI) ->
	       ModelO = ModelI;
	%true  ->
	       negated_literal(L,NegL),
	       \+ identical_member(NegL,ModelI),
	       ModelO = [L|ModelI].


%%% ----------------------------------------------------------------------
%%% model_generation/2

model_generation(matrix(Units,Matrix),matrix(Units2,M1),Model) :-
	unit_extraction(Matrix,M1,Units1),
%	writeln(unit_extraction),display_matrix(Matrix),display_matrix(M1),display_units(Units1),
	dp(M1,Model1),
	append(Units,Units1,Units2),
/*	list_union(Units,Units1,Units2), append allows for multiple occurrences */
	append(Units2,Model1,Model),
	verbose('* model generation').

dp([],[]) :- !.
dp(M,_) :-
	mymember([],M),
	!,
	fail.
dp(M,NewMod) :-
	M=[[L|_]|_],
	split(M,L,M1,M2),
	unit_extraction(M1,M3,U3),
	unit_extraction(M2,M4,U4),
	(dp(M3,Mod),
         KK = [L|U3];
	 dp(M4,Mod),
         negated_literal(L,K),
	 KK = [K|U4]),
        append(KK,Mod,NewMod).


split([],_,[],[]).
split([C|M],L,M3,M4) :-
	split(M,L,M1,M2),
	negated_literal(L,NegL),
	(myselect(L,C,RestC) ->
	      M3=M1,
	      M4=[RestC|M2];
	myselect(NegL,C,RestC) ->
	      M3=[RestC|M1],
	      M4=M2;
        %true ->
	      M3=[C|M1],
	      M4=[C|M2]).

%%% ----------------------------------------------------------------------
%%% cnf/2

cnf(NNF,CNF) :-
	NNF = (F1,F2) ->
	    cnf(F1,CNF1),
	    cnf(F2,CNF2),
	    conjoin(CNF1,CNF2,CNF);
        NNF = (F1;(F2,F3)) ->
	    cnf((F1;F2),CNF1),
	    cnf((F1;F3),CNF2),
	    conjoin(CNF1,CNF2,CNF);
        NNF = ((F1,F2);F3) ->
	    cnf((F1;F3),CNF1),
	    cnf((F2;F3),CNF2),
	    conjoin(CNF1,CNF2,CNF);
	NNF = (F1;F2) ->
	    cnf(F1,CNF1),
	    cnf(F2,CNF2),
            (cnf_p(CNF1,CNF2,F1,F2) ->
	           disjoin(CNF1,CNF2,CNF);
	    %true ->
		   disjoin(CNF1,CNF2,CNF12),
	           cnf(CNF12,CNF));
	%true ->
	    NNF=CNF.

cnf_p(CNF1,CNF2,F1,F2) :-
	(F1=(_,_);F2=(_,_)) ->
	         fail;
		 F1=CNF1,F2=CNF2.

make_matrix(Wff,Matrix) :-
	Wff = (A,B) ->
	      make_matrix(A,MA),
	      make_matrix(B,MB),
	      append(MA,MB,Matrix);
	Wff = true ->
	      Matrix = [];
	Wff = false ->
	      Matrix = [[]];
	%true ->
	      make_clause(Wff,Clause),
	      Matrix=[Clause].

make_clause(Wff,Clause) :-
	Wff = (A;B) ->
	      make_clause(A,CA),
	      make_clause(B,CB),
	      append(CA,CB,Clause);
	%true ->
	      Clause=[Wff].

%%% ----------------------------------------------------------------------
%%% combine_clauses\3
%%%       merges two CNFs into one while simplifying the resulting CNF
%%%       CARE: C1 is supposed to be smaller than C2

combine_clauses(C,[],C) :-
	!,
	verbose('  trivial combination').
combine_clauses([[L]],C1,[[L]|C2]) :-
	!,
	verbose('  1 unit combination'),
	simplify(L,C1,C2).	
combine_clauses([[L1],[L2]],C1,[[L1],[L2]|C3]) :-
	!,
	verbose('  2 unit combinations'),
	simplify(L1,C1,C2),
	simplify(L2,C2,C3).
combine_clauses(C1,C2,C) :-	
	verbose('  general combination'),
	append(C1,C2,C3),
	unit_reduction(C3,C4),
	subs_reduction(C4,C).

%%% ----------------------------------------------------------------------
%%% adjoin/3
%%%
/* adjoin guarantees         that the new information
          is satisfied by the current unit clauses
   adjoin does not guarantee that the new information 
                                  together with the current matrix
          is satisfied by the current unit clauses
   So you better watch for new unit clauses stemming from reducing the
   new matrix

   Nonetheless: whenever a unit is added to the unit list, this literal
   has been removed (by simplify) from the matrix.
*/
      
adjoin_matrix([[L]],matrix(Units,_),_) :-
	verbose('  1 unit adjunction *failure*'),
	negated_literal(L,NegL),
	identical_member(NegL,Units),
	!,fail.
adjoin_matrix([[L]],matrix(Units,Matrix1),matrix([L|Units],Matrix2)) :-
	!,
	verbose('  1 unit adjunction'),
	simplify(L,Matrix1,Matrix2).
adjoin_matrix([[L1],[L2]],matrix(Units,_),_) :-
						% supposition not(negated_literal(L1,L2))
	verbose('  2 unit adjunction *failure*'),
	(negated_literal(L1,NegL) ; negated_literal(L2,NegL)),
	identical_member(NegL,Units),
	!,fail.
adjoin_matrix([[L1],[L2]],matrix(Units,Matrix1),matrix([L1,L2|Units],Matrix3)) :-
						% supposition not(negated_literal(L1,L2))
	!,
	verbose('  2 unit adjunctions'),
	simplify(L1,Matrix1,Matrix2),
	simplify(L2,Matrix2,Matrix3).
adjoin_matrix(Matrix1,matrix(Units,Matrix2),matrix(Units,Matrix3)) :-
	satisfy_matrix(Matrix1,Units,_) ->
	       	verbose('  full adjunction'),
		append(Matrix1,Matrix2,Matrix3);
	% true ->
		verbose('  weak satisfaction *failure*'),
		fail.


append_matrix([[L]],matrix(Units,Matrix),matrix(Units,[[L]|Matrix])) :-
						% allows for multiple occurrences
	verbose('  1 unit appendage'),
	!.
append_matrix([[L1],[L2]],matrix(Units,Matrix),matrix(Units,[[L1],[L2]|Matrix])) :-
						% allows for multiple occurrences
	verbose('  2 unit appendage'),
	!.
append_matrix(Matrix1,matrix(Units,Matrix2),matrix(Units,Matrix3)) :-
	verbose('  full appendage'),
	append(Matrix1,Matrix2,Matrix3).

%%% ----------------------------------------------------------------------
%%% matrix_reduction/2
%%%      bunch of matrix reductions
%%%

matrix_reduction(C1,C) :-
	taut_reduction(C1,C2),
	mult_reduction(C2,C3),
	unit_reduction(C3,C4),
	subs_reduction(C4,C).

%%% ----------------------------------------------------------------------
%%% unit_reduction/2
%%%      unit reduction
%%%

unit_reduction(M,[[L]|M1]) :-
	mymember([L],M),
	!,
	simplify(L,M,M2),
	unit_reduction(M2,M1).
unit_reduction(M,M).

%%% unit_extraction/3 
%%%        is a special purpose reduction for model-finding
%%%          2nd arg gives matrix without unit clauses
%%%          3rd arg gives literals in unit clauses

unit_extraction(M,M1,[L|R]) :-
	mymember([L],M),
	!,
	simplify(L,M,M2),
	unit_extraction(M2,M1,R).
unit_extraction(M,M,[]).


simplify(_L, [], [] ) .
simplify( L, [C|Cs] , NewCs ) :-
	mymember(L,C),
	!,
	simplify(L,Cs,NewCs).
simplify( L, [C|Cs], [NewC|NewCs] ) :-
	negated_literal(L,NegL),
	myselect(NegL,C,NewC),
	!,
	simplify(L,Cs,NewCs).
simplify( L, [C|Cs], [C|NewCs]    ) :-
	simplify(L,Cs,NewCs).

%%% ----------------------------------------------------------------------
%%% subs_reduction/2
%%%      subs reduction
%%%

subs_reduction([],[]).
subs_reduction([C1|M1],M) :-
	subs_reduction(M1,M2),
	(subsumes(C1,M2,M3) ->
	    M = [C1|M3];
	%true ->
	    M = M2).

subsumes(_,[],[]).
subsumes(C1,[C2|_],_) :-
	mysubset(C2,C1),
	!,
	fail.
subsumes(C,[C1|M1],M) :-
	subsumes(C,M1,M2),
	!,
	(mysubset(C,C1) ->
	        M=M2;
	%true ->
		M=[C1|M2]).

%%% ----------------------------------------------------------------------
%%% taut_reduction/2
%%%      taut reduction
%%%

taut_reduction([],[]) :- !.
taut_reduction([C|M1],M2) :-
	taut_reduction(M1,M3),
	(taut_clause(C) ->
	    M2 = M3;
	%true ->
	    M2 = [C|M3]
	).

taut_clause(C) :-
	mymember(L,C),
	negated_literal(L,K),
	mymember(K,C).
	
%%% ----------------------------------------------------------------------
%%% mult_reduction/2
%%%     mult reduction
%%%

mult_reduction([],[]).
mult_reduction([C|M1],[NewC|M3]) :-
	mult_reduction(M1,M3),
	remove_dups(C,NewC).

remove_dups([],[]).
remove_dups([L|RestC],NewC) :-
	remove_dups(RestC,NewRestC),
	(mymember(L,NewRestC) ->
	    NewC = NewRestC;
	%true ->
	    NewC = [L|NewRestC]
	).
