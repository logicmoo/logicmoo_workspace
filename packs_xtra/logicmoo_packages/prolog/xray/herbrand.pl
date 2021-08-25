add_herbrand_preds((Head :- Body),(Head :- Body1)) :-
	herbrandize_variables(Body,[],BodyVars,false,_),
	herbrandize_variables(Head,BodyVars,_,true,Matches),
        conjoin(Matches,Body,Body1).
	

herbrandize_variables(Term,VarsIn,VarsOut,MatchesIn,MatchesOut) :-
        builtin(Term) ->
	        VarsOut = VarsIn,
                MatchesOut = MatchesIn;
        %true ->
	        nonvar(Term) ->
                       myfunctor(Term,_,N),
		       herbrandize_args(Term,VarsIn,VarsOut,MatchesIn,MatchesOut,1,N);
	        identical_member(Term,VarsIn) ->
	               VarsOut = VarsIn,
		       MatchesOut = MatchesIn;
	        %true ->
		       VarsOut = [Term|VarsIn],
		       conjoin(MatchesIn,herbrand(Term),MatchesOut).

herbrandize_args(Term,VarsIn,VarsOut,MatchesIn,MatchesOut,I,N) :-
        I > N ->
                VarsOut = VarsIn,
                MatchesOut = MatchesIn;
        %true ->
                arg(I,Term,Arg),
                herbrandize_variables(Arg,VarsIn,Vars1,MatchesIn,Matches1),
                I1 is I + 1,
                herbrandize_args(Term,Vars1,VarsOut,Matches1,MatchesOut,I1,N).

herbrand_universe(U) :-
	setof(X,herbrand(X),U).

herbrand_preds([],true).
herbrand_preds([C|Cs],Wff) :-
	herbrand_preds(Cs,Wffs),
	conjoin((herbrand(C):-true),Wffs,Wff).

add_answer_preds((query :- Query),(query :- (Query,nl,nl,write(answer:Vars),nl))) :-
	!,
	variables(Query,Vars).
add_answer_preds(R,R).

%%% constants returns a list of the constants appearing in a formula.

constants(Wff,L) :-
        Wff = (A :- B) ->
                constants(A,L1),
                constants(B,L2),
                union(L2,L1,L);
        Wff = (A , B) ->
                constants(A,L1),
                constants(B,L2),
                union(L2,L1,L);
        Wff = (A ; B) ->
                constants(A,L1),
                constants(B,L2),
                union(L2,L1,L);
        Wff = (A : B) ->
                constants(A,L1),
                constants(B,L2),
                union(L2,L1,L);
        myfunctor(Wff,search,_) ->        % list constants in first argument of search
                arg(1,Wff,X),
                constants(X,L);
        builtin(Wff) ->
                L = [];
        %true ->
                myfunctor(Wff,_,N),
                (N > 0 ->
		   constantize_args(Wff,[],L,1,N);
		 %true ->
                   L = []).

constantize_args(Term,FnsIn,FnsOut,I,N) :-
	var(Term) ->
		FnsOut = FnsIn;
	atom(Term) ->
	        FnsOut = [Term|FnsIn];
        I > N ->
                FnsOut = FnsIn;
        %true ->
                arg(I,Term,ArgI),
		(var(ArgI) ->
		        Fns1 = [];
		%true ->
		        myfunctor(ArgI,_,NI),
                        constantize_args(ArgI,FnsIn,Fns1,1,NI)),
                I1 is I + 1,
                constantize_args(Term,Fns1,FnsOut,I1,N).

%%% variables returns a list of the variables appearing in a formula.

variables(Wff,L) :-
        Wff = (A :- B) ->
                variables(A,L1),
                variables(B,L2),
                union(L2,L1,L);
        Wff = (A , B) ->
                variables(A,L1),
                variables(B,L2),
                union(L2,L1,L);
        Wff = (A ; B) ->
                variables(A,L1),
                variables(B,L2),
                union(L2,L1,L);
        Wff = (A : B) ->
                variables(A,L1),
                variables(B,L2),
                union(L2,L1,L);
        myfunctor(Wff,search,_) ->        % list variables in first argument of search
                arg(1,Wff,X),
                variables(X,L);
        builtin(Wff) ->
                L = [];
        %true ->
                myfunctor(Wff,_,N),
                (N > 0 ->
		   variablize_args(Wff,[],L,1,N);
		 %true ->
                   L = []).

variablize_args(Term,FnsIn,FnsOut,I,N) :-
	atom(Term) ->
		FnsOut = FnsIn;
	var(Term) ->
	        FnsOut = [Term|FnsIn];
        I > N ->
                FnsOut = FnsIn;
        %true ->
                arg(I,Term,ArgI),
		(var(ArgI) ->
		        union([ArgI],FnsIn,Fns1);
		%true ->
		        myfunctor(ArgI,_,NI),
                        variablize_args(ArgI,FnsIn,Fns1,1,NI)),
                I1 is I + 1,
                variablize_args(Term,Fns1,FnsOut,I1,N).


variablize_clause(Clause,Vars) :-
	ClauseTerm =.. [clause|Clause],
	variables(ClauseTerm,Vars).

instance([],_,Clause).
instance(Vars,Cons,Clause) :-
	myselect(V,Vars,V1s),
	member(V,Cons),
	instance(V1s,Cons,Clause).

skolem([],Term,Term).
skolem([Var|Vars],Term,SkTerm) :-
	skolem(Vars,Term,Term1),
	SkTerm=(Var^Term1).

instances(Clause,Terms,Instances) :-
	variablize_clause(Clause,Vars),
	skolem(Vars,instance(Vars,Terms,Clause),DoIt),
	setof(Clause,DoIt,Instances).

instantiation([],_,[]) :-
	!.
instantiation(Matrix,[],Matrix) :-
	!.
instantiation([Clause|Matrix],Terms,MatrixInstances) :-
	instances(Clause,Terms,Instances),
	instantiation(Matrix,Terms,IMatrix),
	combine_clauses(Instances,IMatrix,MatrixInstances).
