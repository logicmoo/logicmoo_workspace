:- modeh(1,active(+drug,+activity)).


:- modeb(*,bond(+drug,-atomid,-atomid,#integer)).
:- modeb(*,atm(+drug,-atomid,#element,#integer,-charge)).
:- modeb(1,ind1(+drug,-ind)).
:- modeb(1,lumo(+drug,-energy)).
:- modeb(1,logp(+drug,-hydrophob)).
:- modeb(1,hansch_attr(+drug,-hattr)).
:- modeb(1,lteq(+charge,#real)).
:- modeb(1,lteq(+energy,#real)).
:- modeb(1,lteq(+hydrophob,#real)).
:- modeb(1,lteq(+hattr,#real)).

:- modeb(1,(+activity)=(#activity)).

% :- modeb(1,gteq(+charge,#real)).
% :- modeb(1,gteq(+energy,#real)).
% :- modeb(1,gteq(+hydrophob,#real)).
% :- modeb(1,(+charge)=(#charge)).
% :- modeb(1,(+energy)=(#energy)).
% :- modeb(1,(+hydrophob)=(#hydrophob)).

:- modeb(*,benzene(+drug,-ring)).
:- modeb(*,carbon_5_aromatic_ring(+drug,-ring)).
:- modeb(*,carbon_6_ring(+drug,-ring)).
:- modeb(*,hetero_aromatic_6_ring(+drug,-ring)).
:- modeb(*,hetero_aromatic_5_ring(+drug,-ring)).
:- modeb(*,ring_size_6(+drug,-ring)).
:- modeb(*,ring_size_5(+drug,-ring)).
:- modeb(*,nitro(+drug,-ring)).
:- modeb(*,methyl(+drug,-ring)).
:- modeb(*,anthracene(+drug,-ringlist)).
:- modeb(*,phenanthrene(+drug,-ringlist)).
:- modeb(*,ball3(+drug,-ringlist)).

:- modeb(1,member(+ring,+ringlist)).
:- modeb(1,connected(+ring,+ring)).

:- modeb(1,lin_regress1(+activity,+energy,#number,#number,#number,-linerror1)).
:- modeb(1,lin_regress1(+activity,+hydrophob,#number,#number,#number,-linerror1)).
:- modeb(1,lin_regress1(+activity,+ind,#number,#number,#number,-linerror1)).
:- modeb(1,lin_regress1(+activity,+hattr,#number,#number,#number,-linerror1)).


:- modeb(1,lin_regress2(+activity,+energy,+hydrophob,#number,#number,#number,#number,-linerror2)).
:- modeb(1,lin_regress2(+activity,+energy,+ind,#number,#number,#number,#number,-linerror2)).
:- modeb(1,lin_regress2(+activity,+energy,+hattr,#number,#number,#number,#number,-linerror2)).
:- modeb(1,lin_regress2(+activity,+hydrophob,+ind,#number,#number,#number,#number,-linerror2)).
:- modeb(1,lin_regress2(+activity,+hydrophob,+hattr,#number,#number,#number,#number,-linerror2)).
:- modeb(1,lin_regress2(+activity,+hattr,+ind,#number,#number,#number,#number,-linerror2)).

:- modeb(1,lin_regress3(+activity,+energy,+hydrophob,+hattr,#number,#number,#number,#number,#number,-linerror3)).
:- modeb(1,lin_regress3(+activity,+energy,+hydrophob,+ind,#number,#number,#number,#number,#number,-linerror3)).
:- modeb(1,lin_regress3(+activity,+hydrophob,+hattr,+ind,#number,#number,#number,#number,#number,-linerror3)).
:- modeb(1,lin_regress3(+activity,+energy,+hattr,+ind,#number,#number,#number,#number,#number,-linerror3)).

:- modeb(1,lin_regress4(+activity,+energy,+hydrophob,+hattr,+ind,#number,#number,#number,#number,#number,#number,-linerror4)).

:- modeb(1,expected_value(+activity,#value,#value,-experror)).


:- modeb(*,lte(+linerror1,#number)).
:- modeb(*,lte(+linerror2,#number)).
:- modeb(*,lte(+linerror3,#number)).
:- modeb(*,lte(+linerror4,#number)).
:- modeb(*,lte(+experror,#number)).


:- lazy_evaluate(lin_regress1/6).
:- lazy_evaluate(lin_regress2/8).
:- lazy_evaluate(lin_regress3/10).
:- lazy_evaluate(lin_regress4/12).

:- lazy_evaluate(expected_value/4).

:- positive_only(lin_regress1/6).
:- positive_only(lin_regress2/8).
:- positive_only(lin_regress3/10).
:- positive_only(lin_regress4/12).

:- positive_only(expected_value/4).


:- commutative(lin_regress1/6).
:- commutative(lin_regress2/8).
:- commutative(lin_regress3/10).
:- commutative(lin_regress4/12).


:- determination(active/2,atm/5).
:- determination(active/2,bond/4).
:- determination(active/2,lumo/2).
:- determination(active/2,logp/2).
:- determination(active/2,hansch_attr/2).
:- determination(active/2,ind1/2).

% :- determination(active/2,benzene/2).
% :- determination(active/2,carbon_5_aromatic_ring/2).
% :- determination(active/2,carbon_6_ring/2).
% :- determination(active/2,hetero_aromatic_6_ring/2).
% :- determination(active/2,hetero_aromatic_5_ring/2).
% :- determination(active/2,ring_size_6/2).
% :- determination(active/2,ring_size_5/2).
% :- determination(active/2,nitro/2).
% :- determination(active/2,methyl/2).
% :- determination(active/2,anthracene/2).
% :- determination(active/2,phenanthrene/2).
% :- determination(active/2,ball3/2).
% :- determination(active/2,member/2).
% :- determination(active/2,connected/2).

:- determination(active/2,gteq/2).
:- determination(active/2,lteq/2).

:- determination(active/2,'='/2).
:- determination(active/2,lin_regress1/6).
:- determination(active/2,lin_regress2/8).
% :- determination(active/2,lin_regress3/10).
% :- determination(active/2,lin_regress4/12).
:- determination(active/2,expected_value/4).
:- determination(active/2,lte/2).

:- set(i,3).
:- set(verbose,1).
:- set(evalfn,user).		% user defined cost function
:- set(refine,user).		% user defined refinement operator
:- set(depth,50).
:- set(clauselength,6).
:- set(nodes,5000).
:- set(record,true).
:- set(recordfile,'trace_all').


drug(D):-
        name(D,[100|X]), name(Num,X), integer(Num),
        Num >= 1, Num =< 230.

atomid(A):-
        name(A,[100|X]),
        append([95|Y],Z,X),
        name(N1,Y),
        name(N2,Z),
        integer(N1), integer(N2),
        N2 >= 1, N2 =< 230,
        N1 =< 500.

element(br).
element(c).
element(cl).
element(f).
element(h).
element(i).
element(n).
element(o).
element(s).

% background knowledge

gteq(X,Y):-
        number(X), number(Y),
        X >= Y, !.
gteq(X,X):-
        number(X).

lteq(X,Y):-
        number(X), number(Y),
        X =< Y, !.
lteq(X,X):-
        number(X).

member(X,[X|_]).
member(X,[_|T]):-
        member(X,T).

connected(Ring1,Ring2):-
        Ring1 \== Ring2,
        member(Atom,Ring1),
        member(Atom,Ring2), !.


list([]).
list([_|_]).

value(X,X):-
	number(X), !.
value(Fx,X):-
	Fx =.. [F,X], number(X), !.


% linear regression with 4 independent variables
% testing
lin_regress4(Yval,X1val,X2val,X3val,X4val,M1,M2,M3,M4,C,Sigma,Error):-
        nonvar(Yval), nonvar(X1val), nonvar(X2val), nonvar(X3val), nonvar(X4val),
        value(Yval,Y), value(X1val,X1), value(X2val,X2), value(X3val,X3), value(X4val,X4),
        number(M1), number(M2), number(M3), number(M4), number(C), number(Sigma), !,
        Y0 is M1*X1 + M2*X2 + M3*X3 + M4*X4 + C,
	get_residual(Y,Y0,Sigma,Residual),
        abs_val(Residual,Error).

% call to a C function to obtain regression line
lin_regress4(Y,X1,X2,X3,X4,M1,M2,M3,M4,C,Sigma,0.0):-
        list(Y), list(X1), list(X2), list(X3), list(X4),
	npoints(Y,NPoints),
	NPoints  >= 3*(4+1),		% at least 3*(k+1) (k=4) points 
        var(M1), var(M2), var(M3), var(M4), var(C), !,
        l_regressn(5,[Y,X1,X2,X3,X4],[C,M1,M2,M3,M4],Sigma,Fval,Rsq),
        write('[Parameters = '), write(C/M1/M2/M3/M4/Fval/Rsq), write(']'), nl,
        Fval > 6.63,
        Rsq > 0.80.

% for prediction
lin_regress4(Yval,X1val,X2val,X3val,X4val,M1,M2,M3,M4,C,_,0.0):-
        var(Yval), nonvar(X1val), nonvar(X2val), nonvar(X3val), nonvar(X4val),
        value(X1val,X1), value(X2val,X2), value(X3val,X3), value(X4val,X4),
        number(M1), number(M2), number(M3), number(M4), number(C), !,
        Yval is M1*X1 + M2*X2 + M3*X3 + M4*X4 + C.

% otherwise: for bottom clause
lin_regress4(Y,X1,X2,X3,X4,0,0,0.0,0,Y,0,0.0):-
	number(Y), not(list(X1)), not(list(X2)), not(list(X3)), not(list(X4)),
        X1 \= Y, X2 \= Y, X3 \=Y, X4 \= Y, X1 \= X2, X1 \= X3, X1 \= X4,
        X2 \= X3, X2\=X4, X3 \= X4.
% lin_regress4(Y,X1,X2,M1,M2,C,S,E):-
	% not(list(Y)), not(list(X1)), not(list(X2)), not(list(X3)), not(list(X4)),
        % X1 \= Y, X2 \= Y, X1 \= X2,
        % skolem_var(M1), skolem_var(M2),
        % skolem_var(C), skolem_var(S), skolem_var(E).

% linear regression with 3 independent variables
% testing
lin_regress3(Yval,X1val,X2val,X3val,M1,M2,M3,C,Sigma,Error):-
        nonvar(Yval), nonvar(X1val), nonvar(X2val), nonvar(X3val),
        value(Yval,Y), value(X1val,X1), value(X2val,X2), value(X3val,X3),
        number(M1), number(M2), number(M3), number(C), number(Sigma), !,
        Y0 is M1*X1 + M2*X2 + M3*X3 + C,
	get_residual(Y,Y0,Sigma,Residual),
        abs_val(Residual,Error).

% call to a C function to obtain regression line
lin_regress3(Y,X1,X2,X3,M1,M2,M3,C,Sigma,0.0):-
        list(Y), list(X1), list(X2), list(X3),
	npoints(Y,NPoints),
	NPoints  >= 3*(3+1),		% at least 3*(k+1) (k=3) points
        var(M1), var(M2), var(M3), var(C), !,
        l_regressn(4,[Y,X1,X2,X3],[C,M1,M2,M3],Sigma,Fval,Rsq),
        write('[Parameters = '), write(C/M1/M2/M3/Fval/Rsq), write(']'), nl,
        Fval > 6.63,
        Rsq > 0.80.

% for prediction
lin_regress3(Yval,X1val,X2val,X3val,M1,M2,M3,C,_,_):-
        var(Yval), nonvar(X1val), nonvar(X2val), nonvar(X3val),
        value(X1val,X1), value(X2val,X2), value(X3val,X3),
        number(M1), number(M2), number(M3), number(C), !,
        Yval is M1*X1 + M2*X2 + M3*X3 + C.

% otherwise: for bottom clause
lin_regress3(Y,X1,X2,X3,0,0,0.0,Y,0,0.0):-
	number(Y), not(list(X1)), not(list(X2)), not(list(X3)),
        X1 \= Y, X2 \= Y, X3 \=Y, X1 \= X2, X1 \= X3, X2 \= X3.
% lin_regress3(Y,X1,X2,M1,M2,C,S,E):-
	% not(list(Y)), not(list(X1)), not(list(X2)), not(list(X3)),
        % X1 \= Y, X2 \= Y, X1 \= X2,
        % skolem_var(M1), skolem_var(M2),
        % skolem_var(C), skolem_var(S), skolem_var(E).


% linear regression with 2 independent variables
% testing
lin_regress2(Yval,X1val,X2val,M1,M2,C,Sigma,Error):-
	nonvar(Yval), nonvar(X1val), nonvar(X2val),
	value(Yval,Y), value(X1val,X1), value(X2val,X2),
        number(M1), number(M2), number(C), number(Sigma), !,
        Y0 is M1*X1 + M2*X2 + C,
	get_residual(Y,Y0,Sigma,Residual),
        abs_val(Residual,Error).

% call to a C function to obtain regression line
lin_regress2(Y,X1,X2,M1,M2,C,Sigma,0.0):-
        list(Y), list(X1), list(X2),
	npoints(Y,NPoints),
	NPoints  >= 3*(2+1),	% at least 3*(k+1) (k=2) points
        var(M1), var(M2), var(C), !,
        l_regressn(3,[Y,X1,X2],[C,M1,M2],Sigma,Fval,Rsq),
	write('[Parameters = '), write(C/M1/M2/Fval/Rsq), write(']'), nl,
	Fval > 6.63,
	Rsq > 0.80.

% for prediction
lin_regress2(Yval,X1val,X2val,M1,M2,C,_,0.0):-
	var(Yval), nonvar(X1val), nonvar(X2val),
	value(X1val,X1), value(X2val,X2),
        number(M1), number(M2), number(C), !,
        Yval is M1*X1 + M2*X2 + C.

% otherwise: for bottom clause
lin_regress2(Y,X1,X2,0,0,Y,0,0.0):-
	number(Y), not(list(X1)), not(list(X2)),
        X1 \= Y, X2 \= Y, X1 \= X2.
% lin_regress2(Y,X1,X2,M1,M2,C,S,E):-
	% not(list(Y)), not(list(X1)), not(list(X2)),
        % X1 \= Y, X2 \= Y, X1 \= X2,
        % skolem_var(M1), skolem_var(M2),
        % skolem_var(C), skolem_var(S), skolem_var(E).


% linear regression with 1 independent variable
% testing
lin_regress1(Yval,Xval,M,C,Sigma,Error):-
	nonvar(Yval), nonvar(Xval), value(Yval,Y), value(Xval,X), 
        number(M), number(C), number(Sigma), !,
        Y1 is M*X + C,
	get_residual(Y,Y1,Sigma,Residual),
	abs_val(Residual,Error).

% call to a C function to obtain regression line
lin_regress1(Y,X,M,C,Sigma,0.0):-
        list(X), list(Y), !,
	npoints(Y,NPoints),
	NPoints  >=3*(1+1),	% at least 3*(k+1) (k=1) points 
        l_regressn(2,[Y,X],[C,M],Sigma,Fval,Rsq),
	write('[Parameters = '), write(C/M/Fval/Rsq), write(']'), nl,
	Fval > 6.63,
	Rsq > 0.80.

% for prediction
lin_regress1(Yval,Xval,M,C,_,0.0):-
	var(Yval), nonvar(Xval), value(Xval,X), 
        number(M), number(C), !,
        Yval is M*X + C.

% otherwise: for bottom clause
lin_regress1(Y,X,0,Y,0,0.0):-
	number(Y), not(list(X)),
        X \= Y.

% lin_regress1(Y,X,M,C,S,E):-
	% not(list(Y)), not(list(X)),
        % X \= Y,
        % skolem_var(M), skolem_var(C), skolem_var(E), skolem_var(E).


abs_val(X,X):- X >= 0, !.
abs_val(X,Y):- Z is 1.0 * X, Z < 0, Y is -Z.

% number of points with numerical values
npoints([Pos,Neg],Points):-
	countn(Pos,Points).

countn([],0).
countn([H|T],Points):-
	number(H), !,
	countn(T,P1),
	Points is P1 + 1.
countn([_|T],Points):-
	countn(T,Points).

lte(X,Y):- number(X), not(var(Y)), number(Y), !, X =< Y.
lte(X,Y1):-
        ubound(X,Y1).

gte(X,Y):- number(X), not(var(Y)), number(Y), !, X >= Y.
gte(X,Y1):-
        lbound(X,Y1).

ubound(0.0,2.0):- !.
ubound(X,X):- X \= 0.0.
lbound(0.0,-2.0):- !.
lbound(X,X):- X \= 0.0.

% Expected value of a variable
% for testing
expected_value(Y,Y1,Sigma,Error):-
	number(Y), number(Y1), number(Sigma), !,
	get_residual(Y,Y1,Sigma,Residual),
	abs_val(Residual,Error).

expected_value(Y,Y1,Sigma,Error):-
	not(var(Y)), number(Y1), number(Sigma), !,
	Y = L-U,
	MidPt is (L+U)/2,
	get_residual(MidPt,Y1,Sigma,Residual),
	abs_val(Residual,Error).

% call to a function to obtain expected value
expected_value(Y,Y1,Sigma,0.0):-
	list(Y), var(Y1), !,
	find_mean(Y,Y1,Sigma),
	write('[Mean = '), write(Y1/Sigma), write(']'), nl.

% for prediction
expected_value(Y,Y1,Sigma,0.0):-
	var(Y), number(Y1), number(Sigma), !,
	L is Y1 - 2*Sigma,
	U is Y1 + 2*Sigma,
	Y = L-U.

% otherwise: for bottom clause
expected_value(Y,Y,0.0,0.0):-
	number(Y), !.

get_residual(_,_,Sigma,0):-
	Diff is Sigma - 0.0,
	abs_val(Diff,ADiff),
	ADiff =< 0.00001, !.
get_residual(Y,Y1,Sigma,R):-
	R is (Y - Y1)/Sigma.

find_mean([Points|_],Mean,Sigma):-
	find_sums(Points,0,0.0,0.0,N,S,SSq),
	N >= 5,			% at least 10 points in a cluster
	Mean is S/N,
	SigmaSq is SSq/N - (Mean*Mean),
	Sigma is sqrt(SigmaSq),
	I is Mean/Sigma,
	write(Mean/Sigma/I), nl,
	I >= 2.0.

find_sums([],N,S,Sq,N,S,Sq):- !.
find_sums([X|T],N1,S1,Sq1,N,S,Sq):-
	number(X), !,
	S2 is S1 + X,
	Sq2 is Sq1 + X*X,
	N2 is N1 + 1,
	find_sums(T,N2,S2,Sq2,N,S,Sq), !.
find_sums([X|T],N1,S1,Sq1,N,S,Sq):-
	find_sums(T,N1,S1,Sq1,N,S,Sq), !.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Files

% :- [htrain_struc].
:- [outliers_struc].
% :- [ring_struc].
:- [ind1].
:- [inda].
:- [lumo].
:- [logp].
:- [hansch].
% :- [groups].



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Constraints


false:- 
	hypothesis(_,true,_), !.



refine(false,active(A,Activity)).

% for regression and clustering
refine(active(A,Activity),Clause):-
	member(P1,[true,lumo(A,Lumo),[lumo(A,Lumo),lteq(Lumo,Val1)]]),
	member(P2,[true,logp(A,LogP),[logp(A,LogP),lteq(LogP,Val2)]]),
	member(P3,[true,hansch_attr(A,H),[hansch_attr(A,H),lteq(H,Val3)]]),
	member(P4,[true,atm(A,Id,El,AT,Ch),[atm(A,Id,El,AT,Ch),lteq(Ch,Val0)]]),
	member(P5,[true,bond(A,Id,Id1,BT)]),
	member(P6,[true,ind1(A,I)]),
	get_preds([P1,P2,P3,P4,P5,P6],Preds),
	rm_redun(Preds,Lits1),
	% Lits1 \= [],
	get_pred_model(Activity,Lits1,Model),
	conc(Model,Lits1,BodyL),
	list_to_goals(BodyL,Body),
	Clause = (active(A,Activity):- Body).

% for classification
% refine(active(A,Activity),Clause):-
	% member(P1,[true,[lumo(A,Lumo),lteq(Lumo,Val1)]]),
	% member(P2,[true,[logp(A,LogP),lteq(LogP,Val2)]]),
	% member(P3,[true,[hansch_attr(A,H),lteq(H,Val3)]]),
	% member(P4,[true,atm(A,Id,El,AT,Ch),[atm(A,Id,El,AT,Ch),lteq(Ch,Val0)]]),
	% member(P5,[true,bond(A,Id,Id1,BT)]),
	% member(P6,[true,ind1(A,1),ind1(A,0)]),
	% get_preds([P1,P2,P3,P4,P5,P6],Preds),
        % rm_redun(Preds,Lits1),
        % Lits1 \= [],
        % list_to_goals(Lits1,Body),
        % Clause = (active(A,Activity):- (Activity=vlow,(Body))).


get_pred_model(Y,Preds,Model):-
	not(member(lumo(A,Lumo),Preds)),
	not(member(logp(A,LogP),Preds)),
	not(member(hansch_attr(A,H),Preds)),
	not(member(ind1(A,I),Preds)), !,
	Model=[expected_value(Y,Y1,Sigma,Error),lte(Error,N)].

% only use bi and univariate linear models
% get_pred_model(Y,Preds,Model):-
% 	member(lumo(A,Lumo),Preds),
% 	member(logp(A,LogP),Preds),
% 	member(hansch_attr(A,H),Preds),
% 	member(ind1(A,I),Preds), !,
% 	Model=[lin_regress4(Y,Lumo,LogP,H,I,M1,M2,M3,M4,Const,Sigma,Error),lte(Error,N)].
% get_pred_model(Y,Preds,Model):-
% 	member(lumo(A,Lumo),Preds),
% 	member(logp(A,LogP),Preds),
% 	member(ind1(A,I),Preds), !,
% 	Model=[lin_regress3(Y,Lumo,LogP,I,M1,M2,M3,Const,Sigma,Error),lte(Error,N)].
% get_pred_model(Y,Preds,Model):-
% 	member(lumo(A,Lumo),Preds),
% 	member(logp(A,LogP),Preds),
% 	member(hansch_attr(A,H),Preds), !,
% 	Model=[lin_regress3(Y,Lumo,LogP,H,M1,M2,M3,Const,Sigma,Error),lte(Error,N)].
% get_pred_model(Y,Preds,Model):-
% 	member(lumo(A,Lumo),Preds),
% 	member(hansch_attr(A,H),Preds),
% 	member(ind1(A,I),Preds), !,
% 	Model=[lin_regress3(Y,Lumo,H,I,M1,M2,M3,Const,Sigma,Error),lte(Error,N)].
% get_pred_model(Y,Preds,Model):-
% 	member(logp(A,LogP),Preds),
% 	member(hansch_attr(A,H),Preds),
% 	member(ind1(A,I),Preds), !,
% 	Model=[lin_regress3(Y,LogP,H,I,M1,M2,M3,Const,Sigma,Error),lte(Error,N)].

get_pred_model(Y,Preds,Model):-
	member(lumo(A,Lumo),Preds),
	member(logp(A,LogP),Preds), !,
	Model=[lin_regress2(Y,Lumo,LogP,M1,M2,Const,Sigma,Error),lte(Error,N)].
get_pred_model(Y,Preds,Model):-
	member(lumo(A,Lumo),Preds),
	member(ind1(A,I),Preds), !,
	Model=[lin_regress2(Y,Lumo,I,M1,M2,Const,Sigma,Error),lte(Error,N)].
get_pred_model(Y,Preds,Model):-
	member(logp(A,LogP),Preds), 
	member(ind1(A,I),Preds), !,
	Model=[lin_regress2(Y,LogP,I,M1,M2,Const,Sigma,Error),lte(Error,N)].
get_pred_model(Y,Preds,Model):-
	member(logp(A,LogP),Preds), 
	member(hansch_attr(A,H),Preds), !,
	Model=[lin_regress2(Y,LogP,H,M1,M2,Const,Sigma,Error),lte(Error,N)].
get_pred_model(Y,Preds,Model):-
	member(lumo(A,Lumo),Preds),
	member(hansch_attr(A,H),Preds), !,
	Model=[lin_regress2(Y,Lumo,H,M1,M2,Const,Sigma,Error),lte(Error,N)].
get_pred_model(Y,Preds,Model):-
	member(hansch_attr(A,H),Preds), 
	member(ind1(A,I),Preds), !,
	Model=[lin_regress2(Y,H,I,M1,M2,Const,Sigma,Error),lte(Error,N)].
get_pred_model(Y,Preds,Model):-
	member(lumo(A,Lumo),Preds), !,
	Model=[lin_regress1(Y,Lumo,M1,Const,Sigma,Error),lte(Error,N)].
get_pred_model(Y,Preds,Model):-
	member(logp(A,LogP),Preds), !,
	Model=[lin_regress1(Y,LogP,M1,Const,Sigma,Error),lte(Error,N)].
get_pred_model(Y,Preds,Model):-
	member(hansch_attr(A,H),Preds), !,
	Model=[lin_regress1(Y,H,M1,Const,Sigma,Error),lte(Error,N)].
get_pred_model(Y,Preds,Model):-
	member(ind1(A,I),Preds), !,
	Model=[lin_regress1(Y,I,M1,Const,Sigma,Error),lte(Error,N)].


get_preds([],[]).
get_preds([[Lit|T]|Lits],Preds):-
	!,
	get_preds([Lit|T],P1),
	get_preds(Lits,P2),
	conc(P2,P1,Preds).
get_preds([Lit|Lits],[Lit|Preds]):-
	get_preds(Lits,Preds).

conc(L,[],L).
conc(L,[H|T],[H|T1]):-
	conc(L,T,T1).

rm_redun([],[]):- !.
rm_redun([true|Lits],L):-
	!,
	rm_redun(Lits,L).
rm_redun([Lit|Lits],[Lit|L]):-
	rm_redun(Lits,L).

list_to_goals([Lit],(Lit)):- !.
list_to_goals([Lit|Lits],(Lit,Goals)):-
	list_to_goals(Lits,Goals).

% user-specified cost of a clause: here the same as the mean square error
cost((Head:-Body),Label,Cost):-
	mse(Head,Body,Cost),
	Cost \= undef, !.
cost(_,[P,N,L],Cost):-
	P > L, N = 0, !,
	Cost is -P.
cost(_,_,inf).

% variable is computed by an equation in the body with error E

calculated(Variable,(expected_value(Y,_,_,E),Lits),E,Lits):-
	Variable == Y, !.
calculated(Variable,(lin_regress1(Y,_,_,_,_,E),Lits),E,Lits):-
	Variable == Y, !.
calculated(Variable,(lin_regress2(Y,_,_,_,_,_,_,E),Lits),E,Lits):-
	Variable == Y, !.
calculated(Variable,(lin_regress3(Y,_,_,_,_,_,_,_,_,E),Lits),E,Lits):-
	Variable == Y, !.
calculated(Variable,(lin_regress4(Y,_,_,_,_,_,_,_,_,_,_,E),Lits),E,Lits):-
	Variable == Y, !.
calculated(Variable,(lin_regress5(Y,_,_,_,_,_,_,_,_,_,_,_,_,E),Lits),E,Lits):-
	Variable == Y, !.
calculated(Variable,(_,Lits),E,Left):-
	calculated(Variable,Lits,E,Left), !.
calculated(Variable,expected_value(Y,_,_,E),E,true):-
	Variable == Y, !.
calculated(Variable,lin_regress1(Y,_,_,_,_,E),E,true):-
	Variable == Y, !.
calculated(Variable,lin_regress2(Y,_,_,_,_,_,_,E),E,true):-
	Variable == Y, !.
calculated(Variable,lin_regress3(Y,_,_,_,_,_,_,_,_,E),E,true):-
	Variable == Y, !.
calculated(Variable,lin_regress4(Y,_,_,_,_,_,_,_,_,_,_,E),E,true):-
	Variable == Y, !.
calculated(Variable,lin_regress5(Y,_,_,_,_,_,_,_,_,_,_,_,_,E),E,true):-
	Variable == Y, !.

% get mean-square error of a clause over positive examples
mse(Head,Body,inf):-
	ground((Head:-Body)), !.
mse(active(_,Activity),Body,undef):-
	ground(Activity),
	not(number(Activity)), !.
mse(active(_,Activity),Body,inf):-
	not(calculated(Activity,Body,_,_)), !.
mse(active(Drug,Activity),Body,E):-
	retract_all(mse,sse(_)),
	retract_all(mse,count(_)),
	recorda(mse,sse(0.0),_),
	recorda(mse,count(0),_),
	example(pos,_,active(Drug,Activity1)),
	once(Body),
	get_error(Activity1,Activity,Error),
	SE is (Error)^2,
	recorded(mse,sse(S),Key),
	recorded(mse,count(N),Key1),
	erase(Key), erase(Key1),
	S1 is S + SE,
	N1 is N + 1,
	recorda(mse,sse(S1),_),
	recorda(mse,count(N1),_),
	fail.
mse(_,_,E):-
	recorded(mse,sse(S),Key),
	recorded(mse,count(N),Key1),
	erase(Key), erase(Key1),
	N > 0, !,
	E is S/N.
mse(_,_,inf).

get_error(Y,L-U,E):-
	number(Y),
	!,
	Y1 is (L+U)/2,
	E is Y - Y1.
get_error(Y,Y1,E):-
	number(Y),
	number(Y1), !,
	E is Y - Y1.
	
retract_all(Area,Fact):-
	recorded(Area,Fact,Key),
	erase(Key),
	fail.
retract_all(_,_).
