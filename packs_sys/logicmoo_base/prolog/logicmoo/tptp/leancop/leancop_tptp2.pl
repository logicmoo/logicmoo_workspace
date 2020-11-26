%% File: leancop_tptp2.pl  -  Version: 1.1  -  Date: 3 July 2009
%%
%% Purpose: 1. Translate formula from TPTP into leanCoP syntax
%%          2. Add equality axioms to the given formula
%%
%% Author:  Jens Otten
%% Web:     www.leancop.de
%%
%% Usage: leancop_tptp2(X,F). % where X is a problem file in TPTP
%%                            %  syntax and F the translated formula
%%        leancop_equal(F,G). % where F is a formula and G the
%%                            %  formula with added equality axioms
%%
%% Copyright: (c) 2009 by Jens Otten
%% License:   GNU General Public License


% definitions of logical connectives and quantifiers

% leanCoP syntax
:- op(1130, xfy, <=>). % equivalence
:- op(1110, xfy, =>).  % implication
%                      % disjunction (;)
%                      % conjunction (,)
:- op( 500, fy, ~).    % negation
:- op( 500, fy, all).  % universal quantifier
:- op( 500, fy, ex).   % existential quantifier
:- op( 500,xfy, :).

% TPTP syntax
:- op(1130, xfy, <~>).  % negated equivalence
:- op(1110, xfy, <=).   % implication
:- op(1100, xfy, '|').  % disjunction
:- op(1100, xfy, '~|'). % negated disjunction
:- op(1000, xfy, &).    % conjunction
:- op(1000, xfy, ~&).   % negated conjunction
:- op( 500, fy, !).     % universal quantifier
:- op( 500, fy, ?).     % existential quantifier
:- op( 400, xfx, =).    % equality
:- op( 300, xf, !).     % negated equality (for !=)
:- op( 299, fx, $).     % for $true/$false

% TPTP syntax to leanCoP syntax mapping

op_tptp2((A<=>B),(A1<=>B1),   [A,B],[A1,B1]).
op_tptp2((A<~>B),~((A1<=>B1)),[A,B],[A1,B1]).
op_tptp2((A=>B),(A1=>B1),     [A,B],[A1,B1]).
op_tptp2((A<=B),(B1=>A1),     [A,B],[A1,B1]).
op_tptp2((A|B),(A1;B1),       [A,B],[A1,B1]).
op_tptp2((A'~|'B),~((A1;B1)), [A,B],[A1,B1]).
op_tptp2((A&B),(A1,B1),       [A,B],[A1,B1]).
op_tptp2((A~&B),~((A1,B1)),   [A,B],[A1,B1]).
op_tptp2(~A,~A1,[A],[A1]).
op_tptp2((! [V]:A),(all V:A1),     [A],[A1]).
op_tptp2((! [V|Vars]:A),(all V:A1),[! Vars:A],[A1]).
op_tptp2((? [V]:A),(ex V:A1),      [A],[A1]).
op_tptp2((? [V|Vars]:A),(ex V:A1), [? Vars:A],[A1]).
op_tptp2($true,(true___=>true___),      [],[]).
op_tptp2($false,(false___ , ~ false___),[],[]).
op_tptp2(A=B,~(A1=B),[],[]) :- \+var(A), A=(A1!).
op_tptp2(P,P,[],[]).


%%% translate into leanCoP syntax

leancop_tptp2(File,F) :- leancop_tptp2(File,'',[_],F,_).

leancop_tptp2(File,AxPath,AxNames,F,Con) :-
    open(File,read,Stream), ( fof2cop(Stream,AxPath,AxNames,A,Con)
    -> close(Stream) ; close(Stream), fail ),
    ( Con=[] -> F=A ; A=[] -> F=Con ; F=(A=>Con) ).

fof2cop(Stream,AxPath,AxNames,F,Con) :-
    read(Stream,Term),
    ( Term=end_of_file -> F=[], Con=[] ;
      ( Term=..[fof,Name,Type,Fml|_] ->
        ( \+member(Name,AxNames) -> true ; fml2cop([Fml],[Fml1]) ),
        ( Type=conjecture -> Con=Fml1 ; Con=Con1 ) ;
        ( Term=include(File), AxNames2=[_] ;
          Term=include(File,AxNames2) ) -> name(AxPath,AL),
          name(File,FL), append(AL,FL,AxL), name(AxFile,AxL),
          leancop_tptp2(AxFile,'',AxNames2,Fml1,_), Con=Con1
      ), fof2cop(Stream,AxPath,AxNames,F1,Con1),
      ( Term=..[fof,N,Type|_], (Type=conjecture;\+member(N,AxNames))
      -> (F1=[] -> F=[] ; F=F1) ; (F1=[] -> F=Fml1 ; F=(Fml1,F1)) )
    ).

fml2cop([],[]).
fml2cop([F|Fml],[F1|Fml1]) :-
    op_tptp2(F,F1,FL,FL1) -> fml2cop(FL,FL1), fml2cop(Fml,Fml1).


%%% add equality axioms

leancop_equal(F,F1) :-
    collect_predfunc([F],PL,FL), append(PL2,[(=,2)|PL3],PL),
    append(PL2,PL3,PL1) -> basic_equal_axioms(F0),
    subst_pred_axioms(PL1,F2), (F2=[] -> F3=F0 ; F3=(F0,F2)),
    subst_func_axioms(FL,F4), (F4=[] -> F5=F3 ; F5=(F3,F4)),
    ( F=(A=>C) -> F1=((F5,A)=>C) ; F1=(F5=>F) ) ; F1=F.

basic_equal_axioms(F) :-
    F=(( all X:(X=X) ),
       ( all X:all Y:((X=Y)=>(Y=X)) ),
       ( all X:all Y:all Z:(((X=Y),(Y=Z))=>(X=Z)) )).

% generate substitution axioms

subst_pred_axioms([],[]).
subst_pred_axioms([(P,I)|PL],F) :-
    subst_axiom(A,B,C,D,E,I), subst_pred_axioms(PL,F1), P1=..[P|C],
    P2=..[P|D], E=(B,P1=>P2), ( F1=[] -> F=A ; F=(A,F1) ).

subst_func_axioms([],[]).
subst_func_axioms([(P,I)|FL],F) :-
    subst_axiom(A,B,C,D,E,I), subst_func_axioms(FL,F1), P1=..[P|C],
    P2=..[P|D], E=(B=>(P1=P2)), ( F1=[] -> F=A ; F=(A,F1) ).

subst_axiom((all X:all Y:E),(X=Y),[X],[Y],E,1).
subst_axiom(A,B,[X|C],[Y|D],E,I) :-
    I>1, I1 is I-1, subst_axiom(A1,B1,C,D,E,I1),
    A=(all X:all Y:A1), B=((X=Y),B1).

% collect predicate & function symbols

collect_predfunc([],[],[]).
collect_predfunc([F|Fml],PL,FL) :-
    ( ( F=..[<=>|F1] ; F=..[=>|F1] ; F=..[;|F1] ; F=..[','|F1] ;
        F=..[~|F1] ; (F=..[all,_:F2] ; F=..[ex,_:F2]), F1=[F2] ) ->
      collect_predfunc(F1,PL1,FL1) ; F=..[P|Arg], length(Arg,I),
      I>0 ->  PL1=[(P,I)], collect_func(Arg,FL1) ; PL1=[], FL1=[] ),
    collect_predfunc(Fml,PL2,FL2),
    union1(PL1,PL2,PL), union1(FL1,FL2,FL).

collect_func([],[]).
collect_func([F|FunL],FL) :-
    ( \+var(F), F=..[F1|Arg], length(Arg,I), I>0 ->
      collect_func(Arg,FL1), union1([(F1,I)],FL1,FL2) ; FL2=[] ),
    collect_func(FunL,FL3), union1(FL2,FL3,FL).

union1([],L,L).
union1([H|L1],L2,L3) :- member(H,L2), !, union1(L1,L2,L3).
union1([H|L1],L2,[H|L3]) :- union1(L1,L2,L3).
