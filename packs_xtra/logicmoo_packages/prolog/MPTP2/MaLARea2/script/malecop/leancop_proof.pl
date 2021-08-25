%% File: leancop_proof.pl  -  Version: 1.1  -  Date: 3 July 2009
%%
%% Purpose: Presentation of connection proof found by leanCoP
%%
%% Author:  Jens Otten
%% Web:     www.leancop.de
%%
%% Usage:   leancop_proof(M,P). % where M is a matrix and P is the
%%                              %  connection proof found by leanCoP
%%                              %  e.g. M=[[q(a)],[-p],[p, -q(X)]],
%%                              %  P=[[q(a)],[[-(q(a)),p],[[-(p)]]]]
%%
%% Copyright: (c) 2009 by Jens Otten
%% License:   GNU General Public License


:- assert(proof(readable)). % compact, connect, readable


%%% output of leanCoP proof

leancop_proof(Mat,Proof) :-
    proof(compact) -> leancop_compact_proof(Proof) ;
    proof(connect) -> leancop_connect_proof(Mat,Proof) ;
    leancop_readable_proof(Mat,Proof).

%%% print readable proof

leancop_readable_proof(Mat,Proof) :-
    print('------------------------------------------------------'),
    nl,
    print_explanations,
    print('Proof:'), nl, print('------'), nl, nl,
    print('Translation into (disjunctive) clausal form:'), nl,
    print_clauses(Mat,1,Mat1),
    print_introduction,
    calc_proof(Mat1,Proof,Proof1),
    print_proof(Proof1), nl,
    print_ending,
    print('------------------------------------------------------'),
    nl.

%%% print compact proof

leancop_compact_proof(Proof) :-
    print('------------------------------------------------------'),
    nl,
    print('Compact Proof:'), nl,
    print('--------------'), nl,
    print(Proof), nl,
    print('------------------------------------------------------'),
    nl.

%%% print connection proof

leancop_connect_proof(Mat,Proof) :-
    print('------------------------------------------------------'),
    nl,
    print('Proof for the following clauses:'), nl,
    print_clauses(Mat,1,Mat1),
    calc_proof(Mat1,Proof,Proof1),
    print('Connection Proof:'), nl,
    print('-----------------'), nl,
    print_connect_proof(Proof1),
    print('------------------------------------------------------'),
    nl.

print_connect_proof([(Cla,Num,Sub)|Proof]) :-
    print_connect_proof_step([],Cla,Num,Sub),
    print_connect_proof(Proof,[1]).

print_connect_proof([],_).

print_connect_proof([[(Cla,Num,Sub)|Proof]|Proof2],[I|J]) :-
    print_connect_proof_step([I|J],Cla,Num,Sub),
    print_connect_proof(Proof,[1,I|J]), I1 is I+1,
    print_connect_proof(Proof2,[I1|J]).

print_connect_proof_step(I,Cla,Num,Sub) :-
    append(I,[1],I1), print_step(I1), print('  '), print(Cla),
    ( Num=(R:N) -> append(_,[H|T],I1), N1 is N+1, length([H|T],N1),
      ( R=r -> print('   (reduction:'), print_step(T) ;
               print('   (lemmata:'), print_step(T) ) ;
      print('   ('), print(Num) ), print(')  '),
    ( Sub=[[],_] -> true ; print('substitution:'), print(Sub) ), nl.


%%% calculate leanCoP proof

calc_proof(Mat,[Cla|Proof],[(Cla1,Num,Sub)|Proof1]) :-
    ((Cla=[#|Cla1];Cla=[-!|Cla1]) -> true ; Cla1=Cla),
    clause_num_sub(Cla1,[],[],Mat,1,Num,Sub),
    calc_proof(Cla1,[],[],Mat,Proof,Proof1).

calc_proof(_,_,_,_,[],[]).

calc_proof(Cla,Path,Lem,Mat,[[Cla1|Proof]|Proof2],Proof1) :-
    append(Cla2,[#|Cla3],Cla1), !, append(Cla2,Cla3,Cla4),
    append(Pro1,[[[-(#)]]|Pro2],Proof), append(Pro1,Pro2,Proof3),
    calc_proof(Cla,Path,Lem,Mat,[[Cla4|Proof3]|Proof2],Proof1).

calc_proof([Lit|Cla],Path,Lem,Mat,[[Cla1|Proof]|Proof2],Proof1) :-
    (-NegLit=Lit;-Lit=NegLit), append(Cla2,[NegL|Cla3],Cla1),
    NegLit==NegL, append(Cla2,Cla3,Cla4), length([_|Path],I) ->
      clause_num_sub(Cla1,Path,Lem,Mat,1,Num,Sub),
      Proof1=[[([NegLit|Cla4],Num,Sub)|Proof3]|Proof4],
      calc_proof(Cla4,[I:Lit|Path],Lem,Mat,Proof,Proof3),
      (Lem=[I:J:_|_] -> J1 is J+1 ; J1=1),
      calc_proof(Cla,Path,[I:J1:Lit|Lem],Mat,Proof2,Proof4).

%%% determine clause number and substitution

clause_num_sub([NegLit],Path,Lem,[],_,R:Num,[[],[]]) :-
    (-NegLit=Lit;-Lit=NegLit), member(Num:J:LitL,Lem), LitL==Lit ->
    R=J ; member(Num:NegL,Path), NegL==NegLit -> R=r.

clause_num_sub(Cla,Path,Lem,[Cla1|Mat],I,Num,Sub) :-
    append(Cla2,[L|Cla3],Cla1), append([L|Cla2],Cla3,Cla4),
    instance1(Cla,Cla4) ->
      Num=I, term_variables(Cla4,Var), copy_term(Cla4,Cla5),
      term_variables(Cla5,Var1), Cla=Cla5, Sub=[Var,Var1] ;
      I1 is I+1, clause_num_sub(Cla,Path,Lem,Mat,I1,Num,Sub).

instance1(A,B) :-
    \+ \+ (term_variables(A,VA), unify_with_occurs_check(A,B),
           term_variables(A,VB), VA==VB).

%%% print leanCoP proof

print_proof([(Cla,Num,Sub)|Proof]) :-
    print_clause([],Cla,Num,Sub),
    print_proof(Proof,[1]).

print_proof([],_).

print_proof([[(Cla,Num,Sub)|Proof]|Proof2],[I|J]) :-
    print_proof_step([I|J],Cla,Num,Sub),
    print_proof(Proof,[1,I|J]), I1 is I+1,
    print_proof(Proof2,[I1|J]).

%%% print leanCoP proof step

print_proof_step(I,[Lit|Cla],Num,Sub) :-
    print_assume(I,Lit),
    ( Num=(R:N) -> append(_,[H|T],I), length([H|T],N),
      (R=r -> print_redu(I,[H|T]) ; print_fact(I,[R|T])) ;
      print_clause(I,Cla,Num,Sub) ).

print_assume(I,Lit) :-
    print_step(I), print(' Assume '), (-NegLit=Lit;-Lit=NegLit) ->
    print(NegLit), print(' is '), print('false.'), nl.

print_clause(I,Cla,Num,Sub) :-
    print_sp(I), print(' Then clause ('), print(Num), print(')'),
    ( Sub=[[],[]] -> true ; print(' under the substitution '),
                            print(Sub), nl, print_sp(I) ),
    ( Cla=[] -> print(' is true.') ;
      print(' is false if at least one of the following is false:'),
      nl, print_sp(I), print(' '), print(Cla) ), nl.

print_redu(I,J) :-
    print_sp(I), print(' This is a contradiction to assumption '),
    print_step(J), print('.'), nl.

print_fact(I,J) :-
    print_sp(I), print(' This assumption has been refuted in '),
    print_step(J), print('.'), nl.

%%% print clauses, print step number, print spaces

print_clauses([],_,[]) :- nl.
print_clauses([[-(#)]|Mat],I,Mat1) :- !, print_clauses(Mat,I,Mat1).
print_clauses([Cla|Mat],I,Mat1) :-
    append(Cla2,[#|Cla3],Cla), append(Cla2,Cla3,Cla1),
    print_clauses([Cla1|Mat],I,Mat1).
print_clauses([Cla|Mat],I,[Cla|Mat1]) :-
    print(' ('), print(I), print(')  '),
    print(Cla), nl, I1 is I+1, print_clauses(Mat,I1,Mat1).

print_step([I]) :- print(I).
print_step([I,J|T]) :- print_step([J|T]), print('.'), print(I).

print_sp([]).
print_sp([I]) :- atom(I), !, print(' ').
print_sp([I]) :- I<1.
print_sp([I]) :- I>=1, print(' '), I1 is I/10, print_sp([I1]).
print_sp([I,J|T]) :- print_sp([J|T]), print(' '), print_sp([I]).

%%% print standard proof explanations, introduction/ending of proof

print_explanations :-
 print('Explanations for the proof presented below:'), nl,
 print('- to solve unsatisfiable problems they are negated'), nl,
 print('- equality axioms are added if required'), nl,
 print('- terms and variables are represented by Prolog terms'), nl,
 print('  and Prolog variables, negation is represented by -'), nl,
 print('- I^[t1,..,tn] represents the atom P_I(t1,..,tn)'), nl,
 print('  or the Skolem term f_I(t1,t2,..,tn) introduced'), nl,
 print('  during the clausal form translation'), nl,
 print('- the substitution [[X1,..,Xn],[t1,..,tn]] represents'), nl,
 print('  the assignments X1:=t1, .., Xn:=tn'), nl, nl.

print_introduction :-
 print('We prove that the given clauses are valid, i.e. for'), nl,
 print('a given substitution they evaluate to true for all'), nl,
 print('interpretations. The proof is by contradiction:'), nl,
 print('Assume there is an interpretation so that the given'), nl,
 print('clauses evaluate to false. Then in each clause there'), nl,
 print('has to be at least one literal that is false.'), nl, nl.

print_ending :-
 print('Therefore there is no interpretation that makes all'), nl,
 print('given clauses simultaneously false. Hence the given'), nl,
 print('clauses are valid.'), nl,
 print('                                             q.e.d.'), nl.
