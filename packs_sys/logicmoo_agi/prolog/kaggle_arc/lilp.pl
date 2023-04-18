%-------------------------------------------------------------
%  LILP - Lambda Inductive Logic Programming                  
%  Learning predicate definitions from positive-only examples
%  Implemented in SICStus Prolog v.3
%-------------------------------------------------------------
%  (C) 1999 Zdravko Markov                                    
%      Department of Computer Science,                        
%      Central Connecticut State University                   
%      1615 Stanley Street, New Britain, CT 06050, U.S.A.     
%      Phone:  (860) 832-2711                                 
%      E-mail: markovz@ccsu.edu                               
%      URL: http://www.cs.ccsu.edu/~markov/          
%-------------------------------------------------------------
%  References: 
%  Markov, Z. Lambda-Subsumption and Its Application to Learning
%    from Positive-only Examples, in: S. Muggleton (ed.), 
%    Proceedings of ILP-96, Stockholm, August, 1996, 
%    Selected Papers, Lecture Notes in Artificial Intelligence,
%    Vol.1314, Springer, 1997, 377-396.
%  Markov, Z. Generalization under Implication by lambda-Subsumption,
%    in: David Page (ed.), Proceedings of ILP-98, Madison, 
%    Wisconsin, USA, July 22-24, 1998, Lecture Notes in Computer
%    Science, Vol. 1446, Springer, 1998, 215-224. 
%-------------------------------------------------------------
%  Top level: lilp(+TargetPredicate/Arity)
%  Prints the induced clauses for TargetPredicate  and
%  asserts them in the database in the following form:
%   lilp_clause(SeedExample,Clause,Cover)
%    SeedExample - the example from which Clause is induced;
%    Clause      - the induced clause;
%    Cover       - list of examples cover_lilped by Clause.
%-------------------------------------------------------------
%  Data required (asserted in the database before starting):
%  1. background([P1/A1,...,Pn/An]).
%     Bi - predicate name, Ai - arity
%  2. propositional([C1,C2,...,Cm]).
%     Ci - constants occurring in the examples, not
%     variabilized in the predicate definition. If there are
%     no such constants then specify propositional([]).
%  3. Clauses for P1,...,Pn and instances of TargetPredicate.
%-------------------------------------------------------------
%  Requirements to the data:
%  1. All data are specified in Datalog. Though functions are
%     allowed their structure is used for unification only,
%     but not for literal search.
%  2. The arguments of all atoms MUST NOT be Prolog lists.
%     Preferably use atomic type. Any non-list term is also
%     acceptable. See file "member3" for details.
%  3. In case of non-ground background knowledge ensure that
%     the predicates behave correctly when called with a
%     single instantiated agrument. See file "member3".
%  4. Do not use well-known or built-in predicate names (e.g.
%     append, member etc.) to avoid name clashes.
%  5. Since there are no argument types, preferably use
%     different domains for different types of arguments.
%-------------------------------------------------------------
%  Use of negative examples:
%  Generally negative examples are not necessary. However
%  there are some special cases when negatives help:
%  1. Specifying the more specific predicate to be learnt,
%     when two predicates (one more general than the other)
%     can be learned given same data (see the "min" example).
%  2. Allowing singleton head variables. See file "krki".
%-------------------------------------------------------------
%  Adjustable parameters:
%  1. MaxDet - Maximal number of determinate literals in a
%     clause. The default is MaxDet=3, specified in the goal
%     "for(0,N,3)" in the find_clause/2 predicate.
%  2. MaxDepth - Maximal number of body literals determining
%     a single head argument. The default is 5, specified in
%     the goal "for(1,Depth,5)" in the literal/3 predicate.
%-------------------------------------------------------------
%  Requirements to the Prolog system: SICStus Prolog v.3
%  If another version of Prolog is used then it must provide:
%   1. Standard Edinburgh syntax
%   2. Built-in definitions of:
%     * findall(Term,Goal,List) - List contains all possible
%         instantiations of Term, when executing Goal;
%     * \+(Goal) - negation by failure.
%-------------------------------------------------------------

lilp(F/N) :-
    retractall(lilp_clause(_,_,_)),
    statistics(runtime,_),
    functor(P,F,N),
    (\+(P), write('No examples for'-P),nl,!,fail ; true),
    uncover_lilped(P),
    nl, write('Searching clause for'-P),
    find_clause(P,Clause),
    nl, write1('Found'-Clause),nl,
    cover_lilp(Clause,Cover),
    assertz(lilp_clause(P,Clause,Cover)),
    fail.
lilp(F/N) :-
    nl,write('Check for clause subsumption...'),nl,
    functor(P,F,N),
    lilp_clause(P,Clause,Cover),
    jointly_subsumed(P,Cover),
    write1('Removing'-Clause), nl,
    retract(lilp_clause(P,_,_)),
    fail.
lilp(F/N) :-
    functor(P,F,N),
    lilp_clause(P,Clause,Cover),
    s_subsumed(P,Cover),
    write1('Removing'-Clause), nl,
    retract(lilp_clause(P,_,_)),
    fail.
lilp(F/N) :-
    statistics(runtime,[_,Ms]),
    Sec is Ms/1000,
    functor(P,F,N),
    nl,write('Clauses found in '),
    write(Sec), write(' seconds:'), nl,
    write('------------------------------'),nl,
    lilp_clause(P,Clause,_),
    write1(Clause),nl,
    fail.
lilp(_).

neg(fail).  % to avoid the unknown predicate exception

s_subsumed(P,Cover) :-
    lilp_clause(P1,_,Cover1),
    \+(P1=P),
    subset(Cover,Cover1), !.

jointly_subsumed(P,Cover) :-
    \+(s_subsumed(P,Cover)),
    findall(Cover1,(lilp_clause(P1,_,Cover1),\+(P1=P)),All),
    joint(All,Coverage),
    subset(Cover,Coverage), !.

joint([],[]) :- !.
joint([S|T],U) :-
    joint(T,U1),
    append(S,U1,U).

uncover_lilped(P) :-
   propositional(Constants), !,
   get_example(P,Constants),
   \+((clause(lilp_clause(_,_,Cover),true),member(P,Cover))).

get_example(P,Constants) :-
   call(P),
   P =.. [_|Args],
   intersct(Args,Constants).
get_example(P,Constants) :-
   call(P),
   P =.. [_|Args],
   \+(intersct(Args,Constants)).

cover_lilp((Head:-Body),Cover) :-
   findall(Head,(Head,call1(Body)),Cover), !.
cover_lilp(Head,Cover) :-
   findall(Head,Head,Cover).

%-------------------------------------------------------------
%  find_clause(+Instance,-Clause)
%  Find the best clause cover_lilping Instance.
%  Uses lambda models to check correctness and proof
%  complexity measure to prefer the best clause
%-------------------------------------------------------------
find_clause(Instance,Clause) :-
    propositional(Constants),
    head(Instance,Model),
    for(0,N,3),                                   % MaxDet = 3
    det_literals(N,Model,[],DetLs,NewModel),
    \+(member(Instance,DetLs)),
    nl,write(det_literals-DetLs),
    find_clause([Instance|DetLs],Model,NewModel,Constants,Clause), !.

find_clause([Instance|DetLiterals],Model,NewModel,Constants,_) :-
    Instance =.. [_|HeadArgs],
    find_negs(HeadArgs,Instance,Constants,Negs),
    length(NewModel,N),
    for_depth(0,Depth,N),
    body(NewModel,HeadArgs,[Instance|DetLiterals],Literals,Depth,Negs),
    length(Literals,Len),
    \+((clause(clause_found(_,OldLen,_),true),Len>OldLen)),
    append(DetLiterals,Literals,AllLiterals),
    variabilize([Instance|AllLiterals],ClauseLiterals,[],Constants),
    clausify(ClauseLiterals,Clause),
    evaluate(Clause,HeadArgs,Model,Negs,PC),
    check_min(Clause,Len,PC),
    fail.
find_clause(_,_,_,_,Clause) :-
    retract(clause_found(Clause,_,_)).

find_negs([],_,_,[]) :- !.
find_negs([A|T],P,Cs,[A|Negs]) :-
    functor(P,F,N),
    functor(Q,F,N),
    args(N,P,Cs,Q,QArgs),
    neg(Q),
    member(A,QArgs), !,
    find_negs(T,P,Cs,Negs).
find_negs([_|T],P,Cs,Negs) :-
    find_negs(T,P,Cs,Negs).

args(0,_,_,_,[]) :- !.
args(N,P,Cs,Q,T) :-
   arg(N,P,A),
   member(A,Cs), !,
   arg(N,Q,A),
   M is N-1,
   args(M,P,Cs,Q,T).
args(N,P,Cs,Q,[V|T]) :-
   arg(N,Q,V),
   M is N-1,
   args(M,P,Cs,Q,T).

for_depth(X,X,_).
for_depth(B,I,E)  :-
    check_depth(B),
    B1 is B+1,
    B1 < E+1,
    for_depth(B1,I,E).

check_depth(B) :-
    clause(clause_found(_,Len,_),true), !,
    B < Len.
check_depth(_).

check_min(_,_,PC) :-
    clause(clause_found(_,_,PC1),true),
    PC1=<PC, !.
check_min(Clause,Len,PC) :-
    if_retract(clause_found(_,_,_)),
    assertz(clause_found(Clause,Len,PC)), !.

det_literals(0,[],OldLs,OldLs,[]) :- !.
det_literals(0,Model,OldLs,OldLs,Model) :- !.
det_literals(N,OldModel,OldLs,Ls,Model)  :-
   det_literal(OldModel,OldLs,P,NewModel),
   M  is N-1,
   det_literals(M,NewModel,[P|OldLs],Ls,Model).

%-------------------------------------------------------------
%  body(+Model,HeadArgs,+OldLiteral,-Body,+Len,+Negs,+NoL)
%  Model       - [A1/M1,...,An/Mn], Mi - lambda models for Ai
%  OldLiterals - list of ground literals;
%  Body        - list of body literals;
%  Len         - number of literals in Body.
%-------------------------------------------------------------
body([],_,_,[],0,_) :-  !.
body([A/_|T],HeadArgs,OldLs,Ls,N,Negs) :-
    (member(A,Negs);\+(member(A,HeadArgs))),
    body(T,HeadArgs,OldLs,Ls,N,Negs).
body([A|T],HeadArgs,OldLs,NewLs,N,Negs) :-
    N>0, N1 is N-1,
    body(T,HeadArgs,OldLs,Ls,N1,Negs),
    literal(A,OldLs,P),
    if_add(P,Ls,NewLs).

if_add([P],Ls,Ls) :-
    member_arg(P,Ls), !.
if_add(P,Ls,NewLs) :-
    append(Ls,P,NewLs).

member_arg(P,[Q|_]) :-
    functor(P,F,N),
    functor(Q,F,N),
    arg_intersect(N,P,Q), !.
member_arg(P,[_|T]) :-
    member_arg(P,T).

arg_intersect(0,_,_) :- !.
arg_intersect(N,P,Q) :-
    arg(N,P,A),
    arg(N,Q,B),
    intersct(A,B), !,
    N1 is N-1,
    arg_intersect(N1,P,Q).

variabilize([],[],_,_) :- !.
variabilize([P|R],[Q|T],Subst,Save) :-
    P =.. [F|A],
    subst(A,B,Subst,Subst1,Save),
    Q =.. [F|B],
    variabilize(R,T,Subst1,Save).

subst([],[],S,S,_) :- !.
subst([C|T],[C|V],S,S1,Save) :-
    member(C,Save), !,         % Head prop. constants are preserved
    subst(T,V,S,S1,Save).
subst([X|T],[Var|V],S,S1,Save) :-
    member(X/Var,S), !,
    subst(T,V,S,S1,Save).
subst([[X]|T],[Var|V],S,S1,Save) :-
    member(X/Var,S), !,
    subst(T,V,S,S1,Save).
subst([X|T],[Var|V],S,S1,Save) :-
    member([X]/Var,S), !,
    subst(T,V,S,S1,Save).
subst([X|T],[Var|V],S,S1,Save) :-
    all_members(Y/Var,S),
    shared(X,Y),
    subst(T,V,S,S1,Save).
subst([X|T],[Var|V],S,S1,Save) :-
    subst(T,V,[X/Var|S],S1,Save).
subst([X|T],[C|V],S,S1,Save) :-    % Keep prop. constants lastly
    all_members(C,X),
    member(C,Save),
    subst(T,V,S,S1,Save).

shared(X,Y) :- all_members(Z,X),member(Z,Y), !.
shared(X,Y) :- member(X,Y), !.
shared(X,Y) :- member(Y,X), !.

%-------------------------------------------------------------
%  literal(+A/Model,+OldLs,-Literal)
%  A       - constant;
%  Model   - set of possible values of A;
%  OldLs   - list of ground literals;
%  Literal - ground atom from the background knowledge;
%-------------------------------------------------------------
literal(A/M,OldLs,Q) :-
    for(1,Depth,5),                             % MaxDepth = 5
    findall(P,literals(A/M,OldLs,Depth,P),Ps),
    Ps \== [], !,
    all_members(P,Ps),
    unfold(P,Q).

literals(A/Model,OldLs,Number,Ps)  :-
    get_literals(Number,A,0,P),
    findall(P,check_literals(A,Model,OldLs,P),Ps),
    Ps \== [].

unfold([[P]|T],[Q]) :- !,
    functor(P,F,N),
    functor(Q,F,N),
    columns(N,[[P]|T],Q).
unfold([P],Q) :-
    put_in_lists(P,Q).

columns(0,_,_) :- !.
columns(N,P,Q) :-
    nth_column(P,N,[],An),
    arg(N,Q,An), !,
    M is N-1,
    columns(M,P,Q).

nth_column([],_,A,A) :- !.
nth_column([[P]|T],N,R,An) :-
    arg(N,P,A),
    member(A,R), !,
    nth_column(T,N,R,An).
nth_column([[P]|T],N,R,An) :-
    arg(N,P,A),
    nth_column(T,N,[A|R],An).

get_literals(0,_,_,[]) :- !.
get_literals(D,A,I,[P|Q]) :-
    background(BK),
    membern(I1,F/N,BK),
    I1>I,
    functor(P,F,N),
    P =.. [F|L],
    all_members(A,L),
    D1 is D-1,
    get_literals(D1,A,I1,Q).

check_literals(_,_,[H|Ls],[P]) :-
    functor(H,F,1),
    functor(P,F,1), !,
    call(P),
    \+(in2(P,[H|Ls])).
check_literals(A,Model,OldLs,PQ) :-
    lambda_PQ(A,PQ,OldLs,X,LambdaPQ),
    submodel(LambdaPQ,X,Model,_,_).

lambda_PQ(_,[],_,_,true) :- !.
lambda_PQ(A,[P|Q],OldLs,X,(LambdaP,LambdaQ)) :-
    call(P),
    \+(in2(P,OldLs)),
    P=..[F|L],
    lambda(L,A,X,Lambda),
    LambdaP =.. [F|Lambda],
    lambda_PQ(A,Q,OldLs,X,LambdaQ).

det_literal([A/M],OldLs,P,NewModel) :- !,
    background(BK),
    propositional(Constants),
    all_members(F/N,BK),
    functor(P,F,N),
    P=..[F|Vars],
    membern(I,A,Vars),
    call(P),
    \+(member(P,OldLs)),
    diff(Vars,[A|Constants],NewVars),
    NewVars \== [],
    Q=..[F|NewVars],
    variabilize([P,Q],[VP,VQ],[],Constants),
    VQ=..[F|QVars],
    findall(QVars,(all_members(Ai,M),arg(I,VP,Ai),VP),Rows),
    get_columns(NewVars,1,Rows,NewModel).

det_literal(Model,OldLs,P,NewModel) :-
    background(BK),
    propositional(Constants),
    get_args(Model,OldVars),
    append(OldVars,Constants,AllVars), !,
    efface(A/M,Model,Model1),
    all_members(F/N,BK),
    functor(P,F,N),
    P=..[F|Vars],
    membern(I,A,Vars),
    call(P),
    \+(member(P,OldLs)),
    diff(Vars,AllVars,NewVars),
    NewVars \== [],
    variabilize([P],[W],[],[A|Constants]),
    findall(W,W,Ps),
    Ps=[P],
    Q=..[F|NewVars],
    variabilize([P,Q],[VP,VQ],[],Constants),
    VQ=..[F|QVars],
    findall(QVars,(all_members(Ai,M),arg(I,VP,Ai),VP),Rows),
    get_columns(NewVars,1,Rows,Model2),
    append(Model1,Model2,NewModel).

get_columns([],_,_,[]) :- !.
get_columns([A|T],N,Ls,[A/M|W]) :-
    findall(X,(all_members(L,Ls),membern(N,X,L)),AllX),
    single_out(AllX,M), !,
    K is N+1,
    get_columns(T,K,Ls,W).

get_args([],[]) :- !.
get_args([A/_|T],[A|V]) :-
    get_args(T,V).

%-------------------------------------------------------------
%  evaluate(+Clause,+Args,+Model,+Negs,-ProofComplexity)
%  Checks Clause for correctness accoring to its lambda Model
%  and if it is correct computes its ProofComplexity
%-------------------------------------------------------------
evaluate((Head:-Body),HeadArgs,Model,Negs,PC) :-
    Head =.. [R|HeadVars],
    \+(\+((HeadVars=HeadArgs,Body))),
    \+((neg(Head),Body)), !,  % To prevent overgeneralization
    correct(Model,R,HeadArgs,HeadVars,Body,Negs),
    proof_complexity(Body,HeadArgs,HeadVars,PC).

correct([],_,_,_,_,_) :- !.
correct([A/Model|T],R,Args,Vars,Body,Negs) :-
    lambda(Args,A,V,Lambda),
    submodel((Vars=Lambda,Body),V,Model,A,Negs), !,
    correct(T,R,Args,Vars,Body,Negs).

submodel(P,V,Model,A,Negs) :-
    call(P),
    (var(V),
     \+(member(A,Negs)) ;  % To allow singleton head vars
     \+(member(V,Model))
    ), !,fail.
submodel(_,_,_,_,_).

proof_complexity(true,_,_,0) :- !.
proof_complexity((P,Q),A,V,PC) :- !,
   proof_complexity(P,A,V,PC1),
   proof_complexity(Q,A,V,PC2),
   PC is PC1+PC2.
proof_complexity(P,A,V,Len) :-
   findall(A,(A=V,P),M),
   length(M,Len).

%-------------------------------------------------------------
%  head(+Instance,-Model)
%  Instance  - ground atom;
%  Model     - [A1/M1,...,An/Mn],
%              Mi = || lambda X Instance{A/X} ||
%-------------------------------------------------------------
head(Head,Model) :-
   Head =.. [R|Args],
   single_out(Args,Vars),
   propositional(Constants),
   head_model(Vars,R,Args,Constants,Model), !.

head_model([],_,_,_,[]) :- !.
head_model([A|T],R,L,Constants,W) :-  % Skip constants
   member(A,Constants), !,
   head_model(T,R,L,Constants,W).
head_model([A|T],R,L,Constants,W) :-  % Skip shared variables
   efface(A,L,Rest),
   member(A,Rest), !,
   head_model(T,R,L,Constants,W).
head_model([A|T],R,L,Constants,[A/M|Model]) :-
   lambda(L,A,X,Lambda),
   P =.. [R|Lambda],
   findall(X,P,M),
   head_model(T,R,L,Constants,Model).

%-------------------------------------------------------------
%  lambda(+L,+A,-X,-Lambda)
%  L        - list of constants;
%  A        - element of L;
%  X        - the argument X of Lambda;
%  Lambda   - lambda X (L{A/X});
%-------------------------------------------------------------
lambda([],_,_,[]) :- !.
lambda([A|L],A,X,[X|Lambda]) :- !,
   lambda(L,A,X,Lambda).
lambda([B|L],A,X,[B|Lambda]) :-
   lambda(L,A,X,Lambda).

%-------------------------------------------------------------
%  Auxiliary  Predicates
%-------------------------------------------------------------


all_members(X,[X|_]).
all_members(X,[_|T]) :- 
   all_members(X,T).

if_retract(X) :- 
   retract(X), !.
if_retract(_) :- !.

for(X,X,_).
for(B,I,E)  :- 
   B1 is B+1, B1 < E+1, 
   for(B1,I,E).

membern(1,X,[X|_]).
membern(N,X,[_|T]) :- 
   membern(M,X,T),N is M+1.

append_last([X],X,L,[X|L]).
append_last([H|T],X,L,[H|V]) :- 
   append_last(T,X,L,V).

move_back([],X,[X]) :- !.
move_back([X|T],X,V) :- !, 
   move_back(T,X,V).
move_back([H|T],X,[H|V]) :- 
   move_back(T,X,V).

in2(P,[Q|_]) :- 
   functor(P,F,N), functor(Q,F,N),
   P=..[F|L1], Q=..[F|L2], 
   eqset(L1,L2), !.
in2(P,[_|R]) :- 
   in2(P,R).

efface(X,[X|T],T).
efface(X,[Y|T],[Y|Z]) :- 
   efface(X,T,Z).

subset([],_) :- !.
subset([X|T],Y) :- 
   member(X,Y), !, 
   subset(T,Y).

eqset(S1,S2) :- 
   length(S1,L),length(S2,L),
   subset(S1,S2).

diff([],_,[]) :- !.
diff([X|T],L,V) :- 
   member(X,L), !, 
   diff(T,L,V).
diff([X|T],L,[X|V]) :- 
   diff(T,L,V).

intersect([],_,[]) :- !.
intersect([X|T],L,[X|V]) :- 
   member(X,L), !, 
   intersect(T,L,V).
intersect([_|T],L,V) :- 
   intersect(T,L,V).

intersct(A,B) :- 
   all_members(X,A),
   member(X,B), !.

clausify([Head],(Head:-true)) :- !.
clausify([Head|BodyList],(Head:-Body)) :- 
   list2conj(BodyList,Body).

list2conj([X|T],(X,V)) :- 
   list2conj(T,V), !.
list2conj([X],X).

single_out([],[]) :- !.
single_out([X|T],V) :- 
   member(X,T), !, 
   single_out(T,V).
single_out([X|T],[X|V]) :- 
   single_out(T,V).

put_in_lists([],[]) :- !.
put_in_lists([P|T],[Q|V]) :- 
   functor(P,F,N), functor(Q,F,N),
   list_args(N,P,Q), 
   put_in_lists(T,V).

list_args(0,_,_) :- !.
list_args(N,P,Q) :- 
   arg(N,P,A), arg(N,Q,[A]),
   M is N-1, 
   list_args(M,P,Q).



