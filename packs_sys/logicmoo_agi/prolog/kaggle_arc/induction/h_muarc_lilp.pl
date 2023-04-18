
:- use_module(library(clpr)).
log(I, E, R) :-  {I = E^R}.

:- arithmetic_function(log/2).
:- dynamic(background/1).
:- dynamic(propositional/1).

:- use_module(library(logicmoo_utils)).

%-------------------------------------------------------------
%  LGEN - Lambda GENeralization
%  Learning clauses from positive-only examples
%  by searching a lambda-generalization graph
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
%  Top level: lgen(+TargetPredicate/Arity,+SearchMode)
%    TargetPredicate/Arity - specifies the target predicate to
%      be induced. The clauses for the target prdicate are
%      printed and then asserted in the database as facts:
%      lgen_clause(Example,Clause,Cover), where
%        Example - the example from which Clause is induced;
%        Clause  - the induced clause;
%        Cover   - list of examples cover_lgened by Clause.
%    SearchMode is a parameter specifying the search algorithm:
%      df - depth-first search;
%      DepthBound  - integer number (>0) specifying the maximal
%           depth for iterative deepening search.
%-------------------------------------------------------------
%  Data required (must be asserted in the database):
%  1. background([P1/A1,...,Pn/An]).
%     Bi - predicate name, Ai - arity
%  2. Clauses for P1,...,Pn and instances of TargetPredicate.
%-------------------------------------------------------------
%  Requirements to the data:
%  1. All data are specified in DATALOG, i.e. all predicate 
%     agruments must be of type atomic (atoms or numbers).
%  2. Do not use built-in predicate names (e.g. append,
%     member etc.) to avoid name clashes.
%  3. Since there are no argument types, preferably use
%     different domains for different types of arguments.
%-------------------------------------------------------------
%  Requirements to the Prolog system: SICStus Prolog v.3
%  If another version of Prolog is used then it must provide:
%   1. Standard Edinburgh syntax
%   2. Built-in definitions of:
%     * findall(Term,Goal,List) - List contains all possible
%         instantiations of Term, when executing Goal;
%     * \+(Goal) - negation by failure.
%-------------------------------------------------------------
:- dynamic(const/1).
:- dynamic(consts/1).
:- dynamic(numbex/1).
:- dynamic(pred/1).
:- dynamic(lgen_clause/3).
:- dynamic('$tmp'/3).

lgen:- 
  forall(output(F/A),lgen(F/A,df)),
  %background(L),forall(member(F/A,L),lilp(F/A)),
  listing(lgen_clause/3),
  listing(clause_found/3),
  test_induces,
  !.

lgen(F/N,df) :- !,
    lgen1(F/N,df).
lgen(F/N,Depth) :-
    integer(Depth),
    Depth >= 0, !,
    lgen1(F/N,Depth).
lgen(_,Mode) :-
    write('Illegal search mode' = Mode),nl,
    write('Use df or an integer >= 0'),nl.

lgen1(F/N,Mode) :-

    functor(E,F,N),
    retractall(lgen_clause(E,_,_)),
    atom_concat('lrn_',F,LF),
    E=..[F|Args],C=..[LF|Args],
    dynamic(LF/N),multifile(LF/N),
    ignore(retract((C:-lgen_clause(_,(E:-Body),_),Body))),
    assertz((C:-lgen_clause(_,(E:-Body),_),Body)),
    listing(C), listing(E),

    init(F/N),
    statistics(runtime,_),
    uncover_lgened(E),
    write('Searching clause for '),write(E),write('...'),nl,
    lgen(E,Clause,Mode),
    write('Found '),write1(Clause),nl,
    cover_lgen(Clause,Cover),
    assertz(lgen_clause(E,Clause,Cover)),
    fail.
lgen1(F/N,_) :-
    write('Check for clause subsumption...'),nl,
    functor(E,F,N),
    lgen_clause(E,Clause,Cover),
    subsumed(E,Cover),
    write('Removing '),write1(Clause),nl,
    retract(lgen_clause(E,_,_)),
    fail.
lgen1(F/N,_) :-
    statistics(runtime,[_,Ms]),
    Sec is Ms/1000,
    functor(E,F,N),
    nl,write('Clauses found in '),write(Sec),write(s),nl,
    write('-----------------------'),nl,
    lgen_clause(E,Clause,_),
    write1(Clause),nl,
    fail.
lgen1(_,_).
    
init(T) :-
    background(BK),
    length(BK,P1),
    (member(T,BK),P=P1;P is P1+1), !,
    findall(S,(element(F/M,BK),
               functor(R,F,M),
               call(R),
               R =.. [F|Args],
               element(S,Args)),AllS),
    single(AllS,SS),
    length(SS,C),
    retractall(pred(_)),
    assertz(pred(P)),
    retractall(const(_)),
    assertz(const(C)),
    retractall(consts(_)),
    assertz(consts(SS)),
    retractall(lgen_clause(_,_,_)),
    findall(1,(T=F/N,functor(E,F,N),E),Es),
    length(Es,N),
    retractall(numbex(_)),
    assertz(numbex(N)).

compression((H:-B),Compression) :-
    pred(P),
    const(C),
    numbex(Num),
    find_cover_lgen(Num,(H:-B),Cover),
    length(Cover,M),
    count((H:-B),V,A,N),
    Bits_for_Clause is log(2,V+1)+2*N+N*log(2,P)+A*log(2,V+C),
    functor(H,_,AE),
    Bits_for_Cover is 2*M+M*(log(2,P)+AE*log(2,C)),
    Compression is Bits_for_Cover - Bits_for_Clause.

find_cover_lgen(Num,C,Cover) :- % choose the type of cover_lgen used
    Num < 10, !,
    h_cover_lgen(C,Cover).      %  for small sets of examples
find_cover_lgen(_,C,Cover) :-
    cover_lgen(C,Cover).        %  for larger sets of examples

h_cover_lgen((H:-B),Cover) :-
   consts(Cs),
   findall(H,(herbrand([H],Cs),call1(B)),Cover).

cover_lgen((H:-B),Cover) :-
   findall(H,(H,call1(B)),Cover1),
   single(Cover1,Cover).

count((H:-B),V,A,N) :-     % counts literals, vars and args
    (B=true,count1(H,A,N);
     count1((H,B),A,N)),
    (numbervars((H,B),0,V),
     assertz('$tmp'(V)), fail;
     retract('$tmp'(V))), !.

count1((X,Y),A,N) :- !,
    count1(X,A1,N1),
    count1(Y,A2,N2),
    A is A1+A2,
    N is N1+N2.
count1(X,A,1) :-
    functor(X,_,A).

subsumed(E,Cover) :-
    findall(Cover1,(lgen_clause(E1,_,Cover1), \+ E1=E),All),
    flatten(All,Coverage),
    subset(Cover,Coverage), !.


uncover_lgened(E) :-
   call(E),
   \+((clause(lgen_clause(_,_,Cover),true),member(E,Cover))).

lgen(E,C,Depth) :-
    integer(Depth),
    var_args(E,H,AVs),
    lgen_id(0,Depth,E,(H:-true),AVs,C), !.
lgen(E,C,df) :-
    var_args(E,H,AVs),
    lgen_df((H:-true),E,AVs,C,[],_,0,0), !.
lgen(E,(E:-true),_) :-
    write('No generalizations for '),write(E),nl.

% Depth-first Search
%---------------------------------------------
lgen_df(C,E,Avs,C1,S,S1,Compr1,Compr2) :-
    Compr2 > Compr1,
    lgen_df1(C,E,Avs,C1,S,S1,Compr1,Compr2), !.
lgen_df(C,E,Avs,C2,S,S2,_,Compr) :-
    lambda_gen(C,E,Avs,C1,S,S1),
    compression(C1,Compr1),
    lgen_df(C1,E,Avs,C2,S1,S2,Compr,Compr1).

lgen_df1(C,E,Avs,C2,S,S2,_,Compr) :-
    lambda_gen(C,E,Avs,C1,S,S1),
    compression(C1,Compr1),
    Compr1 > Compr,
    lgen_df1(C1,E,Avs,C2,S1,S2,Compr,Compr1).
lgen_df1(C,_,_,C,S,S,_,_).

% Iterative Deepening Search
%--------------------------------------------
lgen_id(D,_,E,C0,Avs,C) :-
    write('Depth '),write(D),write(' ...'),nl,
    lgen_id1(D,C0,E,Avs,C,[],_,0,0), !.
lgen_id(D,Bound,E,C0,Avs,C) :-
    D < Bound,
    D1 is D+1,
    lgen_id(D1,Bound,E,C0,Avs,C).

lgen_id1(0,C,E,Avs,C1,S,S1,Compr1,Compr2) :-
    Compr2 > Compr1, !,
    lgen_id2(C,E,Avs,C1,S,S1,Compr1,Compr2).
lgen_id1(D,C,E,Avs,C2,S,S2,_,Compr) :-
    D>0,D1 is D-1,
    lambda_gen(C,E,Avs,C1,S,S1),
    compression(C1,Compr1),
    lgen_id1(D1,C1,E,Avs,C2,S1,S2,Compr,Compr1).

lgen_id2(C,E,Avs,C2,S,S2,_,Compr) :-
    lambda_gen(C,E,Avs,C1,S,S1),
    compression(C1,Compr1),
    Compr1 > Compr,
    lgen_id2(C1,E,Avs,C2,S1,S2,Compr,Compr1).
lgen_id2(C,_,_,C,S,S,_,_).

lambda_gen(C,E,Avs,C1,S,S1) :-
    findall(C1/S1/N,literal(C,E,Avs,C1,S,S1,N),All),
    minimum(All,_/N), !,
    element(C1/S1/N,All).

literal((H:-true),E,Avs,(H1:-true),S,S1,M) :-
    variabilize(H,H1,S,S1,[],Cs),
    H \== H1,
    test((H1:-true),E,Avs),
    length(Cs,N),
    M is -1/(N+1).
literal((H:-B),E,Avs,C1,S,S1,N) :-
    get_args((H:-B),[],Args),
    \+(Args=[]),
    findall(C1/S1/N,(get_literal(Args,L,A),
                     \+(L=E),
                     wfc(B,H,L,C),
                     variabilize(C,C1,[A/_|S],S1,[],Cs),
                     test(C1,E,Avs),
                     length(Cs,N)), All),
    \+(All=[]), !,
    element(C1/S1/N,All).
literal((H:-B),E,Avs,C1,S,S1,N) :-
    get_args((H:-B),[],Args),
    \+(Args=[]),
    write('Searching multiple literals for a link...'),nl,
    get_literals(1,Args,E,Ls,A),
    wfc(B,H,Ls,C),
    variabilize(C,C1,[A/_|S],S1,[],Cs),
    test(C1,E,Avs), !,
    length(Cs,N).

wfc(B,H,[L1|T],(H:-B1)) :-
    list2conj([L1|T],B,B1), !.
wfc(true,H,L,(H:-L)) :- !.
wfc(B,H,L,(H:-L,B)).

list2conj([L],true,L) :- !.
list2conj([L],B,(L,B)) :- !.
list2conj([L|T],B,(L,B1)) :-
    list2conj(T,B,B1).

get_literals(D,Args,E,Ls,A) :-
    write(D),write(' literals ...'),nl,
    get_literals_d(D,Args,E,Ls,A).
get_literals(D,Args,E,Ls,A) :-
    D < 2,     %  2 literals for a single link limit !!!
    D1 is D+1,
    get_literals(D1,Args,E,Ls,A).

get_literals_d(1,Args,E,[L],A) :-
    get_literal(Args,L,A),
    \+(L=E).
get_literals_d(D,Args,E,[L|Ls],A) :-
    D>0,D1 is D-1,
    get_literals_d(D1,Args,E,Ls,A),
    get_literal(Args,L,A),
    \+ L=E,
    \+ member(L,Ls).

get_literal(Args,L,A):-
    background(BK),
    element(F/N,BK),
    functor(L,F,N),
    L =.. [F|LArgs],
    element(A,Args),
    element(A,LArgs),
    call(L).

variabilize_link(true,H,L,C,S1,S2,Cs) :- !,
    variabilize((H:-L),C,S1,S2,[],Cs).
variabilize_link(B,H,L,C,S1,S2,Cs) :-
    variabilize((H:-L,B),C,S1,S2,[],Cs).

var_args(E,E1,Anons) :-
    head_model(E,M),
    functor(E,F,N),
    functor(E1,F,N),
    var_args(N,E,M,E1),
    variabilize(E,E1,[],_,[],[]),
    anons(N,E1,Anons).

head_model(E,Model) :-
    functor(E,_,Arity),
    findall(N,(for(1,I,Arity),
               lambda_lgen(I,E,L,X),
               findall(X,L,Xs),
               length(Xs,N)),Model).

test((H:-B),E,Skip) :-
    \+(\+((E=H,B))),
    \+ (H,B,in_lgen(H,B)),
    functor(E,_,N),
    forall((for(1,I,N),\+ member(I,Skip)),
           (lambda_lgen(I,E,L,X),forall((L=H,B),(nonvar(X),H)))), !.

anons(0,_,[]) :- !.
anons(I,E,[I|T]) :-
    arg(I,E,A),
    var(A), !,
    I1 is I-1,
    anons(I1,E,T).
anons(I,E,T) :-
    I1 is I-1,
    anons(I1,E,T).

var_args(0,R,_,R) :- !.
var_args(I,P,M,R) :-
    functor(P,F,N),
    functor(Q,F,N),
    copy1(N,I,P,Q),
    head_model(Q,M), !,
    I1 is I-1,
    var_args(I1,Q,M,R).
var_args(I,P,M,R) :-
    I1 is I-1,
    var_args(I1,P,M,R).

copy1(0,_,_,_) :- !.
copy1(I,I,P,Q) :- !,
    K is I-1,
    copy1(K,I,P,Q).
copy1(N,I,P,Q) :-
    arg(N,P,A),
    arg(N,Q,A),
    K is N-1,
    copy1(K,I,P,Q).

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

lambda_lgen(M,P,Q,X) :-
    functor(P,F,N),
    functor(Q,F,N),
    lambda_args(N,M,P,Q,X).

lambda_args(0,_,_,_,_) :- !.
lambda_args(N,N,P,Q,X) :-
    arg(N,P,A),
    nonvar(A), !,
    arg(N,Q,X),
    M is N-1,
    lambda_args(M,N,P,Q,X).
lambda_args(K,N,P,Q,X) :-
    arg(K,P,A),
    arg(K,Q,A),
    M is K-1,
    lambda_args(M,N,P,Q,X).

variabilize(V,V,S,S,R,R) :-
    var(V), !.
variabilize(true,true,S,S,R,R) :- !.
variabilize(A,X,S,S,R,R) :-
    atomic(A),
    member(A/X,S), !.
variabilize(A,A,S,S,R,R) :-
    atomic(A),
    member(A,R), !.
variabilize(A,A,S,S,R,[A|R]) :-
    atomic(A).
variabilize(A,X,S,[A/X|S],R,R) :-
    atomic(A).
variabilize(P,Q,S1,S,R1,R) :-
    \+(atomic(P)),
    P =.. [F|Args],
    variabilize_args(Args,Vars,S1,S,R1,R),
    Q =.. [F|Vars].

variabilize_args([],[],S,S,R,R) :- !.
variabilize_args([A|T],[B|V],S1,S,R1,R) :-
   variabilize(A,B,S1,S2,R1,R2),
   variabilize_args(T,V,S2,S,R2,R).

single([],[]) :- !.
single([X|T],V) :-
    member(X,T), !,
    single(T,V).
single([X|T],[X|V]) :-
    single(T,V).

get_args(V,As,As) :-
    var(V), !.
get_args(true,As,As) :- !.
get_args(A,As,As) :-
    atomic(A),
    member(A,As), !.
get_args(A,As,[A|As]) :-
    atomic(A), !.
get_args(P,As,As1) :-
    P =.. [_|Args],
    get_args_args(Args,As,As1).

get_args_args([],As,As) :- !.
get_args_args([A|T],As,As2) :-
    get_args(A,As,As1),
    get_args_args(T,As1,As2).


call1(G) :- call(G), !.

element(E,[E|_]).
element(E,[_|L]) :- 
    element(E,L).

minimum([X],X) :- !.
minimum([X/M|T],Y/N) :-
    minimum(T,Z/K),
    (M<K,Y/N=X/M ; Y/N=Z/K), !.

in_lgen(X,(X,_)) :- !.
in_lgen(X,(_,Y)) :- !, 
    in_lgen(X,Y).
in_lgen(X,X) :- !.


herbrand([],_):-!.
herbrand([Head|Tail],Cs) :-
    var(Head), !,
    element(Head,Cs),
    herbrand(Tail,Cs).
herbrand([Head|Tail],Cs) :-
    Head =.. [_|Tail1],
    herbrand(Tail1,Cs),
    herbrand(Tail,Cs).

write1(T) :-  % Writing variable names
    numbervars(T,0,_),
    write(T), fail.
write1(_):- write('.').

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
:- dynamic(lilp_clause/3).
:- dynamic(clause_found/3).

:- dynamic(neg/1).
:- multifile(neg/1).

:- dynamic(output/1).
:- multifile(output/1).

:- dynamic(test_induces/2).
:- multifile(test_induces/2).



lilp:- 
  forall(output(F/A),lilp(F/A)),
  %background(L),forall(member(F/A,L),lilp(F/A)),
  listing(lilp_clause/3),
  listing(clause_found/3),
  test_induces,
  !.

do_test_induce(Assert,Test):- 
  findall(Cl,(member(A,Assert),assertz(A,Cl)),ClS),
  forall(member(A,Test),
    in_cmt((write('\t~\n'),dmsg(?-A),
     write('\t~\n'),ignore(forall(call(A)*->dmsg(A);dmsg(failed-A), true))))),
  nop(forall(member(Cl,ClS),erase(Cl))).

test_induces:-
  forall(test_induces(Assert,Test),
   do_test_induce(Assert,Test)).


lilp(F/N) :-
    functor(P,F,N),
    retractall(lilp_clause(P,_,_)),
    atom_concat('lrn_',F,LF),
    P=..[F|Args],C=..[LF|Args],
    dynamic(LF/N),multifile(LF/N),
    ignore(retract((C:-lilp_clause(_,(P:-Body),_),Body))),
    assertz((C:-lilp_clause(_,(P:-Body),_),Body)),
    listing(C), listing(P),
    statistics(runtime,_),    
    (\+(P), write('No examples for'-P),nl,!,fail ; true),
    uncover_lilped(P),
    nl, write('Searching clause for'-P),
    find_clause(P,Clause),
    nl, write1('Found'(P)-Clause),nl,
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

%neg(fail).  % to avoid the unknown predicate exception

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
    subst5(A,B,Subst,Subst1,Save),
    Q =.. [F|B],
    variabilize(R,T,Subst1,Save).

subst5([],[],S,S,_) :- !.
subst5([C|T],[C|V],S,S1,Save) :-
    member(C,Save), !,         % Head prop. constants are preserved
    subst5(T,V,S,S1,Save).
subst5([X|T],[Var|V],S,S1,Save) :-
    member(X/Var,S), !,
    subst5(T,V,S,S1,Save).
subst5([[X]|T],[Var|V],S,S1,Save) :-
    member(X/Var,S), !,
    subst5(T,V,S,S1,Save).
subst5([X|T],[Var|V],S,S1,Save) :-
    member([X]/Var,S), !,
    subst5(T,V,S,S1,Save).
subst5([X|T],[Var|V],S,S1,Save) :-
    all_members(Y/Var,S),
    shared(X,Y),
    subst5(T,V,S,S1,Save).
subst5([X|T],[Var|V],S,S1,Save) :-
    subst5(T,V,[X/Var|S],S1,Save).
subst5([X|T],[C|V],S,S1,Save) :-    % Keep prop. constants lastly
    all_members(C,X),
    member(C,Save),
    subst5(T,V,S,S1,Save).

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



