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

:- use_module(library(clpr)).
log(I, E, R) :-  {I = E^R}.

:- arithmetic_function(log/2).

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
    init(F/N),
    statistics(runtime,_),
    functor(E,F,N),
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
    \+ (H,B,in(H,B)),
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

in(X,(X,_)) :- !.
in(X,(_,Y)) :- !, 
    in(X,Y).
in(X,X) :- !.


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
write1(_).

