%%% Find a binary Hamming code (N,D) with K elements.

go:-
    statistics(runtime,_),
    top,
    statistics(runtime,[_,Y]),
    write('time : '), write(Y), nl.

top:-    
    hamming_code(8,5,5).

hamming_code(N,D,K) :-
   M is N*K, length(Code,M),
   domain(Code,0,1),
   first_zero(N,Code), %%% [0,...,0] is a word
   constraints(Code,D,N),
   labeling([ff],Code),!,
   write(N,N,Code),nl.
hamming_code(_,_,_) :-
   write('No solution '),nl.

%%% Constraints

constraints(Code,_,N) :-
   length(Code,CL),
   CL =< N, !.
constraints(Code,D,N) :-
   extract(N,Code,First,Last),
   allpairs(Last,First,D,N),
   constraints(Last,D,N).

first_zero(0,_).
first_zero(N,[0|R]):-
  N > 0, M is N -1,
  first_zero(M,R).

allpairs([],_,_,_).
allpairs(Code,Current,D,N) :-
  extract(N,Code,First,Last),
  pair(Current,First,D),
  allpairs(Last,Current,D,N).
pair(Word1,Word2,D) :-
  diffvars(Word1,Word2,Diff),
  domain(Diff,0,1),
  sum(Diff,'#>=',D).

diffvars([],[],[]).
diffvars([A|R],[B|S],[D|T]) :-
%    writeln(D #= abs(A-B)),
  D #= abs(A-B),
  diffvars(R,S,T).

extract(N,Code,First,Last) :-
  length(First,N),
  append(First,Last,Code).

genera_listazeri(0,[]).
genera_listazeri(M,[0|R]) :-
   M > 0, M1 is M - 1,
   genera_listazeri(M1,R).

%%%%%%% PRINT RESULT:
write(_,_,[]) :- !.
write(0,N,Code) :-
   !,nl, write(N,N,Code).
write(M,N,[A|Code]) :-
   M1 is M -1,
   write(A),write(' '),
   write(M1,N,Code).

sum(Diff,#>=,RHS):-
%    writeln(sum(Diff) #>= RHS),
    sum(Diff) #>= RHS.
