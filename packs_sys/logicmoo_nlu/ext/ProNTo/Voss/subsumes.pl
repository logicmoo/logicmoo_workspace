% File SUBSUMES.PL
% Michael A. Covington
% Natural Language Processing for Prolog Programmers
% (Prentice-Hall)
% Chapter 6, Section 6.5.5

% Subsumption checker.
% Based on code by R. A. O'Keefe
% in shared Edinburgh (later Quintus) library.


% subsumes_chk(?T1,?T2)
%  Succeeds if term T1 subsumes T2, i.e.,
%  T1 and T2 can be unified without further
%  instantiating T2.

subsumes_chk(T1,T2) :-
   quietly((\+ ( numvars(T2), \+ (T1 = T2) ))).


% numvars(+Term,-NewTerm)
%  Instantiates each variable in Term to a unique
%  term in the series vvv(0), vvv(1), vvv(2)...

numvars(Term) :-
   numvars_aux(Term,0,_).

numvars_aux(Term,N,N) :- atomic(Term), !.

numvars_aux(Term,N,NewN) :-
   var(Term), !,
   Term = vvv(N),
   NewN is N+1.

numvars_aux(Term,N,NewN) :-
   Term =.. List,
   numvars_list(List,N,NewN).

numvars_list([],N,N).

numvars_list([Term|Terms],N,NewN) :-
   numvars_aux(Term,N,NextN),
   numvars_list(Terms,NextN,NewN).

