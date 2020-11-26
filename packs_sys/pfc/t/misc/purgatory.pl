add(P, DBin, DBout) :-
  sins(P,Sins),
  promote([(P,Sins)|DBin],DBout).

promote([],[]).

promote([(P,Sins)|Tail], DBout) :-  
  promote(Tail,DbTail),
  sinful(Sins)
    -> DBout=[(P,Sins)|DbTail]
     ; (add(P), DBout=Dbtail).


% a list is sinful iff one of its member is a variable.

sinful([Head|Tail]) :- var(Head);sinful(Tail).
  


%% sins(+Term, -Sins) iff Sins is a list of all of the variables found
%% in Term.

sins(Term,Sins) :- sins(Term,[],Sins).

sins(Term,Sins,[Term|Sins])  :- var(Term), !.

sins(Term,Sins,Sins) :- atomic(Term),!.

sins([Head|Tail], Sin,Sout) :-
  !,
  sins(Head,Sin,Smid),
  sins(Tail,Smid,Sout).

sins(Term,Sin,Sout) :-
  % using functor and arg would be better of course.
  Term =.. [_|Args],
   sins(Args,Sin,Sout).



