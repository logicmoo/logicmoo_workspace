%   File   : pfcjust.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Author :  Dave Matuszek, dave@prc.unisys.com
%   Updated:
%   Purpose: predicates for accessing Pfc justifications.
%   Status: more or less working.
%   Bugs:

%% *** predicates for exploring supports of a fact *****


:- use_module(library(lists)).

justification(F,J) :- supports(F,J).

justifications(F,Js) :- bagof(J,justification(F,J),Js).



%% base(P,L) - is true iff L is a list of "base" facts which, taken
%% together, allows us to deduce P.  A base fact is an axiom (a fact 
%% added by the user or a raw Prolog fact (i.e. one w/o any support))
%% or an assumption.

base(F,[F]) :- (axiom(F) ; assumption(F)),!.

base(F,L) :-
  % i.e. (reduce 'append (map 'base (justification f)))
  justification(F,Js),
  bases(Js,L).


%% bases(L1,L2) is true if list L2 represents the union of all of the 
%% facts on which some conclusion in list L1 is based.

bases([],[]).
bases([X|Rest],L) :-
  base(X,Bx),
  bases(Rest,Br),
  pfcUnion(Bx,Br,L).
	
axiom(F) :- 
  pfcGetSupport(F,(user,user)); 
  pfcGetSupport(F,(god,god)).

%% an assumption is a failed goal, i.e. were assuming that our failure to 
%% prove P is a proof of not(P)

assumption(P) :- pfc_negation(P,_).
   
%% assumptions(X,As) if As is a set of assumptions which underly X.

assumptions(X,[X]) :- assumption(X).
assumptions(X,[]) :- axiom(X).
assumptions(X,L) :-
  justification(X,Js),
  assumptions1(Js,L).

assumptions1([],[]).
assumptions1([X|Rest],L) :-
  assumptions(X,Bx),
  assumptions1(Rest,Br),
  pfcUnion(Bx,Br,L).  


%% pfcProofTree(P,T) the proof tree for P is T where a proof tree is
%% of the form
%%
%%     [P , J1, J2, ;;; Jn]         each Ji is an independent P justifier.
%%          ^                         and has the form of
%%          [J11, J12,... J1n]      a list of proof trees.


% pfcChild(P,Q) is true iff P is an immediate justifier for Q.
% mode: pfcChild(+,?)

pfcChild(P,Q) :-
  pfcGetSupport(Q,(P,_)).

pfcChild(P,Q) :-
  pfcGetSupport(Q,(_,Trig)),
  pfcType(Trig,trigger),
  pfcChild(P,Trig).

pfcChildren(P,L) :- bagof(C,pfcChild(P,C),L).

% pfcDescendant(P,Q) is true iff P is a justifier for Q.

pfcDescendant(P,Q) :- 
   pfcDescendant1(P,Q,[]).

pfcDescendant1(P,Q,Seen) :-
  pfcChild(X,Q),
  (\+ member(X,Seen)),
  (P=X ; pfcDescendant1(P,X,[X|Seen])).
  
pfcDescendants(P,L) :- 
  bagof(Q,pfcDescendant1(P,Q,[]),L).

