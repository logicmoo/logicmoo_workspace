%   File   : pfcwhy.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Updated:
%   Purpose: predicates for interactively exploring Pfc justifications.

% ***** predicates for brousing justifications *****

:- use_module(library(lists)).

pfcWhy :- 
  whymemory(P,_),
  pfcWhy(P).

pfcWhy(N) :-
  number(N),
  !,
  whymemory(P,Js),
  pfcWhyCommand(N,P,Js).

pfcWhy(P) :-
  justifications(P,Js),
  retractall(whymemory(_,_)),
  assert(whymemory(P,Js)),
  pfcWhyBrouse(P,Js).

pfcWhy1(P) :-
  justifications(P,Js),
  pfcWhyBrouse(P,Js).

pfcWhyBrouse(P,Js) :-
  pfcShowJustifications(P,Js),
  pfcAsk(' >> ',Answer),
  pfcWhyCommand(Answer,P,Js).

pfcWhyCommand(q,_,_) :- !.
pfcWhyCommand(h,_,_) :- 
  !,
  format("~n
Justification Brouser Commands:
 q   quit.
 N   focus on Nth justification.
 N.M brouse step M of the Nth justification
 u   up a level
",[]).

pfcWhyCommand(N,_P,Js) :-
  float(N),
  !,
  pfcSelectJustificationNode(Js,N,Node),
  pfcWhy1(Node).

pfcWhyCommand(u,_,_) :-
  % u=up
  !.

pfcCommand(N,_,_) :-
  integer(N),
  !,
  format("~n~w is a yet unimplemented command.",[N]),
  fail.

pfcCommand(X,_,_) :-
 format("~n~w is an unrecognized command, enter h. for help.",[X]),
 fail.
  
pfcShowJustifications(P,Js) :-
  format("~nJustifications for ~w:",[P]),
  pfcShowJustification1(Js,1).

pfcShowJustification1([],_).

pfcShowJustification1([J|Js],N) :-
  % show one justification and recurse.
  nl,
  pfcShowJustifications2(J,N,1),
  N2 is N+1,
  pfcShowJustification1(Js,N2).

pfcShowJustifications2([],_,_).

pfcShowJustifications2([C|Rest],JustNo,StepNo) :- 
  copy_term(C,CCopy),
  numbervars(CCopy,0,_),
  format("~n    ~w.~w ~w",[JustNo,StepNo,CCopy]),
  StepNext is 1+StepNo,
  pfcShowJustifications2(Rest,JustNo,StepNext).

pfcAsk(Msg,Ans) :-
  format("~n~w",[Msg]),
  read(Ans).

pfcSelectJustificationNode(Js,Index,Step) :-
  JustNo is integer(Index),
  nth(JustNo,Js,Justification),
  StepNo is 1+ integer(Index*10 - JustNo*10),
  nth(StepNo,Justification,Step).
 


