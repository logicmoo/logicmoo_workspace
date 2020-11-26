%   File   : pfcwhy.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Updated:
%   Purpose: predicates for interactively exploring Pfc justifications.

end_of_file.

:- module(pfcwhy, []).
:- use_module(library(pfc_pack_xform)).

% ***** predicates for brousing justifications *****

:- use_module(library(lists)).

:- thread_local(t_l:whybuffer/2).

pfcWhy :- 
 umt((
  t_l:whybuffer(P,_),
  mpred_why0(P))).
% see pfc_why
mpred_why(X):- source_file(_,_), % non-interactive
  color_line(green,2),
  forall(no_repeats(P-Js,justifications(P,Js)),
    (color_line(yellow,1),mpred_showJustifications(X,Js))),
  color_line(green,2),!.
  

mpred_why(X):-
  umt((mpred_why0(X))).

mpred_why0(N) :-
  number(N),
  !,
  t_l:whybuffer(P,Js),
  mpred_whyCommand0(N,P,Js).

mpred_why0(P) :-
  justifications(P,Js),
  retractall(t_l:whybuffer(_,_)),
  assert(t_l:whybuffer(P,Js)),
  mpred_whyBrouse(P,Js).

mpred_why1(P) :-
  justifications(P,Js),
  mpred_whyBrouse(P,Js).

mpred_whyBrouse(P,Js) :-
  mpred_showJustifications(P,Js),
  ttyflush,
  read_pending_chars(current_input,_,[]),!,
  ttyflush,
  % pfcAsk(' >> ',Answer),
  % read_pending_chars(current_input,[Answer|_],[]),!,  
  format('~N',[]),write('proof [q/h/u/?.?]: '),get_char(Answer),
  mpred_whyCommand0(Answer,P,Js).

mpred_whyCommand0(q,_,_) :- !.
mpred_whyCommand0(h,_,_) :- 
  !,
  format("~n
Justification Brouser Commands:
 q   quit.
 N   focus on Nth justification.
 N.M brouse step M of the Nth justification
 u   up a level
",[]).

mpred_whyCommand0(N,_P,Js) :-
  float(N),
  !,
  mpred_selectJustificationNode(Js,N,Node),
  mpred_why1(Node).

mpred_whyCommand0(u,_,_) :-
  % u=up
  !.

mpred_whyCommand0(N,_,_) :-
  integer(N),
  !,
  format("~n~w is a yet unimplemented command.",[N]),
  fail.

mpred_whyCommand0(X,_,_) :-
 format("~n~w is an unrecognized command, enter h. for help.",[X]),
 fail.
  
mpred_showJustifications(P,Js) :-
  format("~nJustifications for ~w:",[P]),
  mpred_showJustification1(Js,1).

mpred_showJustification1([],_).

mpred_showJustification1([J|Js],N) :-
  % show one justification and recurse.
  nl,
  mpred_showJustifications2(J,N,1),
  N2 is N+1,
  mpred_showJustification1(Js,N2).

mpred_showJustifications2([],_,_).

mpred_showJustifications2([C|Rest],JustNo,StepNo) :- 
  copy_term(C,CCopy),
  numbervars(CCopy,0,_),
  format("~n    ~w.~w ~w",[JustNo,StepNo,CCopy]),
  StepNext is 1+StepNo,
  mpred_showJustifications2(Rest,JustNo,StepNext).

pfcAsk(Msg,Ans) :-
  format("~n~w",[Msg]),
  read(Ans).

mpred_selectJustificationNode(Js,Index,Step) :-
  JustNo is integer(Index),
  nth1(JustNo,Js,Justification),
  StepNo is 1+ integer(Index*10 - JustNo*10),
  nth1(StepNo,Justification,Step).
 



