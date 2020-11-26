% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%   Example code from the book "Natural Language Processing in Prolog"  %
%                      published by Addison Wesley                      %
%        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%
% dagunify.pl [Chapter  7] Unification of DAGS
% - originally written by Bob Carpenter
% this operator precedence assumes DEC10 Prolog
?-op(500,xfy, : ).

% unify two side-ways open DAGs, encoded as open-ended lists

unify(Dag,Dag) :- !.
unify([Path:Value|Dags1],Dag) :-
  pathval(Dag,Path,Value,Dags2),
  unify(Dags1,Dags2).

% Find the value of a DAG at a path, returning the remainder of
% the dag (without that feature and value).  Note that you can
% provide a partially specified DAG as Value and the actual value
% will be unified with it (this fact is used in the clause for
% unification above).

pathval(Dag1,Feature:Path,Value,Dags) :-
  !, pathval(Dag1,Feature,Dag2,Dags),
  pathval(Dag2,Path,Value,_).
pathval([Feature:Value1|Dags],Feature,Value2,Dags) :-
  !, unify(Value1,Value2).
pathval([Dag|Dags1],Feature,Value,[Dag|Dags2]) :-
  pathval(Dags1,Feature,Value,Dags2).
