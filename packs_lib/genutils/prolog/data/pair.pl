:- module(pair,
          [ ffst/3, ffst//3
          , fsnd/3, fsnd//3
          , dup/2, pair/3, fst/2, snd/2, is_pair/1
          , select_key_value/4
          , select_key_default_value/5
          , map_select_key_value/5
          , map_select_key_default_value/6
          , (&)/4
          , op(650,xfy,&)
			 ]).

:- meta_predicate fsnd(2,?,?),
						ffst(2,?,?),
                  fsnd(4,?,?,?,?),
						ffst(4,?,?,?,?),
                  &(2,2,?,?),
						map_select_key_value(2,+,-,+,-),
						map_select_key_default_value(2,+,+,-,+,-).

%% is_pair(+X) is semidet.
%  True if X is a pair.
is_pair(_-_).

%% pair(X:A, Y:B, Z:pair(A,B)) is det.
pair(X, Y, X-Y).

%% dup(X:A, Y:pair(A,A)) is det.
dup(X, X-X).

%% fst(X:pair(A,B), Y:A) is det.
fst(X-_, X).

%% snd(X:pair(A,B), Y:B) is det.
snd(_-Y, Y).

%% ffst(+P:pred(A,B), X:pair(A,C), Y:pair(B,C)) is det.
%% ffst(+P:pred(A,B,S,S), X:pair(A,C), Y:pair(B,C), S1:S, S2:S) is det.
%  Apply P to first element of pair. Two versions: one for normal use and
%  another for use in DCG goals.
ffst(P,Y-X,Z-X) :- call(P,Y,Z).
ffst(P,Y-X,Z-X) --> call(P,Y,Z).

%% fsnd(+P:pred(B,C), X:pair(A,B), Y:pair(A,C)) is det.
%% fsnd(+P:pred(B,C,S,S), X:pair(A,B), Y:pair(A,C), S1:S, S2:S) is det.
%  Apply P to second element of pair. Two versions: one for normal use and
%  another for use in DCG goals.
fsnd(P,X-Y,X-Z) :- call(P,Y,Z).
fsnd(P,X-Y,X-Z) --> call(P,Y,Z).

%% &(+F:pred(A,B), +G:pred(A,C), X:A, Y:pair(B,C)) is det.
%  Apply F and G to X and pair results.
&(F,G,X,Y-Z) :- call(F,X,Y), call(G,X,Z).


%% map_select_key_value(+P:pred(A,B), K:C, Y:B, L1:list(pair(C,A)), L2:list(pair(C,A))) is nondet.
%  True when L2 is L1 with an element K-X removed, and P maps X to Y.
map_select_key_value(P, K, Y, L1, L2) :-
	select(K-X, L1, L2), call(P,X,Y).

%% map_select_key_default_value(+P:pred(A,B), K:C, Z:B, Y:B, L1:list(pair(C,A)), L2:list(pair(C,A))) is det.
%  If key K exists in pair list L1, extract value associated with it and apply P to get Y.
%  Otherwise unify default Z with Y.
map_select_key_default_value(P, K, Default, Y, L1, L2) :-
   (  select(K-X, L1, L2) -> call(P,X,Y)
   ;  Y=Default, L1=L2
   ).

%% select_key_value(K:C, Y:B, L1:list(pair(C,A)), L2:list(pair(C,A))) is nondet.
%  True when L2 is L1 with an element K-Y removed.
select_key_value(K, X, L1, L2) :- select(K-X, L1, L2).

%% select_key_default_value(K:C, Z:B, Y:B, L1:list(pair(C,A)), L2:list(pair(C,A))) is det.
%  If key K exists in pair list L1, extract value Y associated with it.
%  Otherwise unify default Z with Y.
select_key_default_value(K, Default, X, L1, L2) :-
   map_select_key_default_value((=), K, Default, X, L1, L2).

user:goal_expansion(fsnd(P,P1,P2), (P1=X-Y1, P2=X-Y2, call(P,Y1,Y2))).
user:goal_expansion(ffst(P,P1,P2), (P1=X1-Y, P2=X2-Y, call(P,X1,X2))).
user:goal_expansion(fst(P,X), P=X-_).
user:goal_expansion(snd(P,Y), P=_-Y).
user:goal_expansion(pair(X,Y,P), P=X-Y).
