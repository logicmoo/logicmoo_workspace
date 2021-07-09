:- module(callutils, [ (*)/4, (*)//4, (*:)//3
                     , const/3 , constf//3
							, pairf//3
                     , mr/5
							, op(600,yfx,*:)
                     , flip/3
                     , true2/2, true1/1
                     , fail2/2, fail1/1
                     , call_with_time_limit//2
                     , timeout/3, timeout//3
                     , timeout_retry//3
                     , bt_call/2
					    	]).

/** <module> High-order utility predicates

Some high-order predicates to enable high-order 'point-free' and
lambda free composition of predicates. Also provides a goal expansion 
for call/N when the target predicate is already known.
*/

:- meta_predicate *(2,2,?,?)
                , *(4,4,?,?,?,?)
                , constf(3,?,?,?,?)
                , pairf(3,3,?,?,?)
                , mr(2,3,?,?,?)
                , flip(2,?,?)
                .

%% flip(+P:pred(A,B), X:B, Y:A) is det.
%  Call binary predicate P with arguments flipped.
flip(P,X,Y) :- call(P,Y,X).

%% *(+P:pred(B,C,S,S), +Q:pred(A,B,S,S), X:A, Z:C, S1:S, S2:S) is det.
%  Pure and stateful predicate composition, order may look weird but
%  it follows the usual convention for function composition. Maybe I should
%  flip it round. Calls Q before P.
*(P,Q,X,Z) --> call(Q,X,Y), call(P,Y,Z).
*(P,Q,X,Z) :- call(Q,X,Y), call(P,Y,Z).

%% *:(+P:pred(A,B,S,S), +G:pred(A,S), X:B, S1:S, S2:S) is det.
% Stateful piping of generator G into function P. Calls G before P!
*:(P,G,Y) --> call(G,X), call(P,X,Y).

%% const(X:A,Y:_,Z:A) is det.
%  Unifies X and Z - const(X) is useful as a binary predicate.
const(X,_,X).

%% pairf(+F:pred(A,S,S), +G:pred(B,S,S), X:pair(A,B), S1:S, S2:S) is det.
%  Call F and G respectively on components of a pair.
pairf(F,G,X-Y) --> call(F,X), call(G,Y).

%% constf(+F:pred(A,S,S), Y:_, X:A, S1:S, S2:S) is det.
%  Call F on X ignoring argument Y.
constf(F,_,X) --> call(F,X).

%% mr(+Mapper:pred(A,B), +Reducer:pred(B,S,S), X:A, S1:S, S2:S) is det.
%  Meet Mr. mr. A map reducer for use with any folding predicate. 
mr(M,R,X,S1,S2) :- call(M,X,Y), call(R,Y,S1,S2).

user:goal_expansion(*(P,Q,X,Z), (call(Q,X,Y), call(P,Y,Z))) :- 
   nonvar(P), nonvar(Q).

user:goal_expansion(G1, G2) :-
   G1 =.. [call, Closure |Args],
   nonvar(Closure), expand_call(Closure, Args, G2).

expand_call(Mod:Head, Args, Mod:G) :- !,
   nonvar(Head), expand_call(Head, Args, G).
expand_call(Head, Args, G) :-
   Head =.. [Pred|Bound],
   append(Bound, Args, AllArgs),
   G =.. [Pred | AllArgs].

true1(_).
true2(_,_).
fail1(_) :- fail.
fail2(_,_) :- fail.

:- meta_predicate call_with_time_limit(+,//,?,?).
call_with_time_limit(T,G,S1,S2) :- 
   call_with_time_limit(T,call_dcg(G,S1,S2)).

:- meta_predicate timeout(+,0,0).
timeout(T,G,R) :- 
   catch(call_with_time_limit(T,G),
         time_limit_exceeded, R).

:- meta_predicate timeout(+,//,//,?,?).
timeout(T,G,R,S1,S2) :- 
   timeout(T, call_dcg(G,S1,S2), call_dcg(R,S1,S2)).

:- meta_predicate timeout_retry(+,//,//,?,?).
timeout_retry(T,G,R) --> 
	timeout(T,G,(R, timeout_retry(T,G,R))).

%! bt_call( :Do, :Undo) is nondet.
%
%  Creates a backtrackable operation from a non-backtrackable Do
%  operation and a corresponding operation to undo it. Do can
%  be non-deterministic, in which case bt_call(Do,Undo) will also
%  have multiple solutions. Undo is called inside once/1.
%  bt_call is a valid debug topic - you can trace all do and undo
%  operations by issuing debug(bt_call).
:- meta_predicate bt_call(0,0).
bt_call(Do,Undo)  :-
   debug(bt_call,'doing: ~p.\n',[Do]),
   Do,
   (  true
   ;  debug(bt_call,'undoing: ~p.\n',[Undo]),
      once(Undo), fail
   ).
