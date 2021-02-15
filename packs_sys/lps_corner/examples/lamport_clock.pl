
:- expects_dialect(lps).

% Lamport's clock example in LPS
% by Jacinto Dávila and Bob Kowalski (based on Lamport's Clock Protocol spec.)

maxTime(10).

initially now(ag1,1), now(ag2, 1), now(ag3, 1).

fluents now(_,_).
actions tick(_,_), sending(_,_, _, _).

% received(Ag, Message, From, Stamp): Ag received Message From at Stamp
% send(Ag, Message, To): Ag sends Message To

events receiving(_,_M, _From, _Stamp), to_be_sent(_,_, _To), received(_,_,_,_), send(_,_, _).

observe to_be_sent(ag3, requestresource, ag1) from 1 to 2.
observe to_be_sent(ag3, requestresource, ag2) from 2 to 3. 
observe  received(ag1, requestresource, ag3, 3) from 3 to 4.
%observe  to_be_sent(ag1, ok, ag3) from 4 to 5.
%observe  received(ag3, ok, ag1) from 5 to 6.
%observe  received(ag2, requestresource, ag3, 5) from 6 to 7.
%observe  to_be_sent(ag2, ok, ag3) from 7 to 8.
observe  received(ag1, releaseresource, ag3, 7) from 8 to 9.

if to_be_sent(Ag, M, To) from T1 to T3
then send(Ag, M, To) from T3 to T4.

if received(Ag, M, From, Stamp) from T1 to T2, now(Ag, T3), maxi(Stamp, T3, Tnew)
then receiving(Ag, M, From, Tnew) from T2 to T4.

send(Ag, M, To) if now(Ag, T2), tick(Ag, T2), sending(Ag, M, To, T2).

receiving(Ag, _M, _From, Stamp) from T1 to T2 if
  tick(Ag, Stamp) from T1 to T2.

maxi(X1,X2,X2) :- X2>X1.
maxi(X1,X2,X1) :- X1>=X2.

incr(X, X1) :- X1 is X + 1.

previous(T1, T2) :- T1 =< T2.

tick(Ag, Tp) initiates now(Ag, T) if  incr(Tp, T).
tick(Ag, Tp) terminates now(Ag, T) if  now(Ag, T), previous(T, Tp).

%%%% end of file
