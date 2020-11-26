initiatedAt(rich(_131251)=true, _131257, _131236, _131259) :-
     happensAtIE(win_lottery(_131251),_131236),
     _131257=<_131236,
     _131236<_131259.

initiatedAt(location(_131251)=_131239, _131258, _131236, _131260) :-
     happensAtIE(go_to(_131251,_131239),_131236),
     _131258=<_131236,
     _131236<_131260.

terminatedAt(rich(_131251)=true, _131257, _131236, _131259) :-
     happensAtIE(lose_wallet(_131251),_131236),
     _131257=<_131236,
     _131236<_131259.

holdsForSDFluent(happy(_131251)=true,_131236) :-
     holdsForProcessedSimpleFluent(_131251,rich(_131251)=true,_131257),
     holdsForProcessedSimpleFluent(_131251,location(_131251)=pub,_131268),
     union_all([_131257,_131268],_131236).

cachingOrder2(_131235, location(_131235)=home) :-
     person(_131235),place(home).

cachingOrder2(_131235, location(_131235)=pub) :-
     person(_131235),place(pub).

cachingOrder2(_131235, location(_131235)=work) :-
     person(_131235),place(work).

cachingOrder2(_131235, rich(_131235)=true) :-
     person(_131235).

cachingOrder2(_131235, rich(_131235)=false) :-
     person(_131235).

cachingOrder2(_131235, happy(_131235)=true) :-
     person(_131235).

cachingOrder2(_131235, happy(_131235)=false) :-
     person(_131235).

