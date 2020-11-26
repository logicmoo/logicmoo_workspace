society_goal.


%%%% TEORIA PROLOG %%%%

% da fiore a teschio con lo stesso colore
%%% risposta([X,Y,C,fiore],[X2,Y2,C,teschio], 12).
%%% notrisposta([X,Y,C,fiore],[X2,Y2,C2,tux], 1).

%% risucire a specificare quale condizione verificare con la regola
% regola([2,3,_,_], dx, 1).
% regola([3,1,_,_], dx, 1).
% dx(X, Y, X2, Y2) :- X2 #> X.
% sx(X, Y, X2, Y2) :- X #> X2.


%%:- regola([X,Y,_,_], _),  X > X2.


%% funziona 
% condregola([X,Y,C,F], [X2,Y2,C2,F2]) :- X2 #> X.

%%% inizio diverso da 1,1
inizio(2, 2, _, _).
fine(10, 10, _, _).
% fine(2, 3, _, _).
% maxmove(5).



