%%% QUEENS - September 2008

domain(1..25).
range(X) :- domain(X).


%%% Function
%%% queen( -colonna- , -riga.)
%%% per ogni colonna esiste una ed una sola riga

% GASP
%assignment(X,Y).

% lparse
 1{assignment(X,Y) : range(Y)}1 :- domain(X).

%%% Horizontal
:- domain(X1), domain(X2), range(Y1), range(Y2),    %%%  
   assignment(X1,Y1), assignment(X2,Y2), 
   Y1=Y2, neq(X1,X2).
   
%%% Diagonal
:- domain(X1),domain(X2),range(Y1),range(Y2),
   assignment(X1,Y1), assignment(X2,Y2),
   neq(X1,X2),
   abs(X1-X2) == abs(Y1-Y2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

