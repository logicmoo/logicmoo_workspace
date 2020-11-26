:- expects_dialect(lps).

% trading.pl
%

% oferta(Bien, Tarifa, Fecha).

oferta(btc, 8400, 1).
oferta(eth, 1000, 1).
demanda(btc, 7800, 1).
oferta(btc, 9000, 2).

% comprador(Nombre, Bien, Fecha).

comprador(google, btc, 2).
comprador(X, B, T) :- oferta(B, V1, T1), oferta(B, V2, T), V1 > V2.

% vendedor(Nombre, Bien, Fecha).

vendedor(Y, B, T) :- demanda(B, V1, T1), demanda(B, V2, T), V2 > V1.  