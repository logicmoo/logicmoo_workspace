:- expects_dialect(lps).

% un modelo de cambio

actions liberar, invadir, bombardear, sancionar, manipular. 
fluents resistencia, invadidos.

liberar initiates resistencia.
invadir initiates invadidos if not resistencia.

bombardear terminates resistencia.
sancionar terminates resistencia.
manipular terminates resistencia.
liberar terminates invadidos.

initially resistencia.

observe manipular from 1 to 2.
observe liberar from 1 to 2.
observe sancionar from 3 to 4.
observe liberar from 3 to 4.
observe bombardear from 5 to 6.
% observe liberar from 5 to 6.
observe invadir from 6 to 7.