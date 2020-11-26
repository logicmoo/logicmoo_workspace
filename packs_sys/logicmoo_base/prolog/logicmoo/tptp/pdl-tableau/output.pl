%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Output predicates
% This version of the output predicates enables all output
%

pdl_write(X) :- write(X).
pdl_on_write(X) :- write(X).
pdl_nl :- nl.
pdl_on_nl :- nl.
pdl_write_ln(X) :- write_ln(X).
pdl_writef(X,Y) :- writef(X,Y).
pdl_tab(X) :- tab(X).
