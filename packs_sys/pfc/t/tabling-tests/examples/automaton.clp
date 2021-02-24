%% An example due to Horst Reichel of TU Dresden.

% For example,
%   ?-  L = [ a, b, e, a , b, c, d | L ], automaton( L, s0 ).
% should succeed.

trans( s0, a, s1 ).
trans( s1, b, s2 ).
trans( s2, e, s0 ).
trans( s2, c, s3 ).
trans( s3, d, s0 ).

:- coinductive0 automaton/2.

automaton( [ X | T ],  St ) :-
           trans( St, X, NewSt ),
           automaton( T, NewSt ).

:- topl go/2.

go( A, B ) :-  automaton( A, B ).


