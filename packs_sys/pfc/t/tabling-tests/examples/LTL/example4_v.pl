%% Get the translated interpreter:

:- [ 'v.pl' ].


%--- An example that distinguishes between the "one path" and "all pathes"
%--- approach, i.e., exhibits the problem discovered by Brian DeVries.

% NOTE: A "final" state must have an edge to itself.



%                  +------+
%                  |      |
%                  |      |
%               S2: q <---+
%                  ^
%                  |
%                  |
%                  |
%               S0:
%                  |
%                  |
%                  |
%                  v
%               S1: p <---+
%                 |       |
%                 |       |
%                 +-------+

proposition( p ).
proposition( q ).

state( s0 ).
state( s1 ).
state( s2 ).

trans( s0, s1 ).
trans( s0, s2 ).
trans( s1, s1 ).
trans( s2, s2 ).

holds( s1, p ).
holds( s2, q ).


%                                         Expected   v.pl
q :-  check( s0, x p v x q ).         %   yes        yes
