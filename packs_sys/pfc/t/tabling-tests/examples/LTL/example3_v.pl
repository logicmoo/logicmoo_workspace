%% Get the translated interpreter:

:- [ 'v.pl' ].


%--- An example: some states, some queries...

% NOTE: A "final" state must have an edge to itself.



%                  +------+
%                  |      |
%                  |      |
%               S2: p <---+
%                  ^
%                  |
%                  |
%                  |
%               S0:
%                  |
%                  |
%                  |
%                  v
%               S1: t <---+
%                 |       |
%                 |       |
%                 +-------+

proposition( p ).


state( s0 ).
state( s1 ).
state( s2 ).


trans( s0, s1 ).
trans( s0, s2 ).

trans( s1, s1 ).

trans( s2, s2 ).


holds( s1, p ).


%                                         Expected   v.pl

q :-  check( s0, g p ).             %     no         no
