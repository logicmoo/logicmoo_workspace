%% Get the translated interpreter:

:- [ 'verifier.pl' ].


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
%               S1: t <---+               S3: p <---+
%                 |       |                 |       |
%                 |       |                 |       |
%                 +-------+                 +-------+

proposition( p ).
proposition( f ).
proposition( ho( ho ) ).


% state( s0 ).
state( s1 ).
state( s2 ).
state( s3 ).


trans( s0, s1 ).
trans( s0, s2 ).

trans( s1, s1 ).

trans( s2, s2 ).

trans( s3, s3 ).


holds( s1, p ).


q :-  check( s0, g p ).  % This is supposed to produce errors (inconsistency)
