%% Get the translated interpreter:

:- [ 'verifier.pl' ].


%--- An example: some states, some queries...

% NOTE: A "final" state must have an edge to itself.


%        S3: p, z ----> S4: p, s
%             ^         /
%              \       /
%               \     /
%                \   /
%                 \ v
%               S2: p, q
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
proposition( q ).
proposition( s ).
proposition( t ).
proposition( z ).


state( s0 ).
state( s1 ).
state( s2 ).
state( s3 ).
state( s4 ).


trans( s0, s1 ).
trans( s0, s2 ).

trans( s1, s1 ).

trans( s2, s3 ).

trans( s3, s4 ).

trans( s4, s2 ).


holds( s1, t ).

holds( s2, p ).
holds( s2, q ).

holds( s3, p ).
holds( s3, z ).

holds( s4, p ).
holds( s4, s ).


%                                         Expected   Prolog    Tabling

q1  :- check( s0, g p ).                  % no       no        no

q2  :- check( s0, f p ).                  % no       no        no

q3  :- check( s0, f p v f t ).            % yes      yes       yes

q4  :- check( s0, f (p v t) ).            % yes      yes       yes

q5  :- check( s1, g p ).                  % no       no        no

q6  :- check( s1, f p ).                  % no       no        no

q7  :- check( s0, f g p ).                % no       no        no

q8  :- check( s2, g p ).                  % yes      loops     yes

q9  :- check( s2, f g p ).                % yes      loops     yes

q10 :- check( s2, f( q ^ z ) ).           % no       no        no

q11 :- check( s2, f q ^ f z ).            % yes      yes       yes

q12 :- check( s2, g f s ).                % yes      loops     yes

