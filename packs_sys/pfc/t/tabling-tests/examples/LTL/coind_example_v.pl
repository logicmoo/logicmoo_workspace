:- [ 'v.pl' ].

proposition( p ).
proposition( q ).

state( s0 ).

trans( s0, s0 ).

holds( s0, q ).


%                                         Expected   v.pl

q :- check( s0, f p ).     %              no         no
