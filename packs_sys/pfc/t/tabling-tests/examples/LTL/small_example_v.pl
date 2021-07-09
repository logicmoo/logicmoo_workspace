:- [ 'v.pl' ].

proposition( p ).

state( s0 ).
state( s1 ).

trans( s0, s0 ).
trans( s0, s1 ).
trans( s1, s0 ).

holds( s0, p ).
holds( s1, p ).

%                                          Expected   v.pl
q  :- check( s0, g p ).                 %  yes        yes


