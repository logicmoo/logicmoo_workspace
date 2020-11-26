% grammar rules

s([hans,trifft,susi],[]):- 
     pn([hans,trifft,susi],[trifft,susi]),
     v_t([trifft,susi],[susi]),
     pn([susi],[]).

s([martha,schlaeft],[]):- 
     pn([martha,schlaeft],[schlaeft]),
     v_i([schlaeft],[]).


vp(A,B) :- v_t(A,C),np(C,B).
vp([sieht,den,mann],[]):-
   v_t([sieht,den,mann],[den,mann]),
   det([den,mann],[mann]),
   n([mann],[]).
vp([hilft,karl],[]):- 
   v_t([hilft,karl],[karl]),
   pn([karl],[]).

min1(D,[s(D)|E]):- min1(D,E).
min1(F,[s(s(F))|G]):- min1(F,G).

end_of_file.

| ?- clear_kb,do_full_kb('examples/ex3.pl').

Try:
| ?- clear_kb, init_kb('examples/ex3.pl').
| ?- intra_construct1(1,2,A,B,C),show_clauses([1,2,A,B,C]).
| ?- g2_op(1,2,A,B,C).   %% stellt Fragen
| ?- show_kb.
| ?- identify(4,3,J), show_clause(J).
| ?- identify(5,I,J), show_clause(J).
| ?- apply_g2([4,5,10],A,BB),show_kb.
