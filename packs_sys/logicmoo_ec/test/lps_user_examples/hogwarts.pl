:- expects_dialect(lps).

maxTime(10).

fluents     varita_obedece(X), vivo(X).
actions     desarmo/2, cortar_cabeza/2, destruye_varita/1.
events      batalla/1, derroto/2.

initially varita_obedece(dumbledore), vivo(voldemort), vivo(nagini).

observe batalla(hogwarts) from 4 to 5.


derroto(X,Y) if varita_obedece(X), cortar_cabeza(neville,nagini).
derroto(X,Y) terminates vivo(Y).
desarmo(X,Y) initiates  varita_obedece(X).
desarmo(X,Y) terminates varita_obedece(Z).

destruye_varita(X) terminates varita_obedece(X).

cortar_cabeza(X,Y) terminates vivo(Y).


if varita_obedece(dumbledore) at T1
then desarmo(malfoy,dumbledore) from T2 to T3.

if varita_obedece(malfoy) at T1
then desarmo(harry,malfoy) from T2 to T3.

if not vivo(voldemort) at T1
then destruye_varita(harry) from T2 to T3.

if batalla(hogwarts) from T1 to T2 then derroto(harry, voldemort) from T2
to T3.