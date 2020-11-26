:- expects_dialect(lps).

maxTime(6).
fluents esta(X,Y), cerca(X,Y).
actions precavido/2, puede/2, salvar/2, liberarlo/2.
events cortar_cuerdas/1, libero/1, liberar/2.

initially esta(león, amarre).
observe cortar_cuerdas(yo) from 1 to 2.
puede(X,Y) initiates esta(X,Y) if cerca(X,Y).
puede(Z,Y) terminates esta(X,Y) if X \== Z.
libero(león) initiates cerca(yo, amarre) if esta(león, amarre).
libero(león) if precavido(yo, león).
liberarlo(yo, amarre) if esta(león, amarre), libero(león).
liberar(X,Y) if liberarlo(X,Y), puede(X,Y), salvar(X,Y).
if cortar_cuerdas(yo) from T1 to T2 then liberar(yo, amarre) from T2 to T3.
/*
?- go(Timeline).
?- go.
*/