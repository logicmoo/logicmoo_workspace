:- expects_dialect(lps).

maxTime(8).

fluents engaña(X,Y).
actions ayuda/2,encuentra/1,obtiene/1.
events  necesita/2,escapa/1.

initially engaña(bruja,niño).

observe necesita(bruja,objeto) from 1 to 2.

ayuda(X,Y) initiates engaña(X,Y) if obtiene(objeto).

escapa(niño) if ayuda(niño,bruja).

if necesita(bruja,objeto) from T1 to T2 then escapa(niño) from T2 to T3.

?- go(Timeline).
?- go.








