:- expects_dialect(lps).

maxTime(8).

fluents engaña/2.
actions ayuda/2,encuentra/1.
events  necesita/2,escapa/1.

initially engaña(bruja,niño).

observe necesita(bruja,objeto) from 1 to 2.

encuentra(Z) initiates engaña(X,Y) if engaña(Y,X).

if necesita(bruja,objeto) then ayuda(niño,bruja).

if ayuda(niño,bruja) then encuentra(objeto).

escapa(niño) if engaña(niño,bruja).


observe necesita(bruja,objeto) from 4 to 5.

if necesita(bruja,objeto) then ayuda(niño,bruja).

if ayuda(niño,bruja) then encuentra(objeto).

if encuentra(objeto),engaña(niño,bruja) then escapa(niño).
