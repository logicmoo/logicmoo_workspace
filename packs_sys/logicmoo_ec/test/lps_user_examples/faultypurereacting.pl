:- expects_dialect(lps).

maxTime(10). 

events alguien_me_ataca.
actions responde_igual, consigue_ayuda, trata_de_escapar. 

observe alguien_me_ataca from 1 to 2. 

%            Si alguien me ataca, responde igual.
if alguien_me_ataca from T1 to T2 then responde_igual from T2 to T3. 

%            Si alguien me ataca, consigue ayuda.
if alguien_me_ataca from T1 to T2 then consigue_ayuda from T2 to T3. 

%            Si alguien me ataca, trata de escapar. 
if alguien_me_ataca from T1 to T2 then trata_de_escapar from T2 to T3.

responde_igual from T1 to T2 if consigue_ayuda from T1 to T2. 

trata_de_escapar from T1 to T2 if consigue_ayuda from T1 to T2. 

/** <examples>
?- go(Timeline).
?- go. 
*/
