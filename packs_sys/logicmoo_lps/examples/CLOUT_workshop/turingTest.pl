
:- expects_dialect(lps).

% for XSB only: 
% :- import append/3 from basics.

maxTime(10).

fluents  	said(_,_).
actions 	say(_,_).

initially	said(turing, []), said(robot, []).

observe		say(turing, what)	from 1 to 2.
observe		say(turing, is)		from 2 to 3.
observe		say(turing, your)	from 3 to 4.
observe		say(turing, name)	from 4 to 5.

if		saying(turing, sentence)  	from T1 to T2  
then 	saying(robot, sentence) 	from T3 to T4.

saying(Agent, sentence)		from T1 to T3	if	saying(Agent, nounphrase)	from T1 to T2,
							saying(Agent, verbphrase)	from T2 to T3.

saying(Agent, nounphrase)	from T1 to T3  if	saying(Agent, adjective)	from T1 to T2,
							saying(Agent, noun)	from T2 to T3.

saying(Agent, nounphrase)	from T1 to T2  if	saying(Agent, noun)		from T1 to T2.

saying(Agent, verbphrase)	from T1 to T3  if	saying(Agent, verb)	from T1  to T2,
							saying(Agent, nounphrase) from T2 to T3.
saying(Agent, verbphrase)	from T1 to T2  if	saying(Agent, verb)		from T1 to T2.

saying(Agent, adjective) from T1 to T2	 if	say(Agent, my) from T1 to T2.
saying(Agent, adjective) from T1 to T2	 if	say(Agent, your) from T1 to T2.

saying(Agent, noun) from T1 to T2		if	say(Agent, name) from T1 to T2.
saying(Agent, noun) from T1 to T2		if	say(Agent, what) from T1 to T2.
saying(Agent, noun) from T1 to T2		if	say(Agent, bob) from T1 to T2.
saying(Agent, verb) from T1 to T2		if	say(Agent, is) from T1 to T2.

say(Agent, Word)  initiates	said(Agent, NewPhrase)	if	
							said(Agent, OldPhrase),  
							append(OldPhrase, [Word], NewPhrase).
say(Agent, _Word) terminates  said(Agent, OldPhrase)	if	 
							said(Agent, OldPhrase).

false	say(Agent, Word1),  say(Agent, Word2),    Word1 \= Word2.

/** <examples>
?- go(Timeline).
*/
