
:- expects_dialect(lps).

maxTime(10).

actions tell(_).

if maxTime(Max), Last is Max-1
then 
	lps_send_email('mc@interprolog.com', 'LPS program dying',
        'You only gave me ~w cycles... Good bye!'-[Max]) from Last.

/** <examples>
?- godc(Timeline).
*/