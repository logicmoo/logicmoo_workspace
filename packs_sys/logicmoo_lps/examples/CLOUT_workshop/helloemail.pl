
:- expects_dialect(lps).

actions lps_terminate.

if lps_user(User,Email), User \== unknown_user
then 
	lps_send_email(Email, 'LPS program dying',
        'Hello ~w ... Good bye!'-[User]), lps_terminate.
if lps_user(unknown_user,_)
then 
	writeln('But who are you...? Please Login first!'), lps_terminate.

/** <examples>
?- godc(Timeline).
*/