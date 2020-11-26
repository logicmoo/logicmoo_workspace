:- expects_dialect(lps).

maxTime(10).
actions		attack, rob, call_the(Number),
			repeat_wife_message(Message).
fluents		is_a_weapon, is_a_gift.
events   	persuade, prepare, 
			general_shows(Number),   
			call_general(Number), 
			general_remembers_wife(Message).

initially is_a_weapon. 

false is_a_gift, attack. 

if is_a_weapon then prepare, attack. 

if is_a_weapon then persuade.

prepare from T1 to T2 if 
            true at T1, 
			T2 is T1 + 6. 

persuade if general_shows(Number), 
			general_remembers_wife(Message),
			call_general(Number),
			repeat_wife_message(Message).

call_general(Number) if rob, call_the(Number). 

persuade terminates is_a_weapon.
persuade initiates is_a_gift. 

observe general_shows('+86-555000001') from 2 to 3.
observe general_remembers_wife('##########') from 3 to 4. 
    
/** <examples>
?- go(T).
?- go.
*/
