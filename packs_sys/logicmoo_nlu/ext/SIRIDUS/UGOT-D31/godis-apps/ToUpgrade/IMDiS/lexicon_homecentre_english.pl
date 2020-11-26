
 
/*************************************************************************

         name: lexicon_homecenter_english.pl 
      version: Apr 7, 1999
  description: An example domain file, with two languages
       author: Peter Bohlin, Staffan Larsson
 
*************************************************************************/

:- module( lexicon_homecenter_english, [output_form/2, input_form/2,
yn_answer/1] ).

:- use_module( library(lists), [ member/2, select/3, append/3 ] ).

/*----------------------------------------------------------------------
     output_form( +Move, -String )
     -- Canned output
----------------------------------------------------------------------*/

output_form( greet, "Hello and welcome to the TRINDI homecentre assistant").

output_form( inform(task(instruct_exec(reinstall(print_head)))), "Reinstalling the print head.").
output_form( instruct_check(moved_forward(carriage_lock)), "Make sure that the green carriage lock lever is STILL moved all the way forward before you install the print head.").
output_form( instruct_exec(secure(print_head)), "Secure the print head." ).
output_form( instruct_exec(close(top_cover)), "Close the top cover").
output_form( instruct_exec(reattach(scanner)), "Reattach the scanner").
output_form( instruct_exec(press_and_release(yellow_LED_button)), "Press and release the yellow LED button") .
output_form( instruct_exec(line_up(hole_in_print_head, post)),"Line up the hole in the print head with the green post on the printer carriage").
output_form( instruct_exec(lower(print_head)),"Lower the print head down gently into position").
output_form( instruct_exec(push(cartridge_lock_lever)),"Gently push the green cartridge lock lever up until it snaps into place").
output_form( inform(secured(print_head)),"The print head is now secured").

% dialogue case
output_form( ask( move_from_center_position(carriage_head)), "Has the carriage moved from the center position?" ).

% output_form( instruct_exec(remove(print_head) ), "Remove the print head.").
output_form( instruct_exec(remove_and_reinstall(print_head) ), "Remove and reinstall the print head.").

% text case

output_form( inform( if_then( not_move_from_center_position(carriage_head),
			      remove_and_reinstall(print_head) ) ),
		     "Note: If the carriage does not move from the center position after you press the cartridge change button, remove and reinstall the print head" ).


output_form( inform(reinstalled(print_head)), "The print head is now installed" ).
output_form( inform(next(prepare_cartridge_for_printing)), "The printer will prepare the cartridge for printing." ).

output_form( repeat(Move), Str ):-
	output_form( Move, Str ).

output_form( thank, "Thank you very much." ).
output_form( ask(X^(task(X))), "What do you want to do?" ).
output_form( reqRep(understanding), "I didn't understand that. Please rephrase." ).
output_form( quit, "" ).

/*----------------------------------------------------------------------
     input_form( +Phrase, -Move )
     -- Almost canned input
----------------------------------------------------------------------*/

input_form( [hello], greet ).
input_form( [bye], quit ).
input_form( [quit], quit ).
input_form( [what,did,you,say], reqRep ).
input_form( [what], reqRep ).
input_form( [yes], answer_y ).
input_form( [no], answer_n ).
input_form( [ok], confirm ).
input_form( [done], confirm ).
input_form( [and,then], confirm ).
input_form( [reinstall], answer(task(instruct_exec(reinstall(print_head)))) ).
input_form( [how], query_how ).



yn_answer(A):-
	A = 'yes';
	A = 'no'.