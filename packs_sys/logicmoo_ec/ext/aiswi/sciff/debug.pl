:-module(debug,
     [write_debug/1,writeln_debug/1,write_error/1,write_error2/1]).

:- use_module(sciff_options).
:- use_module(text_style).


writeln_debug(Message) :-
	get_option(sciff_debug, on),
	!,
	write(Message), nl.
writeln_debug(_).

write_debug(Message) :-
	get_option(sciff_debug, on),
	!,
	write(Message).
write_debug(_).
write_error(T):-
    foreground_color(red),
    text_property(bold,1),
    write(T),
    reset_text_style.

write_error2(T):-
    foreground_color(red),
    text_property(underlined,1),
    text_property(bold,1),
    write(T),
    reset_text_style.
