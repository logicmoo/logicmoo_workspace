:- module(text_style,
	 [reset_text_style/0,
    foreground_color/1,
    background_color/1,
    text_property/2
    ]).

:- use_module(prolog_version).
:- (is_dialect(swi) -> use_module(swi_specific) ; use_module(sicstus_specific)).
:- use_module(sciff_options).



% In Linux, coloring works both in SWI and in SICStus.
% To use coloring, I use the ANSI codes.
% In Windows the console does not support coloring by default
% The following page on Wikipedia
%   http://en.wikipedia.org/wiki/ANSI_escape_code
% says that by loading ANSI.SYS one should be able to make the command.com (not the cmd.exe!)
% able to show colors. I tried and it did not work.
% So, I set coloring on by default on Unix, and off otherwise

set_default_coloring:-
    (is_unix(true)
        ->  set_option(coloring,on)
        ;   set_option(coloring,off)
    ).

:- (get_option(coloring,_) -> true ; set_default_coloring).


reset_text_style:- text_style_if_enabled(0).

foreground_color(Col):-
    color_num(Col,Num),
    Num1 is Num+30,
    text_style_if_enabled(Num1).
background_color(Col):-
    color_num(Col,Num),
    Num1 is Num+40,
    text_style_if_enabled(Num1).

color_num(black,0).
color_num(red,1).
color_num(green,2).
color_num(yellow,3).
color_num(blue,4).
color_num(magenta,5).
color_num(cyan,6).
color_num(white,7).

% On=1 -> activated, On=0 -> deactivated
text_property(Prop,On):-
    text_property_num(Prop,Num),
    N is Num+20*(1-On),
    text_style_if_enabled(N).

text_property_num(bold,1).
text_property_num(light,2).
text_property_num(underlined,4).
text_property_num(reversed,7).
text_property_num(invisible,8).
text_property_num(strikeout,9).

text_style_if_enabled(_):-
    get_option(coloring,off),!.
text_style_if_enabled(N):-
    text_style(N).
