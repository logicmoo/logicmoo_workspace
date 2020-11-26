
/*************************************************************************

         name: lamps.pl 
	 date: 2004-11-23
       author: Andreas Wallentin

       Represents all possible variants of lamps in a house and their
       internal address(X10)
       
*************************************************************************/

%:- dynamic used_lamp/1.

is_lamp( Lamp ):-
	lamp( [Lamp],_ ).

%%% X10-address is used to "catch" the correct address when using
%%% the X10-system.
%%% This application is using the Heyu software for controlling X10.
%%% The address must be the same as set in the X10
%%% config file; $HOME/.heyu/x10config

%%% lamp( ?Sort, ?X10-address )
lamp( [skrivbordslampa], 'A4' ).
lamp( [golvlampa], 'A3' ).

lamp( [alla], _A).

/*
det finns bara lamporna ovan inkopplade i systemet

AW 050120

lamp( [taklampa] ).
lamp( [sänglampa] ).
lamp( [bordslampa] ).
lamp( [hörnlampa] ).
lamp( [sofflampa] ).
*/

