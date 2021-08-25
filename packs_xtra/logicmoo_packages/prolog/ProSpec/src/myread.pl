%% ----------------------------------------
%% File: myread.pl
%% Author: bernd thomas "benno" 
%% E-mail: bthomas@informatik.uni-koblenz.de
%% --------------------------------------------------
%%  reads a prolog term
%%  OR a comment '% text \n' so all comments are kept.
%%
%% $Id: myread.pl,v 1.1 1998/01/16 14:50:50 bthomas Exp $
%% $Log: myread.pl,v $
%% Revision 1.1  1998/01/16 14:50:50  bthomas
%% Initial revision
%%
%% 

myread(S,Term) :-
	go_text_start(S),
	getval(fp,[S,FP]),
	seek(S,FP),
	getval(lastch,[S,C]),
	( C = 37 ->
          read_till_eol(S,Term)
       ;
	  read(S,Term)
      ).

read_till_eol(Stream,Term) :-
	rteol(Stream,SL),
	concat_string(SL,Term).

rteol(Stream,SL) :-
	get_char(Stream,C),
	( C == "\n" ->
	    SL = []
	;
	SL = [C|MC],
	rteol(Stream,MC)
        ).
	
go_text_start(S) :-
	repeat,
	rc(S,C),
	\+ case(C).

rc(S,C) :-
	at(S,FP),
	setval(fp,[S,FP]),
	get(S,C),
	setval(lastch,[S,C]), !.

case(32).
case(10).
