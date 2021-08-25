% reads a prolog term
% or a comment '% text \n'

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
