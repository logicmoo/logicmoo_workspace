:- module(m11,[
	trans/2
]).

blub(X) :-
	trans(X,m2).

:- module_transparent(trans/2).
trans(X,Z) :-
	% parameter:
	Z:data(X),   % call(Z:data(X))
	call(Z:(
		data(X),
		true,
		call(data, X),
		m1:data(2)
	)),
	% context_module
	context_module(M),
	call(M:data(X)),
	% vorheriges goal bindet modul:
	blub(Z2),
	Z2:data(_),
	% Fehlerfall, den der SWI-Compiler nicht abfängt
	Z3 = Z4, 
	Z3:data(_).
	
trans(X) :-
	data(Y),
	call(data(X)),
	call(mist).

data(m1).


:- dynamic dlg_key_msg/2.
