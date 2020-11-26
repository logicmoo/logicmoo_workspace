:- module(m1,[
	trans/1
]).


:- module_transparent(trans/1).
trans(X) :-
%	blub,
	context_module(M),
	call(M:data(X)).
trans(X) :-
	call(data(X)),
	call(mist).

%:- module_transparent(trans/1).
%trans(X) :-
%	trans_intern(X).
% 
%:- module_transparent(trans_intern/1).
%trans_intern(X) :-
%	context_module(M),
%	call(M:data(X)).
%trans_intern(X) :-
%	call(data(X)),
%	call(mist).
%
%blub.

data(m1).
