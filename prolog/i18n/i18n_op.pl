:- module(i18n_op,
	  [op(600, xfy, :~),
	   op(600, xfy, :~~),
	   op(1150, fx, resourceterm),
	   op(700, xfx, =~),	% translation
	   op(700, xfx, ~=),	% dictionary
	   op(700, xfx, =~~),	% dictionary + translation
	   op(200, fy, ~),
	   op(200, fy, ~~)]).
