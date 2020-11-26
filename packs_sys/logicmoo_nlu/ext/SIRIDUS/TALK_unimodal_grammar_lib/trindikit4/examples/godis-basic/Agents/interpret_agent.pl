% Trindikit interpret agent for use with godis-basic
% contains module interpret_simple
% David Hjelm 0507

:-use_module('../../../core/prolog/trindikit').
:-ensure_loaded(app_search_paths).

run:-
	setprop(modules,[ interpret : interpret_simple ]),
	setprop(oaa-libdir,'$OAA_HOME/src/oaalib/prolog'),
	setprop(oaa-name, interpret),
	setprop(oaa,yes),
	oaa_slave.