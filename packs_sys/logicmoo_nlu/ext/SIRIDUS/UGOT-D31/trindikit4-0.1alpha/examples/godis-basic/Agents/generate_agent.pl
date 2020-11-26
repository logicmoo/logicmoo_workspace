% Trindikit generate agent for use with godis-basic
% contains module generate_simple
% David Hjelm 0507

:-use_module('../../../core/prolog/trindikit').
:-ensure_loaded(app_search_paths).

run:-
	setprop(modules,[ generate : generate_simple ]),
	setprop(oaa-libdir,'$OAA_HOME/src/oaalib/prolog'),
	setprop(oaa-name, generate),
	setprop(oaa,yes),
	oaa_slave.