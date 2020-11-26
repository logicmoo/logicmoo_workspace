% Trindikit update agent for use with godis-basic
% contains module update
% David Hjelm 0507

:-use_module('../../../core/prolog/trindikit').
:-ensure_loaded(app_search_paths).

run:-
	setprop(modules,[ update : update ]),
	setprop(oaa-libdir,'$OAA_HOME/src/oaalib/prolog'),
	setprop(oaa-name, update),
	setprop(oaa,yes),
	oaa_slave.