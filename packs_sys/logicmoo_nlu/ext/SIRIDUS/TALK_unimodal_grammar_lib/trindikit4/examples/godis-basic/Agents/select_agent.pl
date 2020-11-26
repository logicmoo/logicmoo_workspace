% Trindikit select agent for use with godis-basic
% contains module select
% David Hjelm 0507

:-use_module('../../../core/prolog/trindikit').
:-ensure_loaded(app_search_paths).

run:-
	setprop(modules,[ select : select ]),
	setprop(oaa-libdir,'$OAA_HOME/src/oaalib/prolog'),
	setprop(oaa-name, select),
	setprop(oaa,yes),
	oaa_slave.