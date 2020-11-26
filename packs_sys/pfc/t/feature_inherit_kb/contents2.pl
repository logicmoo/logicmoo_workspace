%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% FLUX State Exporter
:- module('microtheoryAboutFn(mMG)',[fluents_for_mt/1]).

:- include(pack('free-life-planner/data-git/systems/planning/state-exporter')).
%% :- flp_include('/var/lib/myfrdcsa/codebases/minor/free-life-planner/data-git/systems/planning/state-exporter').

fluents_for_mt(AllAssertedKnowledge) :-
	pred_for_m('microtheoryAboutFn(mMG)',AllAssertedKnowledge).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

loves(aD,mMG).
