%% fuzzy reasoning front-end
%% provides the Prolog API to the resolution back-ends
%%
%% Author: Stasinos Konstantopoulos <stasinos@users.sourceforge.net>
%% Created: 7-4-2007
%% Copyright (C) 2007 Stasinos Konstantopoulos <stasinos@users.sourceforge.net>
%%
%% This program is free software; you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation; either version 2 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License along
%% with this program; if not, write to the Free Software Foundation, Inc.,
%% 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

:- module( yadlr, [check_membership/5, check_types/5, check_members/5,
		   yadlr_concept/2, yadlr_relation/2, yadlr_instance/2,
		   yadlr_assert/3, yadlr_init/1,
		   set_debug_msgs/1, set_depth_limit/1,
		   set_proof_tree_log/1, unset_proof_tree_log/0] ).


%%
%% Load Inference engine
%%

possible_combination( resolution, alg_lukasiewicz ).
possible_combination( settheor,   alg_lukasiewicz ).

:- user:use_inference_engine( Engine ), atom( Engine ),
	user:use_algebra( Alg ), atom( Alg ),
	possible_combination( Engine, Alg ),
	use_module(Engine).


%%
%% DL Services Front-end
%%

check_membership( KB, Instance, Concept, Degree, Restr ) :-
        % atom(KB),
	yadlr_concept_name( KB, Concept ),
	yadlr_instance_name( KB, Instance ),
	T =.. [Concept,Instance],
	prove( KB, (T), Degree, [], Restr ),
	!.

check_types_rec( _, _, [], [], [], [] ).
check_types_rec( KB, Instance, [Degree|DRest], [H|Rest], [H|Concepts], [ExtraRestr|ExtraRestrRest] ) :-
	check_membership( KB, Instance, H, Degree, ExtraRestr ),
	!,
	check_types_rec( KB, Instance, DRest, Rest, Concepts, ExtraRestrRest ).
check_types_rec( KB, Instance, Degrees, [_|Rest], Concepts, ExtraRestr ) :-
	check_types_rec( KB, Instance, Degrees, Rest, Concepts, ExtraRestr ).

check_types( KB, Instance, Degrees, Concepts, Restr ) :-
	findall( C, yadlr_retrieve_concept(KB, C), CList ),
	check_types_rec( KB, Instance, Degrees, CList, Concepts, Restr ).
	
check_members_rec( _, _, [], [], [], [] ).
check_members_rec( KB, Concept, [Degree|DRest], [H|Rest], [H|Instances], [Restr|RestrRest] ) :-
	check_membership( KB, H, Concept, Degree, Restr ),
	!,
	check_members_rec( KB, Concept, DRest, Rest, Instances, RestrRest ).
check_members_rec( KB, Concept, Degrees, [_|Rest], Instances, Restr ) :-
	check_members_rec( KB, Concept, Degrees, Rest, Instances, Restr ).

check_members( KB, Concept, Degree, Instances, Restr ) :-
	findall( I, yadlr_retrieve_instance(KB, I), IList ),
	check_members_rec( KB, Concept, Degree, IList, Instances, Restr ).

	%use_depth( Depth ),
	%depth_bound_call( prove_clauses(KB,Clauses,Open), Depth ).

calleverybody( [] ).
calleverybody( [T|Rest] ) :-
	nonvar( T ),
	call( T ),
	calleverybody( Rest ).
