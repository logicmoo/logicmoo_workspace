

% Mark instances of task as being runnable.
initialize_prop(S, task) :-
   component_of_metaverse_object_with_type(C, S, $'PropInfo'),
   set_property(C, "IsRunnable", true).

%=autodoc
%% initialize_prop( ?S, ?Container) is semidet.
%
% Initialize Prop.
%


% Mark instances of animation as being playable.
initialize_prop(S, animation) :-
   component_of_metaverse_object_with_type(C, S, $'PropInfo'),
   set_property(C, "IsAnimation", true).