%%
%% Definitions for containers
%%

% Set the IsContainer field of the PhysicalObject component
initialize_prop(S, container) :-
   component_of_gameobject_with_type(C, S, $'PhysicalObject'),
   set_property(C, "IsContainer", true).

% work_surfaces should have their ContentsVisible fields set to true.
initialize_prop(S, work_surface) :-
   component_of_gameobject_with_type(C, S, $'PhysicalObject'),
   set_property(C, "ContentsVisible", true).
