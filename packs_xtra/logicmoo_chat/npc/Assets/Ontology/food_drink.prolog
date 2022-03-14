

% Mark instances of thought as being edible.
initialize_prop(S, thought) :-
   component_of_metaverse_object_with_type(C, S, $'PropInfo'),
   set_property(C, "IsThought", true).