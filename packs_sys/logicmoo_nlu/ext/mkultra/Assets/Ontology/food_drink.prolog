

% Mark instances of food as being edible.
initialize_prop(S, food) :-
   component_of_gameobject_with_type(C, S, $'PropInfo'),
   set_property(C, "IsFood", true).

% Mark instances of beverage as being drinkable.
initialize_prop(S, beverage) :-
   component_of_gameobject_with_type(C, S, $'PropInfo'),
   set_property(C, "IsBeverage", true).