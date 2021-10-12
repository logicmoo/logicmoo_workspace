test(integrity(property_types_declared),
     [ true(UndeclaredProperties == []) ]) :-
   all(Property,
       ( property_value(_, Property, _),
	 \+ property_type(Property, _, _) ),
       UndeclaredProperties).

test(integrity(valid_property_types),
     [ true(InvalidValues == []) ]) :-
   all(Object.Property=Value,
       ( property_value(Object, Property, Value),
	 \+ valid_property_value(Property, Value) ),
       InvalidValues).