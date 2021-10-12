test_options(completion(_, _),
	     setup(bind(input_from_player, true))).

test(completion(s, what_is_on),
     [ true( nonempty_instantiated_atom_list(Completion) ),
       nondet ]) :-
   s_test(_, interrogative, [what, is, on | Completion]).

test(completion(s, imperative),
     [ true( nonempty_instantiated_atom_list(Completion) ),
       true(Mood == imperative),
       nondet ]) :-
   s_test(_, Mood, [go, to | Completion]).

test(generate(s, in_expression)) :-
   s_test(location($'Kavi', $'kitchen'), indicative, Generated),
   Generated == ['Kavi', is, in, the, kitchen ].

test(generate(s, future_indicative),
     [ setup(bind(speaker, $pc)),
       true(Generated == ['I', will, eat, the, plant]),
       nondet ]) :-
   s(eat($pc, $plant), indicative, affirmative, future, simple, Generated, [ ]).

test(generate(s, future_indicative2),
     [ setup(bind(speaker, $pc)),
       true(Generated == ['I', will, talk, to, 'Kavi']),
       nondet ]) :-
   bind(generating_nl, true),
   s(talk($pc, $'Kavi', _), indicative, affirmative, future, simple, Generated, [ ]).

test(parse(s, imperative),
     [ setup(bind(addressee, $pc)),
       true(LF == go($pc, $bed)),
       true(Mood == imperative),
       nondet]):-
   bind(input_from_player, true),
   s_test(LF, Mood, [go, to, the, bed]).

test(parse(s, adjectival_property),
     [ true(Generated == ['Betsy', is, female]),
       nondet ]) :-
   s(property_value($pc, gender, female),
     indicative, affirmative, present, simple,
     Generated, []).

test(parse(s, wh_transitive),
     [ true(LF = Object:( can(type(unknown_speaker, Object)),
			  is_a(Object, entity) )) ]) :-
   s_test(LF, interrogative, [what, can, 'I', type]).

test(generate(s, wh_transitive),
     [ true(Generated == [what, can, 'I', type]),
       nondet ]) :-
   s_test(X:(can(type(unknown_speaker, X)), is_a(X, entity)), interrogative, Generated).

s_test(LF, Mood, SurfaceForm) :-
   s(LF, Mood, affirmative, present, simple, SurfaceForm, [ ]).