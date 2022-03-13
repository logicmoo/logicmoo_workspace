test_options(completion(_, _),
	     setup(bind(input_from_player, true))).

test(completion(s, what_is_on),
     [ true( nonempty_instantiated_atom_list(Completion) ),
       nondet ]) :-
   with_bind(generating_nl, true, s_test(_, interrogative, [what, is, on | Completion])).

test(completion(s, imperative),
     [ true( nonempty_instantiated_atom_list(Completion) ),
       true(Mood == imperative),
       nondet ]) :-
   s_test(_, Mood, [go, to | Completion]).

test(generate(s, in_expression)) :-
   s_test(t(location, $'Sophia', $'research_program'), indicative, Generated),
   Generated == ['Sophia', is, in, the, research].

test(generate(s, in_expression0)) :-         
  while_completing( s_test(t(contained_in, $'Sophia', $'research_program'), indicative, Generated)),
   Generated == ['Sophia', is, in, the, research].

test(generate(s, in_expression1)) :-
         s_test(t(contained_in, $'Sophia', $'research_program'), indicative, Generated),
   Generated == ['Sophia', is, in, the, research].

test(generate(s, in_expression2)) :-
   s_test(t(location, $'Sophia', $'research_program'), indicative, Generated),
   Generated == ['Sophia', is, in, the, research].

test(generate(s, future_indicative),
     [ true(Generated == ['I', will, eat, the, plant]),
       nondet ]) :-
   with_bind(speaker, $pc, 
      s(eat($pc, $plant), indicative, affirmative, future, simple, Generated, [ ])).

test(generate(s, future_indicative2),
     [ true(Generated == ['I', will, talk, to, 'Sophia']),
       nondet ]) :-
  with_bind(generating_nl, true,
    with_bind(speaker, $pc,
     s(talk($pc, $'Sophia', _), indicative, affirmative, future, simple, Generated, [ ]))).

test(parse(s, imperative),
     [ true(LF == go($pc, $buggy_routine)),
       true(Mood == imperative),
       nondet]):-
   with_bind(input_from_player, true,
    with_bind(addressee, $pc,
     s_test(LF, Mood, [go, to, the, buggy_routine]))).

:- register_prop($buggy_routine,buggy_routine,[]).
:- assert(location($buggy_routine,$buggy_program)).

test(parse(s, adjectival_property),
     [ true(Generated == ['User', is, male]),
       nondet ]) :-
   s(t(gender, $pc, male),
     indicative, affirmative, present, simple,
     Generated, []).

test(parse(s, wh_transitive),
     [ true(LF = Object:( can(comm(keystrokes,$speaker, Object)),
			  iz_a(Object, entity) )) ]) :-
   s_test(LF, interrogative, [what, can, 'I', type]).

test(generate(s, wh_transitive),
     [ true(Generated == [what, can, 'I', type]),
       nondet ]) :-
   s_test(X:(can(comm(keystrokes,$speaker, X)), iz_a(X, entity)), interrogative, Generated).

s_test(LF, Mood, SurfaceForm) :-
   s(LF, Mood, affirmative, present, simple, SurfaceForm, [ ]).