test_options(generate(np, _),
	     [ setup( (bind(speaker, $pc), bind(addressee, $user)) ) ]).

%%
%% Generation
%%

test(generate(np, speaker_subject_case),
     [ true(Generated == [ 'I' ]) ]) :-
   np_test($pc, subject, first:singular, Generated).

test(generate(np, speaker_object_case),
     [ true(Generated == [ me ]) ]) :-
   np_test($pc, object, first:singular, Generated).

test(generate(np, character_third_person),
     [ true(Generated == [ 'Sophia' ]) ]) :-
   np_test($'Sophia', object, third:singular, Generated).

test(generate(np, addressee),
     [ true(Generated == [ you ]) ]) :-
   np_test($user, subject, second:singular, Generated).

test(generate(np, kind),
     [ true(Generated == [a, module]) ]) :-
   bind_discourse_variables(iz_a(X, module), 
     {np_test(X, subject, third:singular, Generated)},[],[]).

test(generate(np, prop),
     [ true(Generated == [the, buggy, routine]) ]) :-
   np_test($buggy_routine, subject, third:singular, Generated).

%%
%% Completion
%%

test(completion(np, from_nothing),
     [ true(nonempty_instantiated_atom_list(Completion)),
       nondet ]) :-
   np(_, subject, third:singular, nogap, nogap, Completion, [ ]).

test(completion(np, kind),
     [ true(ground(Completion)),
       true(Completion = [_|_]),
       nondet ]) :-
   np(_, subject, third:singular, nogap, nogap, [a | Completion ], [ ]).

test(completion(np, reject_verb)) :-
   \+ np(_, subject, third:singular, nogap, nogap, [go | _Completion ], [ ]).

test(completion(np, prop),
     [ true(atomic(Noun)),
       nondet ]) :-
   np_test(_, subject, third:singular, [the, Noun]).

%%
%% Parsing
%%

test(parse(np, speaker_subject_case),
     [ true(LF == $speaker) ]) :-
   np_test(LF, subject, first:singular, [ 'I' ]).

test(parse(np, speaker_object_case),
     [ true(LF == $speaker) ]) :-
   np_test(LF, object, first:singular, [ me ]).

test(parse(np, character_third_person),
     [ true(LF == $'Sophia') ]) :-
   np_test(LF, object, third:singular, ['Sophia']).

test(parse(np, addressee),
     [ true(LF == $addressee) ]) :-
   np_test(LF, subject, second:singular, [ you ]).

test(parse(np, kind),
     [ true(LF = ((X^S)^(S, iz_a(X, Module)))) ]) :-
   np(LF, subject, third:singular, nogap, nogap, [a, Module], []).

test(parse(np, prop),
     [ true(LF == $buggy_routine),
       nondet ]) :-
   with_bind(input_from_player, true,
    np_test(LF, subject, third:singular, [the, buggy, routine])).


np_test(LF, Case, Agreement, SurfaceForm) :-
   np((LF^S)^S, Case, Agreement, nogap, nogap, SurfaceForm, [ ]).