test_options(generate(_, _),
	     [ setup( (bind(speaker, $'Bruce'), bind(addressee, player)) ) ]).
test(generate(np, speaker_subject_case),
     [ true(Generated == [ 'I' ]) ]) :-
   np(($'Bruce'^S)^S, subject, first:singular, nogap, nogap, Generated, []).
test(generate(np, speaker_object_case),
     [ true(Generated == [ me ]) ]) :-
   np(($'Bruce'^S)^S, object, first:singular, nogap, nogap, Generated, []).
test(generate(np, addressee),
     [ true(Generated == [ you ]) ]) :-
   np((player^S)^S, subject, second:singular, nogap, nogap, Generated, []).
