%%
%% Generation
%%

test(generate(vp, intransitive),
     [ true(Generated == [halts]) ]) :-
   vp_test(halt(x), simple, Generated).

test(generate(vp, transitive),
     [ true(Generated == [drinks, the, beer]) ]) :-
   vp_test(drink(x, $beer1), simple, Generated).

test(generate(vp, ditransitive),
     [ true(Generated == [gives, 'Kavi', the, beer]),
       nondet ]) :-
   vp_test(give(x, $'Kavi', $beer1), simple, Generated).

%%
%% Completion
%%

test(completion(vp, from_nothing),
     [ true(nonempty_instantiated_atom_list(Completion)),
       nondet ]) :-
   vp_test(_, simple, Completion).

test(completion(vp, transitive),
     [ true(nonempty_instantiated_atom_list(Completion)),
       nondet ]) :-
   vp_test(_, simple, [drinks | Completion]).

test(completion(vp, ditransitive),
     [ true(nonempty_instantiated_atom_list(Completion)),
       nondet ]) :-
   vp_test(_, simple, [gives | Completion]).

%%
%% Parsing
%%


%%
%% Testing utilties
%%

vp_test(LF, Form, Phrase) :-
   vp(Form, Pred^Pred, _^LF, present, third:singular, nogap, Phrase, []).