%%
%% Generation
%%

test(generate(vp, intransitive),
     [ true(Generated == [halts]),
       nondet ]) :-
   vp_test(halt(x), simple, Generated).

test(generate(vp, transitive),
     [ true(Generated == [performs, the, lemonaid]) ]) :-
   vp_test(perform(x, $lemonaid), simple, Generated).

test(generate(vp, ditransitive),
     [ true(Generated == [gives, 'Sophia', the, lemonaid]),
       nondet ]) :-
   vp_test(give(x, $'Sophia', $lemonaid), simple, Generated).

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
   vp_test(_, simple, [performs | Completion]).

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