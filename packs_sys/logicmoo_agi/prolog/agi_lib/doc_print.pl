
% Ran with (base) root@mail:/opt/logicmoo_workspace/packs_sys/logicmoo_ec/prolog/ec_planner# swipl -f /opt/logicmoo_workspace/.swiplrc -l ec_config.pl

verbs(_,contains).
verbs(_,enables).
verbs(_,prevents).
verbs(_,sequencesTo).
%verbs(_,sequencesTo(byChoice)).
%verbs(_,'sequencesTo(Automatically)').
%verbs(_,sequenceTo('auto/Choice')).
%verbs(X,implies).

nouns(1,t(some,story)).
%nouns(1,t(some,phrase)).
nouns(1,t(some,situation)).
nouns(1,t(some,percept)).

nouns(1,t(some,goal)).
nouns(1,t(some,action)).
nouns(1,t(some,protoExemplar)).
%nouns(X,class).

toPC(t(R,C),M):- toPC(C,PC),M=..[R,PC],!.
toPC(S,M):- is_list(S),!,must_maplist(toPC,S,M).
toPC(S,M):- compound(S),!,
   compound_name_arguments(S,F,Args),
   toPC([F|Args],[U|ArgsO]),
   compound_name_arguments(M,U,ArgsO),!.
toPC(X,Y):- toUpperCamelcase(X,Y).

printall(X):-  
  verbs(X,V0),nouns(X,S0),nl, nouns(X,O0),
  toPC([S0,V0,O0],[S,V,O]),
%  S\==O,
  format('~N~w - ~|~w - ~|~w~n',[S,V, O]),fail.
printall(_).

printall:- between(1,4,X), \+ \+ nouns(X,_), printall(X),fail.
printall.

nm:- mmake,cls, printall.
:- nm.

end_of_file.

some(Story) - Contains - some(Story)
some(Story) - Contains - some(Situation)
some(Story) - Contains - some(Percept)
some(Story) - Contains - some(Goal)
some(Story) - Contains - some(Action)
some(Story) - Contains - some(ProtoExemplar)

some(Situation) - Contains - some(Story)
some(Situation) - Contains - some(Situation)
some(Situation) - Contains - some(Percept)
some(Situation) - Contains - some(Goal)
some(Situation) - Contains - some(Action)
some(Situation) - Contains - some(ProtoExemplar)

some(Percept) - Contains - some(Story)
some(Percept) - Contains - some(Situation)
some(Percept) - Contains - some(Percept)
some(Percept) - Contains - some(Goal)
some(Percept) - Contains - some(Action)
some(Percept) - Contains - some(ProtoExemplar)

some(Goal) - Contains - some(Story)
some(Goal) - Contains - some(Situation)
some(Goal) - Contains - some(Percept)
some(Goal) - Contains - some(Goal)
some(Goal) - Contains - some(Action)
some(Goal) - Contains - some(ProtoExemplar)

some(Action) - Contains - some(Story)
some(Action) - Contains - some(Situation)
some(Action) - Contains - some(Percept)
some(Action) - Contains - some(Goal)
some(Action) - Contains - some(Action)
some(Action) - Contains - some(ProtoExemplar)

some(ProtoExemplar) - Contains - some(Story)
some(ProtoExemplar) - Contains - some(Situation)
some(ProtoExemplar) - Contains - some(Percept)
some(ProtoExemplar) - Contains - some(Goal)
some(ProtoExemplar) - Contains - some(Action)
some(ProtoExemplar) - Contains - some(ProtoExemplar)

some(Story) - Enables - some(Story)
some(Story) - Enables - some(Situation)
some(Story) - Enables - some(Percept)
some(Story) - Enables - some(Goal)
some(Story) - Enables - some(Action)
some(Story) - Enables - some(ProtoExemplar)

some(Situation) - Enables - some(Story)
some(Situation) - Enables - some(Situation)
some(Situation) - Enables - some(Percept)
some(Situation) - Enables - some(Goal)
some(Situation) - Enables - some(Action)
some(Situation) - Enables - some(ProtoExemplar)

some(Percept) - Enables - some(Story)
some(Percept) - Enables - some(Situation)
some(Percept) - Enables - some(Percept)
some(Percept) - Enables - some(Goal)
some(Percept) - Enables - some(Action)
some(Percept) - Enables - some(ProtoExemplar)

some(Goal) - Enables - some(Story)
some(Goal) - Enables - some(Situation)
some(Goal) - Enables - some(Percept)
some(Goal) - Enables - some(Goal)
some(Goal) - Enables - some(Action)
some(Goal) - Enables - some(ProtoExemplar)

some(Action) - Enables - some(Story)
some(Action) - Enables - some(Situation)
some(Action) - Enables - some(Percept)
some(Action) - Enables - some(Goal)
some(Action) - Enables - some(Action)
some(Action) - Enables - some(ProtoExemplar)

some(ProtoExemplar) - Enables - some(Story)
some(ProtoExemplar) - Enables - some(Situation)
some(ProtoExemplar) - Enables - some(Percept)
some(ProtoExemplar) - Enables - some(Goal)
some(ProtoExemplar) - Enables - some(Action)
some(ProtoExemplar) - Enables - some(ProtoExemplar)

some(Story) - Prevents - some(Story)
some(Story) - Prevents - some(Situation)
some(Story) - Prevents - some(Percept)
some(Story) - Prevents - some(Goal)
some(Story) - Prevents - some(Action)
some(Story) - Prevents - some(ProtoExemplar)

some(Situation) - Prevents - some(Story)
some(Situation) - Prevents - some(Situation)
some(Situation) - Prevents - some(Percept)
some(Situation) - Prevents - some(Goal)
some(Situation) - Prevents - some(Action)
some(Situation) - Prevents - some(ProtoExemplar)

some(Percept) - Prevents - some(Story)
some(Percept) - Prevents - some(Situation)
some(Percept) - Prevents - some(Percept)
some(Percept) - Prevents - some(Goal)
some(Percept) - Prevents - some(Action)
some(Percept) - Prevents - some(ProtoExemplar)

some(Goal) - Prevents - some(Story)
some(Goal) - Prevents - some(Situation)
some(Goal) - Prevents - some(Percept)
some(Goal) - Prevents - some(Goal)
some(Goal) - Prevents - some(Action)
some(Goal) - Prevents - some(ProtoExemplar)

some(Action) - Prevents - some(Story)
some(Action) - Prevents - some(Situation)
some(Action) - Prevents - some(Percept)
some(Action) - Prevents - some(Goal)
some(Action) - Prevents - some(Action)
some(Action) - Prevents - some(ProtoExemplar)

some(ProtoExemplar) - Prevents - some(Story)
some(ProtoExemplar) - Prevents - some(Situation)
some(ProtoExemplar) - Prevents - some(Percept)
some(ProtoExemplar) - Prevents - some(Goal)
some(ProtoExemplar) - Prevents - some(Action)
some(ProtoExemplar) - Prevents - some(ProtoExemplar)

some(Story) - SequencesTo - some(Story)
some(Story) - SequencesTo - some(Situation)
some(Story) - SequencesTo - some(Percept)
some(Story) - SequencesTo - some(Goal)
some(Story) - SequencesTo - some(Action)
some(Story) - SequencesTo - some(ProtoExemplar)

some(Situation) - SequencesTo - some(Story)
some(Situation) - SequencesTo - some(Situation)
some(Situation) - SequencesTo - some(Percept)
some(Situation) - SequencesTo - some(Goal)
some(Situation) - SequencesTo - some(Action)
some(Situation) - SequencesTo - some(ProtoExemplar)

some(Percept) - SequencesTo - some(Story)
some(Percept) - SequencesTo - some(Situation)
some(Percept) - SequencesTo - some(Percept)
some(Percept) - SequencesTo - some(Goal)
some(Percept) - SequencesTo - some(Action)
some(Percept) - SequencesTo - some(ProtoExemplar)

some(Goal) - SequencesTo - some(Story)
some(Goal) - SequencesTo - some(Situation)
some(Goal) - SequencesTo - some(Percept)
some(Goal) - SequencesTo - some(Goal)
some(Goal) - SequencesTo - some(Action)
some(Goal) - SequencesTo - some(ProtoExemplar)

some(Action) - SequencesTo - some(Story)
some(Action) - SequencesTo - some(Situation)
some(Action) - SequencesTo - some(Percept)
some(Action) - SequencesTo - some(Goal)
some(Action) - SequencesTo - some(Action)
some(Action) - SequencesTo - some(ProtoExemplar)

some(ProtoExemplar) - SequencesTo - some(Story)
some(ProtoExemplar) - SequencesTo - some(Situation)
some(ProtoExemplar) - SequencesTo - some(Percept)
some(ProtoExemplar) - SequencesTo - some(Goal)
some(ProtoExemplar) - SequencesTo - some(Action)
some(ProtoExemplar) - SequencesTo - some(ProtoExemplar)

