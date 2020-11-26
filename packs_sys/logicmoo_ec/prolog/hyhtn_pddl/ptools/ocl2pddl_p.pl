% -----------ocl2pddl_p -----------
% OCL Pddl translator, for problem
% 15 May, 2000
% Donghong Liu
% University of Huddersfield

:- op(100,xfy,'=>').

convert_p:-
     tell('pddl_problem'),
     planner_task(Num,Goal,Ini),
     write('\(define: \(problem '),
     write(Num),
     write('\)'),nl,
     write('  \(:domain  '),
     domain(Name),
     write(Name),
     write('\)'),nl,
     write('  \(:objects '),nl,
     write_obj,
     write('\)'),nl,
     write_init(Ini),
     write_goal(Goal),
     write('\)'),nl,
     fail.
convert_p:-
     tell('pddl_problem'),
     told.

write_obj:-
     objects(Sort,Objs),
     write_obj1(Objs),
     write('- '),
     write(Sort),nl,
     fail.
write_obj.

write_obj1([]):-!.
write_obj1([HObj|Rest]):-
     write(HObj),
     write(' '),
     write_obj1(Rest),!.

write_init(Ini):-
     write('  \(:init '),
     atomic_invariants(Atom),
     write_preds(Atom),
     write_ss(Ini),
     write('\)'),nl,nl,!.

write_goal([se(_,_,Pred)]):-
     length(Pred,Len),
     Len = 1,
     write('  \(:goal '),nl,
     write_preds(Pred),
     write('\)'),nl,nl,!.
write_goal(Goal):-
     write('  \(:goal '),nl,
     write('\(and '),
     write_se(Goal),
     write('\)\)'),nl,nl,!.

write_preds([]):-!.
write_preds([HPred|Rest]):-
      write('\('),
      HPred=..[Name|Varls],
      write(Name),write(' '),
      write_preds1(Varls),
      write('\)'),nl,
      write_preds(Rest),!.

write_preds1([H|[]]):-
      write(H),!.
write_preds1([H|T]):-
      write(H), write(' '),
      write_preds1(T),!.

write_ss([]):-!.
write_ss([ss(_,_,Preds)|Ini]):-
      write_preds(Preds),
      write_ss(Ini),!.

write_se([]):-!.
write_se([se(_,_,Preds)|Goal]):-
      write_preds(Preds),
      write_se(Goal),!.

