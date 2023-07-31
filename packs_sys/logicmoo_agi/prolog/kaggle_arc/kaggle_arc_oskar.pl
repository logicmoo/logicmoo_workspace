




call_w_time_limit(Time, Goal) :-
    call_w_time_limit(Time, Goal, '$no_ctx').

    %call_w_time_limit(6, current_alarm(Time, Goal, Id, Status)).

call_w_time_limit(Time, Goal, Ctx) :-
  %current_alarm(Time, Goal, Id, Status)->uninstall_alarm(Id)
  setup_call_cleanup(alarm(Time, maybe_yield(Time,Id,Goal,overtime), Id, [install(false),remove(false)]),
        run_alarm_goal(Time, Id, Goal), alarm_unneeded(Id)).

alarm_unneeded(Id):- current_alarm(Time, Goal, Id, Status), uninstall_alarm(Id).
alarm_unneeded(_).
maybe_yield(Time,Id,Goal,overtime):- engine_self(_),engine_yield(overtime(Time,Id,Goal)).
maybe_yield(_,_,_,_).

run_alarm_goal(Time, Id, Goal) :- install_alarm( Id, Time), Goal,!,alarm_unneeded(Id).






%change_of_position
