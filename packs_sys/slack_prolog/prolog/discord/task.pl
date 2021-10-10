:- if(current_prolog_flag(xref,true);(prolog_load_context(file,F),prolog_load_context(source,F))).
:- module(discord_task,[]).
:- endif.

% ====================================
% Discord Task Calls
% ====================================

is_thread_running(ID):-
  is_thread(ID), thread_property(ID,status(What)),!,
   (What==running->true;(thread_join(ID,_ ),!,fail)).

:- volatile(tmp:doing_task/1).
:- dynamic(tmp:doing_task/1).
:- volatile(tmp:last_done/2).
:- dynamic(tmp:last_done/2).

check_since(never,_Task):-!.
check_since(_Often,Task):- with_mutex(last_done_mutex, (\+ tmp:last_done(Task,_),remember_task(Task))),!,do_task(Task).
check_since(once,_Task):-!.
check_since(Often,Task):- with_mutex(last_done_mutex, ((tmp:last_done(Task,When), get_time(Now), When+Often < Now, remember_task(Task)))),do_task(Task).

do_task(Task):- tmp:doing_task(Task),!.
do_task(Task):-
  setup_call_cleanup(asserta(tmp:doing_task(Task),R),
     setup_call_cleanup(
         remember_task(Task), ignore(call(Task)), remember_task(Task)),erase(R)).

how_long_ago(Task,Ago):- nonvar(Ago),!,how_long_ago(Task,TryAgo),!,Ago=TryAgo.
how_long_ago(Task,Ago):- 
  with_mutex(last_done_mutex,tmp:last_done(Task,Time);Ago=never),!,
  ignore((number(Time),get_time(Now),Ago is Now - Time)),!.

remember_task(Task):- with_mutex(last_done_mutex, 
  ignore((retractall(tmp:last_done(Task,_)),get_time(Time),asserta(tmp:last_done(Task,Time))))).

%how_often(200, guilds).
%how_often(30, channels).

%how_often(5, handle_discord_websocket_events).
%how_often(41, send_ping).
how_often(2, handle_chat_events).
how_often(once, discord_update(channels)).
how_often(300, rtrv_new_messages).
/*
how_often(500, rtrv_dm_handles).
*/
%how_often(600, show_discord_tasks).

% ===============================================
% Thread Pool Models
% ===============================================

ensure_thread_model2(M,Already):-
 \+ thread_self(M),!, (is_thread_running(M) -> call(Already) ; show_call(thread_create(M,_,[alias(M),detached(true)]))).
ensure_thread_model(M,Pause,G):- ensure_thread_model2(M,G) -> true ; (call(Pause),call(G),call(M)),!.
%ensure_thread_model(M,Pause,G):- 

ping_discord:- disable_gateway,!.
ping_discord:- ensure_thread_model(ping_discord,ping_sleep,do_task(send_ping)).

send_ping:- remember_task(send_ping),discord_send({"op": 1, "d": $s}).
ping_sleep:- discord_ddd(heartbeat_interval,hasValue,Time)->sleep(Time);sleep(30).


%deque_discord_events:- disable_gateway,!.
deque_discord_events:- ensure_thread_model(deque_discord_events,sleep(1),do_task(handle_discord_websocket_events)).

discord_proc_tasks:- ensure_thread_model(discord_proc_tasks, sleep(1), check_4_tasks).

check_4_tasks:- forall(how_often(Often,Task),ignore(check_since(Often,Task))),fail.
check_4_tasks.

discord_message_checking_01:- disable_gateway,!.
discord_message_checking_01:-  
  ensure_thread_model(discord_message_checking_01,
  sleep(1), check_4_tasks).

discord_message_checking_02:-  
  ensure_thread_model(discord_message_checking_02,
  sleep(1), check_4_tasks).


%how_often(300, rtrv_messages_chan).
%how_often(10, discord_message_checking_01).
%how_often(10, discord_message_checking_02).
:- meta_predicate(discord_show_list(?)).
discord_show_list(TmpR):- forall(tmp:TmpR,format(' ~q.~n',[TmpR])).

show_discord_tasks:- 
 remember_task(show_discord_tasks),
 discord_show_list(discord_websocket_event(_,_)),
 discord_show_list(discord_chat_event(_,_)),
 findall(Task,tmp:doing_task(Task),TaskList),ttyflush,format('~N~n%~~ (secs)\tdoing: ~q~n',[TaskList]),
 findall(Task,tmp:last_done(Task,_);tmp:doing_task(Task);how_often(_,Task),TaskL),list_to_set(TaskL,TaskS),
 forall(member(Task,TaskS),ignore(show_discord_task(Task))).

show_discord_task(Task):- tmp:last_done(Task,When),!,show_discord_task(Task,When).
show_discord_task(Task):- show_discord_task(Task,never).
show_discord_task(Task,When):-  ignore(how_often(Often,Task)),ignore(Often=whenever), 
 ignore((number(When), get_time(Now), Ago is integer(Now-When))), ignore(Ago=When),
 format('~N%~~ ~w  \t~q.',[Ago/Often,Task]).


handle_discord_websocket_events:- remember_task(handle_discord_websocket_events),  
  handle_discord_websocket_event,fail.
handle_discord_websocket_events:- 
  handle_chat_events,fail.
handle_discord_websocket_events.

handle_discord_websocket_event:-
  retract(tmp:discord_websocket_event(Type,Message))*->ignore(discord_websocket_hook_1(Type,Message));true.

handle_chat_events:- 
 handle_chat_event,fail.
handle_chat_events:- remember_task(handle_chat_events).

handle_chat_event:- 
  (retract(tmp:discord_chat_event(Type,Message))*->ignore(handle_discord_chat_event(Type,Message));true).

