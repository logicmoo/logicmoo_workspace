:- use_module(library('linda/client'),
    [	linda_client/1,
	close_client/0,
	in/1,
	%in/2, Excluded because it clashes with clpfd
	in_noblock/1,
	out/1,
	rd/1,
	rd/2,
	rd_noblock/1,
        bagof_rd_noblock/3,
	linda_timeout/2,
	shutdown_server/0]).
:- use_module(library(system)).
:- multifile hap/2.
:- dynamic hap/2.

:- dynamic timestamp/1.
timestamp(0).

history_dyn(T,Tnew,Closed,Blocking):-
    hap(Event,Time),
    Time > T, !,
    write_debug('Cached event: '), writeln_debug(h(Event,Time)),
    h(Event,Time),
    history_dyn(Time,Tnew,Closed,Blocking).
history_dyn(T,T,Closed,Blocking):-
    %get_new_events(Blocking,Closed).
    get_single_event(Blocking,Closed,_,Event),
    (Event = h(_,Time)
      ->    history_dyn_directional(Time,_Tnew,Closed,Blocking)
      ;     % either history is closed -> close_history has already been 
            % executed (nothing to do)
            % or there is no new event: succeed
            true
    ).

% get_new_events(+Blocking,-Closed)
get_new_events(Blocking,Closed):-
    get_new_events(Blocking,Closed,_).
get_new_events(Blocking,Closed,Pattern):-
    copy_term(Pattern,E),
    in_noblock(h(E,T)),!,
    assertz(hap(E,T)),
    write_debug('Received event: '), writeln_debug(h(E,T)),
    h(E,T),
    
/*    Lui mi ha detto 3 eventi H1, H2 e H3
    Il primo e` in un H1 -> E6 \/ E7; scelgo E6.
    Il secondo mi fa fallire (H2 -> EN...)
    allora faccio backtracking e prendo E7. 
    A questo punto, la get_new_events successiva va a leggere da Linda
    e non dalla cache;
    l'evento 2 non c'e` piu` e non lo inserisco.
    Quindi, in pratica, l'evento che
    mi ha fatto fallire io l'ho ignorato.
    Questo e` proprio quello che succede allo scientist quando il dept
    gli dice EN(pay(400))
    Quindi la get_new_events non dovrebbe richiamarsi ricorsivamente, ma andare
    a vedere prima se c'e` qualche evento in cache che non ho considerato
*/  
    get_new_events(Blocking,Closed,Pattern).
get_new_events(_,Closed,_):-
    rd_noblock(close_history),!, Closed=closed,
    close_history.
get_new_events(blocking,Closed,Pattern):-
    rd([close_history,h(_,_)],_),
    get_new_events(blocking,Closed,Pattern).
get_new_events(nonblocking,_Closed,_).

% Acquires just one event.
get_single_event(_Blocking,_Closed,Pattern,h(E,T)):-
    copy_term(Pattern,E),
    in_noblock(h(E,T)),!,
    assertz(hap(E,T)),
    write_debug('Received event: '), writeln_debug(h(E,T)),
    h(E,T).
get_single_event(_,Closed,_,Event):-
    rd_noblock(close_history),!, Closed=closed, Event=closed,
    close_history.
get_single_event(nonblocking,_,_,none).

out_event(Event,Time):-
    (var(Time)
      ->    compute_time(NewTime), Time=NewTime
            % Facendo direttamente compute_time(Time) in certi casi da` errore, quando Time e` una variabile constrained
            %now(Time)  % Get System time
      ;     true),
    out(h(Event,Time)),
    write_debug('Output event: '),
    writeln_debug(h(Event,Time)).

out_event_once(Event,_):- hap(Event1,_), variant(Event,Event1), !.
out_event_once(Event,Time):-
    (var(Time)
      ->    compute_time(NewTime), Time=NewTime
      ;     true),
    out(h(Event,Time)),
    write_debug('Output event: '),
    writeln_debug(h(Event,Time)).

%run_dyn(+Blocking).
% Blocking can be either 'blocking' or 'nonblocking'
run_dyn(Blocking):-
    load_ics,
    current_time(0),
    society_goal,
    history_dyn(-inf,TLast,Closed,nonblocking),
    % here you can invoke, if needed, other predicates that continue
    % the elaboration, like ground_time, make_choice, etc.
    (nonvar(Closed)    % last check
      ->    true;
        history_dyn(TLast,_,_,Blocking)
    ).

run_dyn_argument(Blocking):-
    load_ics,
    current_time(0),
    society_goal,
    history_dyn(-inf,_TLast,_Closed,Blocking), % qua c'era un commento ...
    once(abd(finished_reasoning,1)),
    no_more_messages.

no_more_messages:-
    rd(_), fail.
no_more_messages:-
    rd_noblock(leave_dialogue).

%%%%%%%%%%%%%%%%%%%%%%%%%% PROCESS COMMUNICATION %%%%%%%%%%%%%%%%%%%%%%%%%%%
% So far, there is no concept of sending a message: everyone can pick a
% message from the board, ad the others will not see it.
% Let's insert some directionality. Events are of the form
%   h(tell(Sender,Receiver,Content),Time)
% Only the correct receiver can remove the message from the blackboard.

run_dyn_argument_directional(Blocking):-
    load_ics,
    init_linda_time(IniTime),
    current_time(IniTime),
    society_goal,
    history_dyn_directional(-inf,_TLast,_Closed,Blocking), % qua c'era un commento ...
    once(abd(finished_reasoning,1)),
    % In realta` e` un po' semplicistico: qualunque cosa l'altro mi dica io
    % fallisco e faccio backtracking su society_goal!
    % Forse dovrei inserire un punto di scelta in history_dyn_directional:
    % li` io ipotizzavo che non arrivassero nuovi eventi, per cui se ne arrivano
    % devo fare backtracking.
    no_more_incoming_messages.

run_dyn_argument_directional_closed(Blocking):-
    load_ics,
    init_linda_time(IniTime),
    current_time(IniTime),
    society_goal,
    history_dyn_directional(-inf,_TLast,_Closed,Blocking), % qua c'era un commento ...
    close_history,
    once(abd(finished_reasoning,1)),
    % In realta` e` un po' semplicistico: qualunque cosa l'altro mi dica io
    % fallisco e faccio backtracking su society_goal!
    % Forse dovrei inserire un punto di scelta in history_dyn_directional:
    % li` io ipotizzavo che non arrivassero nuovi eventi, per cui se ne arrivano
    % devo fare backtracking.
    no_more_incoming_messages.


send_message(Sender,Receiver,Content,Time):-
    % Send the message
    out_event_once(tell(Sender,Receiver,Content),Time), % QUI NON SI POTREBBE METTERE UNA OUT_EVENT_ONCE??????
    % Save the event in the local cache
    % La out_event_once da Time=var se l'evento c'era gia`. In tal caso,
    % non lo si salva in cache
    (var(Time)
        -> true
        ;  assertz(hap(tell(Sender,Receiver,Content),Time))).

%Se inserendo un evento con get_new_events c'e` un fallimento, lui non re-inserisce
%in backtracking l'evento.


% Assumes that in the SOKB there exists a predicate me/1, that tells the unique
% name of the agent
history_dyn_directional(T,Tnew,Closed,Blocking):-
    hap(Event,Time),
    Time > T, !,
    write_debug('Cached event: '), writeln_debug(h(Event,Time)),
    h(Event,Time),
    history_dyn_directional(Time,Tnew,Closed,Blocking).
history_dyn_directional(_T,Tnew,Closed,Blocking):-
    me(ME),
    get_single_event(Blocking,Closed,tell(_,ME,_),Event),
    (Event = h(_,Time)
      ->    history_dyn_directional(Time,Tnew,Closed,Blocking)
      ;     % either history is closed -> close_history has already been 
            % executed (nothing to do)
            % or there is no new event: succeed
            true
    ).
    
% backtracking: New events have arrived: let us process them!
%history_dyn_directional(T,Tnew,Closed,Blocking):-  Questo non considera quello che ho detto io, per cui ripeto sempre le stesse cose!
%    me(ME),
%    rd(h(tell(_,ME,_),_)), !, 
%    get_new_events(Blocking,Closed,tell(_,ME,_)).
history_dyn_directional(T,_Tnew,_Closed,_Blocking):-
    % No new cached events (i.e., events generated by me)
    \+(( hap(_,Time),
        Time > T
    )),
    % No new messages to me
    \+(( me(ME),
        rd_noblock(h(tell(_,ME,_),_))
    )),!,
    % Then, it is a failure due to the proof: fail.
    fail.
% Otherwise, there must be some news: let us process them
history_dyn_directional(T,Tnew,Closed,Blocking):-
    history_dyn_directional(T,Tnew,Closed,Blocking).
%history_dyn_directional(T,Tnew,Closed,Blocking):-
%    me(ME),
%    rd(h(tell(_,ME,_),_)), !, 
%    get_new_events(Blocking,Closed,tell(_,ME,_)).


% backtracking, no new events. The reason of failure must be something else,
% thus we simply backtrack further.
% history_dyn_directional(T,Tnew,Closed,Blocking):- fail.

%no_more_incoming_messages:-
%    rd_noblock(leave_dialogue).
no_more_incoming_messages:-
    me(ME),
    rd([leave_dialogue,h(tell(_,ME,_),_)],X),
    (X = leave_dialogue
    -> true
    ;  fail
    ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   TIME
% The first agent sets the time to 0, the others accept the current time
init_linda_time(N):-
    rd_noblock(linda_time(N)),!.
init_linda_time(0):-
    out(linda_time(0)).

compute_time(Time):-
    in(linda_time(Time)),
    NewTime is Time+1,
    out(linda_time(NewTime)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Per velocizzarmi nel far partire i vari processi ...

connect:-
    ['../linda_server/hostname.pl'],
    hostname(HN),
    linda_client(HN).
%:-compile(sciff).
