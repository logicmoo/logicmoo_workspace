
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%memory.pl

%11.1.2 State accumulation using engines - https://www.swi-prolog.org/pldoc/man?section=engine-state
:- use_module(library(heaps)).

create_heap(E) :- empty_heap(H), engine_create(_, update_heap(H), E).
update_heap(H) :- engine_fetch(Command), ( update_heap(Command, Reply, H, H1) ->  true; H1 = H, Reply = false ), engine_yield(Reply), update_heap(H1).

update_heap(add(Priority, Key), true, H0, H) :- add_to_heap(H0, Priority, Key, H).
update_heap(get(Priority, Key), Priority-Key, H0, H) :- get_from_heap(H0, Priority, Key, H).

heap_add(Priority, Key, E) :- engine_post(E, add(Priority, Key), true).

heap_get(Priority, Key, E) :- engine_post(E, get(Priority, Key), Priority-Key).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%control.pl

priority([_, [F,C]], P) :- opencog_z_f_exp([F, C], E), P is E.

input_event(Event) :- heap_add(1.0, Event, belief_events_queue).

derive_event(Event) :- priority(Event, P), heap_add(P, Event, belief_events_queue).

inference_step(_) :- (heap_get(_Priority, Event, belief_events_queue),
                      heap_get(Priority2, Event2, belief_events_queue),
                      heap_add(Priority2, Event2, belief_events_queue), %undo removal of the second premise (TODO)
                      inference(Event,Event2,Conclusion), 
                      derive_event(Conclusion),
                      write(Conclusion), nl
                     ; true ).

opencog_main :- create_heap(belief_events_queue), opencog_main(1).
opencog_main(T) :- read_atomese(X),
     (X = 1, write("performing 1 inference steps:"), nl, 
     inference_step(T), write("done with 1 additioatomspace inference steps."), nl,  
       opencog_main(T+1) ; 
       X \= 1, write("Input: "), write(X), nl, input_event(X), opencog_main(T+1)).

:- if( prolog_load_context(reload,false)).
:- create_heap(belief_events_queue).
:- endif.

%test:
%opencog_main.
%[inheritance(cat,animal), [1.0, 0.9]].
%[inheritance(animal,being), [1.0, 0.9]].
%1.
%output:
%performing 1 inference steps:
%[inheritance(cat,being),[1.0,0.81]]
%done with 1 additioatomspace inference steps.


:-  fixup_exports.



