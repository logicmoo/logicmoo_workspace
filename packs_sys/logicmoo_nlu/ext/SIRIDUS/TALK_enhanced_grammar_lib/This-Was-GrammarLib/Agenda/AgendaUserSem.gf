--# -path=.:../Common:prelude

concrete AgendaUserSem of AgendaUser = GodisUserSem, BookingSem ** 
    open Prolog, AgendaSystemSem in {

lin 

-- short answers

event  x = pm1 (shortAns (event x));
time x = pm1 (shortAns (time x));
date x = pm1 (shortAns (date x));
event_time e t = pm2 (shortAns (event e)) (shortAns (time t));


-- predicates  


-- U: How is the schedule for monday?
how_is_schedule d = pm2 (ask (pWhQ "bookings")) (answer (date d));

-- U: When is eventon date?
when_is_event_day e d = pm3 (ask (pWhQ "start_time_to_store")) (answer (event e)) (answer (date d));

-- U: What is the date today?
what_is_the_date = pm1 (ask (pWhQ "todaysdate"));


-- U: Am I booked Date Time?
am_I_booked d t = pm3 (ask (pWhQ "event_to_store")) (answer (date d)) (answer (time t));


-- U: What day is the Event?
what_day_is_event e = pm2 (ask (pWhQ "date")) (answer (event e));


-- U: What day is the Event at Time?
what_day_is_event_at_time e t = pm3 (ask (pWhQ "date")) (answer (event e)) (answer (time t));



-- action add

add = pm1 (request agenda_add);



add__event e = pm2 (request agenda_add) (answer ( event e ));

add__event_date e d = pm3 (request agenda_add) (answer (event e)) (answer (date d));
add__event_time e t = pm3 (request agenda_add) (answer (event e)) (answer (time t));
add__event_time_date e t d = pm4 (request agenda_add) (answer (event e)) (answer (time t)) (answer (date d));


-- action delete

delete = pm1 (request agenda_delete);


-- action move

move_booking = pm1 (request agenda_move);


-- action change_time

change_time = pm1 (request agenda_change_time);


-- action change_date

change_date = pm1 (request agenda_change_date);


}

