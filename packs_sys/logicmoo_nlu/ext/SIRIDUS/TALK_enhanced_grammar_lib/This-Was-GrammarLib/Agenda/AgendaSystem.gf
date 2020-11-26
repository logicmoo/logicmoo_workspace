--# -path=.:../Common:prelude

abstract AgendaSystem = GodisSystem, Booking ** {

fun

-----------------------------------------------------
-- Predicates
-- Questions and Propositions as they are intended to be used 
-- by either System or User


-- S: What date?
date_to_add_Q : Question;
-- U: DATE
date_to_add_P : Date -> Proposition;


-- S: What time?
time_to_add_Q : Question;
-- U: TIME
time_to_add_P : Time -> Proposition;

-- S: What type of booking?
event_to_add_Q : Question;
-- U: EVENT
event_to_add_P : Event -> Proposition;

{-
-- U: When is the EVENT?
time_for_event_Q : Question;
-- S: DATE
time_for_event_P : Date -> Preposition;


-- U: Do I have anything booked DATE?
booking_on_date_Q : Question;
-- ??? S: you have nothing/EVENT,TIME booked
booking_on_date_P : Preposition;
-}


------------------------------------------------------

-- short anwers

event : Event -> ShortAns;
time : Time -> ShortAns;
date : Date -> ShortAns;


------------------------------------------------------


-- actions
agenda_add : Action;

agenda_delete : Action;

agenda_move : Action;

agenda_change_time : Action;
agenda_change_date : Action;
agenda_change_time__booking : Action;
agenda_change_date__booking : Action;

------------------------------------------------------

}
